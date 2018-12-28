{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Monad (mapM_)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Functor ((<&>))
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as T
import           Data.Version (showVersion)
import           Development.GitRev (gitHash)
import qualified Ldap.Client as Ldap
import           Ldap.Client hiding (scope)
import           Options.Applicative
import           System.Environment (getArgs)
import           System.Exit (die, exitFailure, exitSuccess)
import           Text.RawString.QQ

import           Paths_collapd (version)

import qualified Debug.Trace as DT

import Collapd.Bind
import Collapd.Conf
import Collapd.Filter
import Collapd.List
import Collapd.InOut
import Collapd.Utils

-- default mode is to read from StdIn
data Mode = List ListOptions | Bind BindOptions
  deriving (Show)

data Options = Options
  { configPath :: Maybe FilePath
  , mode :: Maybe Mode
  }
  deriving (Show)

data BindOptions = BindOptions
  { bindDn :: T.Text
  , password :: Maybe T.Text
  , repeated :: Bool
  }
  deriving (Show)

data ListOptions = ListOptions
  { listFilter :: Maybe B.ByteString
  , scope :: Maybe B.ByteString
  , returnOriginal :: Bool
  , noUnique :: Bool
  , attributes :: [T.Text]
  }
  deriving (Show)

getBindOptions :: ParserInfo Mode
getBindOptions = info
  (Bind <$> (BindOptions
            <$> (strOption $
                 long "dn" <> help "DN with which to bind." <> metavar "DN")
            <*> (optional . strOption)
                (long "password"
                <> short 'p'
                <> help ("Password with which to bind. " ++
                   "If not supplied it will be asked for/read from StdIn.")
                <> metavar "PW"
                )
            <*> (switch $ long "repeat" <> short 'r'
                <> help ("Upon failed login attempt, " ++ 
                         "keep asking for new passwords.")
                )
            )
  )
  (progDesc "Bind with given DN.")

getListOptions :: ParserInfo Mode
getListOptions = info
  (List <$> (ListOptions
            <$> (optional . strOption)
                (long "filter" <> short 'f' <>
                 help "LDAP filter to apply to listed data.")
            <*> (optional . strOption)
                (long "scope" <> short 's' <>
                 help "Scope for the ldap request (base|one|sub).")
            <*> switch
                (long "original" <>
                 help "Return original entries.")
            <*> switch
                (long "non-unique" <>
                 help "Do not make entries unique.")
            <*> (many . strOption)
                (long "attribute"
                <> short 'a'
                <> help "Attributes to fetch."
                <> metavar "ATTR"
                )
            )
  )
  (progDesc "List all available entries")

getOptions :: Parser Options
getOptions = Options
  <$> (optional $ strOption $ long "config" <> short 'c'
      <> help ("Explicit path to config yaml-file. " ++
               "Alternatively set COLLAPD_CONF.")
      <> metavar "PATH")
  <*> optional (hsubparser
    (  command "list" getListOptions
    <> command "bind" getBindOptions
    ))

getOpts :: ParserInfo Options
getOpts = info (helper <*> versionOption <*> getOptions)
    ( fullDesc
    <> header "collapd - collapse several LDAP backends into a single tree"
    <> progDesc getProgDesc
    <> footer "Run -h for each command to see additional command line\
             \ options.")
  where
    versionOption = infoOption
      (concat [(showVersion version), " ", $(gitHash)])
      (short 'V' <> long "version" <> help "Show version")

getProgDesc :: String
getProgDesc = [r|
Without any command given, run in Shell-backend mode. That is, read the given
LDAP request from stdin and write the response to stdout.
|]

run :: Maybe FilePath -> Maybe Mode -> IO ()
run confPath (Just (List opts)) = getConf confPath >>= runList opts
run confPath (Just (Bind opts)) = getConf confPath >>= runBind opts
run conf Nothing = getConf conf >>= runStdIn

runBind :: BindOptions -> Conf -> IO ()
runBind opts conf = do
    cEntry <- listAllHosts conf >>= return . getEntry (Dn . bindDn $ opts)
            >>= checkEntry
    password <- checkPw $ password opts
    authSuccess <- bindEntry cEntry password
    if authSuccess then
      T.putStrLn "Success" >> exitSuccess
    else 
      T.putStrLn "Failure" >> continue
  where
    continue :: IO ()
    continue | repeated opts = runBind opts conf
             | otherwise = exitFailure

    checkEntry :: Maybe CollapsedEntry -> IO CollapsedEntry
    checkEntry Nothing = die $ "Error: Could not find specified entry "
                             ++ (T.unpack . bindDn $ opts)
    checkEntry (Just ce) = return ce

    checkPw (Nothing) = getPassword (Dn . bindDn $ opts) >>= return . encodePw
    checkPw (Just pw) = return . encodePw $ pw

    encodePw :: T.Text -> Password
    encodePw = Password . encodeUtf8

runList :: ListOptions -> Conf -> IO ()
runList opts conf = do
      scope' <- (readScope $ scope opts)
      filter' <- readFilter $ listFilter opts
      let attrs' = (fmap Attr) $ attributes opts
      listHostsWith filter' scope' attrs' conf
      >>= (uniq (not $ noUnique opts)
        <&> formatEntriesWith (getter $ returnOriginal opts)
        <&> return)
      >>= BL.putStrLn
    where
      -- decide whether or not we make the list uniq
      uniq :: Bool
           -> [CollapsedEntry] -> [CollapsedEntry]
      uniq False = id
      uniq True = makeUnique

      -- decide which part of the data to get
      getter :: Bool
             -> CollapsedEntry -> SearchEntry
      getter True = original
      getter False = collapsed

      readFilter :: Maybe B.ByteString -> IO (Maybe Filter)
      -- if nothing was supplied just pass
      readFilter Nothing = return Nothing
      readFilter (Just f) = case toFilter f of
        -- notify user we could not parse the supplied filter
        Nothing -> die "Error: Could parse specified filter."
        parsed -> return parsed

      readScope :: Maybe B.ByteString -> IO (Maybe (Ldap.Mod Search))
      readScope (Just "base") = scopeToMod BaseObject
      readScope (Just "one" ) = scopeToMod SingleLevel
      readScope (Just "sub" ) = scopeToMod WholeSubtree
      readScope Nothing       = return Nothing
      readScope _             = die "Error: Invalid scope specified.\
                                   \ Should be base/one/sub."

      scopeToMod :: Scope -> IO (Maybe (Ldap.Mod Search))
      scopeToMod = return . Just . Ldap.scope

runStdIn :: Conf -> IO ()
runStdIn conf = do
  stdin <- BL.getContents
  let request = readRequest . BL.toStrict $ stdin `BL.snoc` '\n'
  case request of
    Left e -> die $ "Could not read request: " ++ e
    Right req -> handleRequest conf req

main :: IO ()
main = do
  opts <- execParser getOpts
  run (configPath opts) (mode opts)
