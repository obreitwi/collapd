{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Collapd.Conf
  ( getConf
  , Conf(..)
  , BackendEntry(..)
  , WithConf
  , WithConfOnly
) where

import           Control.Applicative (liftA)
import           Control.Monad (filterM, liftM, join, (=<<))
import           Control.Monad.Reader (ReaderT)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as B
import           Data.Functor ((<&>))
import           Data.Functor.Identity
import           Data.List (intercalate)
import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.YAML
import           Ldap.Client as Ldap
import           System.Directory (doesFileExist)
import           System.Environment
import           System.Exit (die)

import           Collapd.Filter

deriving instance Read Ldap.Attr
deriving instance Read Ldap.Filter

data Conf = Conf
  { attributesForward :: [Attr]   -- attributes to forward
  , attributesAppend :: AttrList []   -- attributes to append to each entry
  , collapsedBase :: Dn     -- the DN as wich all entries appear
  , backends :: [BackendEntry] -- all hosts that are collapsed into one
} deriving (Show)

data BackendEntry = BackendEntry
  { host :: T.Text
  , port :: PortNumber
  , base :: Dn
  , filter :: Filter       -- the filter with which to search entries
} deriving (Show)

newtype YamlAttrList = YamlAttrList { unYamlAttrList :: (AttrList []) }
  deriving (Show)

instance FromYAML Conf where
  parseYAML = withMap "Conf" $ \m -> Conf
      <$> m .: "attributes_forward"
      <*> (unYamlAttrList <$> m .: "attributes_append")
      <*> m .: "collapsed_base"
      <*> m .: "backends"

instance FromYAML BackendEntry where
  parseYAML = withMap "BackendEntry" $ \m -> BackendEntry
    <$> m .: "host"
    <*> m .:? "port" .!= 389
    <*> m .: "base"
    <*> getFilter m .!= (Attr "objectClass" := "posixAccount")

getFilter :: Mapping -> Parser (Maybe Filter)
getFilter m = do
  entry <- m .:? "filter"
  return . join $ (toFilter . encodeUtf8) <$> entry

instance FromYAML Dn where
  parseYAML = withStr "Dn" $ return . Dn

instance FromYAML PortNumber where
  parseYAML = withInt "Port" $ return . fromIntegral

instance FromYAML Attr where
  parseYAML = withStr "Attr" $ return . Attr

instance FromYAML YamlAttrList where
  parseYAML = withSeq "outter list for attributes" $ 
      liftM (YamlAttrList . concat) . mapM convertDict
    where
      convertDict :: Node -> Parser [(Attr, [AttrValue])]
      convertDict = withMap "single dict for attributes" $
        mapM (toAttr) . Map.toList

      toAttr :: (Node, Node) -> Parser (Attr, [AttrValue]) 
      toAttr (Scalar k, Scalar v) = return $ (attr k, [value v])
        where
          attr = Attr . T.pack . scalarToStr
          value = B.pack . scalarToStr
      toAttr _ = fail "Attribute lists only support scalars as key/values."

-- TODO convert to Text
scalarToStr :: Scalar -> String
scalarToStr (SNull)    = ""
scalarToStr (SBool b)  = show b
scalarToStr (SFloat d) = show d
scalarToStr (SInt i)   = show i
scalarToStr (SStr t)   = T.unpack t

-- return function that appends given string to argument
append :: String -> String -> String
append = flip (++)

confHome :: IO (Maybe String)
confHome = lookupEnv "HOME" <&> fmap (append "/.config/collapd.yaml")

confEnv :: IO (Maybe String)
confEnv = lookupEnv "COLLAPD_CONF"

confSys :: IO (Maybe String)
confSys = return . Just $ "/etc/collapd.yaml"

confFileOrder :: [IO (Maybe String)]
confFileOrder = [confEnv, confHome, confSys]

-- Get all possible locations for the configuration file
confFileLocations :: IO [String]
confFileLocations = sequence confFileOrder <&> catMaybes

-- Return valid (=existing) configuration path or exit.
confPath :: IO FilePath
confPath = do
  locations <- confFileLocations
  confFiles <- filterM doesFileExist locations
  case confFiles of
    confFile:_ -> return confFile
    _ -> die $ "No valid configuration files found. "
            ++ "Please specify via --config or by setting COLLAPD_CONF. "
            ++ "\n\nSearched: "
            ++ intercalate ", " locations

-- Get the configuration file.
-- We stop after the first file was found.
-- Override with option
getConf :: Maybe FilePath -> IO Conf
getConf Nothing = confPath >>= readConf
getConf (Just path) = readConf path

-- read the actual configuration or fail with an error
readConf :: FilePath -> IO Conf
readConf path = do
  content <- B.readFile path
  let decoded = decodeStrict content
  case decoded of
    Left e -> error $ "Error reading configuration: " ++ show e
    Right [conf] -> return conf
    _ -> error $ "Found multiple configurations in file: " ++ path

type WithConf m = ReaderT Conf m
type WithConfOnly = ReaderT Conf Identity
