{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Collapd.InOut
  ( Request
  , RequestType(..)
  , TextLdapAttrList
  , TextLdapEntry
  , checkFail
  , formatEntries
  , formatEntriesOriginal
  , formatEntriesWith
  , getAttr
  , handleRequest
  , putStrDebug
  , readRequest
  ) where

-- handle LDIF text mangling for pretty output
-- decode input messages

import           Prelude hiding (filter)

import           Control.Applicative ((<|>))
import           Control.Monad (join)
import           Control.Monad.Trans.Writer (tell)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Functor ((<&>))
import           Data.List.NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Ldap.Client as LC
import qualified Ldap.Client.Internal as LC
import           System.Exit (die, exitFailure, exitSuccess, ExitCode(..), exitWith)
import qualified Text.LDAP.Data as TLD
import qualified Text.LDAP.Parser as TLI
import qualified Text.LDAP.Printer as TLO

import qualified Debug.Trace as DT

import           Collapd.Bind
import           Collapd.Conf
import           Collapd.Filter
import           Collapd.List
import           Collapd.Utils

data RequestType = ReqBind
                 | ReqUnbind
                 | ReqSearch
                 | ReqCompare
                 | ReqModify
                 | ReqModifyRDN
                 | ReqAdd
                 | ReqDelete
                 | ReqAbandon
  deriving Show

type Request = (RequestType, TextLdapAttrList)
type TextLdapEntry = (TLD.DN, [TLD.Attribute])
type TextLdapAttrList = [TLD.Attribute]

data Response = Response TextLdapAttrList

formatEntries :: [CollapsedEntry] -> BL.ByteString
formatEntries = formatEntriesWith collapsed

formatEntriesOriginal :: [CollapsedEntry] -> BL.ByteString
formatEntriesOriginal = formatEntriesWith original

formatEntriesWith :: (CollapsedEntry -> LC.SearchEntry)
                  -> [CollapsedEntry]
                  -> BL.ByteString
formatEntriesWith getter entries = TLO.runLdapPrinter
  (TLO.openLdapData TLO.ldifEncodeAttrValue)
  $ fmap (convertEntry . getter) entries

-- check if computation failed
checkFail :: forall a. T.Text -> Maybe a -> IO a
checkFail _ (Just x) = return x
checkFail t Nothing = putStrDebug t >> exitFailure

-- convert between Ldap.Client and Text.Ldap
convertEntry :: LC.SearchEntry -> TextLdapEntry
convertEntry (LC.SearchEntry (LC.Dn fromDn) fromAttrs) = (toDn, toAttrs)
  where
    toAttrs = join $ fmap convAttribute fromAttrs

    toDn = case TLI.runLdapParser TLI.dn . BL.fromStrict . T.encodeUtf8
        $ fromDn of
      Left e -> error $ "Could not convert DN, this should never happen: "
                        ++ e
      Right d -> d

-- Ldap.Client encode several attributes of the same type as (key, [value]),
-- whereas Text.Ldap prefers [(key, value)] -> we need to mitigate between the
-- two
convAttribute :: (LC.Attr, [LC.AttrValue]) -> [TLD.Attribute]
convAttribute (fromAttr, fromAttrValue) = fmap cloneAttr fromAttrValue
  where
    newAttribute = TLD.AttrType . T.encodeUtf8 $ LC.unAttr fromAttr

    cloneAttr value = (newAttribute, TLD.AttrValue value)

requestType :: AP.Parser RequestType
requestType
  = ( ("BIND"    >> return ReqBind)
  <|> ("UNBIND"  >> return ReqUnbind)
  <|> ("SEARCH"  >> return ReqSearch)
  <|> ("COMPARE" >> return ReqCompare)
  <|> ("MODIFY"  >> return ReqModify)
  <|> ("MODRDN"  >> return ReqModifyRDN)
  <|> ("ADD"     >> return ReqAdd)
  <|> ("DELETE"  >> return ReqDelete)
  <|> ("ABANDON" >> return ReqAbandon)
  ) <* AP.endOfLine

request :: AP.Parser (RequestType, TextLdapAttrList)
request = (,)
  <$> requestType
  <*> AP.many' (TLI.ldifAttr TLI.ldifDecodeAttrValue <* AP.endOfLine)

readRequest :: B.ByteString -> Either String Request
readRequest = AP.eitherResult . AP.parse request

-- exposed function that does not directly use Text.LDAP.Data types
getAttr :: B.ByteString -> Request -> Maybe B.ByteString
getAttr attrType (_, attrList) =
    conv <$> getAttr' (TLD.AttrType attrType) attrList
  where
    conv :: TLD.AttrValue -> B.ByteString
    conv (TLD.AttrValue value) = value

getAttr' :: TLD.AttrType -> TextLdapAttrList -> Maybe TLD.AttrValue
getAttr' _ [] = Nothing
getAttr' attr ((key, val):rest)
  | attr == key = Just val
  | otherwise = getAttr' attr rest

handleRequest :: Conf -> Request -> IO ()
handleRequest conf (ReqUnbind, _) = exitSuccess
handleRequest conf (ReqAbandon, _) = exitSuccess

handleRequest conf req@(ReqSearch, _) = do
  filter <- checkFail "No filter specified."
         $ getAttr "filter" req >>= toFilter
  scope' <- checkFail "No scope specified."
         $ getAttr "scope" req >>= toScope
  attronly <- checkFail "Invalid attronly specified."
           $ (toAttrOnly . getAttr "attronly") req 
  attrs <- checkFail "No attrs specified. Has to be 'all' or\
                   \ space-separated list."
        $ getAttr "attrs" req >>= toAttrs
  let searchMods =  LC.scope scope'
                 <> LC.typesOnly attronly
  listHostsWith (Just filter) (Just searchMods) attrs conf
    >>= (makeUnique <&> formatEntries <&> return)
    >>= BL.putStrLn
    >> putStrDebug "Search successful"
    >> exitSuccess

handleRequest conf req@(ReqBind, _) = do
  entries <- listAllHosts conf
  bDn <- checkFail "No dn to bind to given."
      $ getAttr "dn" req <&> T.decodeUtf8 <&> LC.Dn
  bEntry <- checkFail  ("Entry not found: " `T.append` unDn bDn) $
         flip getEntry entries bDn
  let pw' = LC.Password <$> getAttr "cred" req
  pw <- checkFail "No credentials supplied." pw'
  authSuccess <- bindEntry bEntry pw
  let partial = [("matched", T.encodeUtf8 . unDn $ bDn)]
  if authSuccess then
    putStrDebug ("Auth successful for " `T.append` unDn bDn)
    >> (printResponse . makeResponse) (partial ++ [
        ("code", "0"),
        ("info", "Auth successful.")
    ]) >> exitSuccess
  else
    putStrDebug ("Auth failed for " `T.append` unDn bDn)
    >> (printResponse . makeResponse) (partial ++ [
        ("code", "49"),
        ("info", (T.encodeUtf8 . T.concat) $
          [ "Auth failed for original entry: "
          , (unDn. getDn . original) bEntry])
    ]) >> exitWith (ExitFailure 49)

handleRequest _ _ = exitFailure

toAttrs :: B.ByteString -> Maybe [LC.Attr]
toAttrs = AP.maybeResult . (flip AP.feed "") .
          AP.parse (AP.many1' $ attr <* AP.skipSpace)

toAttrOnly :: Maybe B.ByteString -> Maybe Bool
toAttrOnly (Just "0") = Just False
toAttrOnly (Just "1") = Just True
toAttrOnly Nothing    = Just False
toAttrOnly _          = Nothing

toScope :: B.ByteString -> Maybe LC.Scope
toScope "0" = Just LC.BaseObject
toScope "1" = Just LC.SingleLevel
toScope "2" = Just LC.WholeSubtree
toScope  _  = Nothing

putStrDebug :: T.Text -> IO ()
putStrDebug = mapM_ (T.putStrLn . T.append "DEBUG: ") . (T.splitOn "\n")

printResponse :: Response -> IO ()
printResponse = BL.putStrLn . TLO.runLdapPrinter printerResponse

printerResponse :: TLO.LdapPrinter Response
printerResponse = d
  where
    d (Response attrs) = do
      tell . pure $ "RESULT\n"
      mapM_ ((>> (tell . pure) "\n") . TLO.ldifAttr TLO.ldifEncodeAttrValue)
        attrs

setAttr :: (B.ByteString, B.ByteString) -> TLD.Attribute
setAttr (key, value) = (TLD.AttrType key, TLD.AttrValue value)

makeResponse :: [(B.ByteString, B.ByteString)] -> Response
makeResponse = Response . fmap setAttr
