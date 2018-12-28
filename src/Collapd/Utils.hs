module Collapd.Utils (
  CollapsedEntry(..),
  collapseEntry,
  getDn,
  translateDn,
  translateAttr,
  unDn
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Ldap.Client as Ldap

import           Collapd.Conf

-- Store both the collapsed and original entry for easy mapping.
-- This might change in the future but for ~500 entries it is still acceptable
-- memory-wise.
data CollapsedEntry = CollapsedEntry
  { collapsed :: SearchEntry
  , original :: SearchEntry
  , backendEntry :: BackendEntry
  } deriving (Show)

-- convert a search entry from one host to the common format
collapseEntry :: Dn -> BackendEntry -> SearchEntry -> CollapsedEntry
collapseEntry toDn entry original@(SearchEntry oldDn attrs) =
    CollapsedEntry
      { collapsed=collapsed
      , original=original
      , backendEntry=entry
      }
  where
    fromDn = base entry
    collapsed = SearchEntry newDn newAttrs
    newDn = translateDn fromDn toDn oldDn
    newAttrs = map (translateAttr fromDn toDn) attrs

getDn :: SearchEntry -> Dn
getDn (SearchEntry dn _) = dn

unDn :: Dn -> T.Text
unDn (Dn dn) = dn

-- translate sourceDn to targetDn by replacing fromDn with toDn
translateDn :: Dn -> Dn -> Dn -> Dn
translateDn (Dn fromDn) (Dn toDn) (Dn sourceDn) = Dn targetDn
  where
    targetDn = T.replace fromDn toDn sourceDn

translateAttr :: Functor f =>
                 Dn -> Dn -> (Attr, f AttrValue) -> (Attr, f AttrValue)
translateAttr (Dn fromDn) (Dn toDn) (key, value) = (key, fmap trans value)
  where
    trans = encodeUtf8 . T.replace fromDn toDn . decodeUtf8
