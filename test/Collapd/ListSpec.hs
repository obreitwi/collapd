{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Collapd.ListSpec (spec) where

import qualified Data.ByteString.Char8 as B
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Test.Hspec
import           Test.Hspec.Attoparsec

import           Collapd.Conf
import           Collapd.Filter as F
import           Collapd.List
import           Collapd.Utils
import           Ldap.Client as LC


host1 :: BackendEntry
host1 = BackendEntry
  "localhost"
  1234
  (Dn "ou=host1base,dc=original,dc=source1")
  (Attr "objectClass" := "posixAccount")

host2 :: BackendEntry
host2 = BackendEntry
  "localhost"
  3456
  (Dn "ou=host1base,dc=original,dc=source2")
  (Attr "objectClass" := "posixAccount")

nonUniqueList :: [CollapsedEntry]
nonUniqueList =
  [ CollapsedEntry (SearchEntry (Dn "uid=foobar,dc=some,dc=thing") [])
                   (SearchEntry (Dn "uid=foobar,dc=original,dc=source1") [])
                   host1
  , CollapsedEntry (SearchEntry (Dn "uid=foofoo,dc=some,dc=thing") [])
                   (SearchEntry (Dn "uid=foobar,dc=original,dc=source1") [])
                   host1
  , CollapsedEntry (SearchEntry (Dn "uid=deadbeef,dc=some,dc=thing") [])
                   (SearchEntry (Dn "uid=foobar,dc=original,dc=source2") [])
                   host2
  , CollapsedEntry (SearchEntry (Dn "uid=foobar,dc=some,dc=thing") [])
                   (SearchEntry (Dn "uid=foobar,dc=original,dc=source2") [])
                   host2
  ]

uniqueList :: [CollapsedEntry]
uniqueList =
  [ CollapsedEntry (SearchEntry (Dn "uid=foobar,dc=some,dc=thing") [])
                   (SearchEntry (Dn "uid=foobar,dc=original,dc=source1") [])
                   host1
  , CollapsedEntry (SearchEntry (Dn "uid=foofoo,dc=some,dc=thing") [])
                   (SearchEntry (Dn "uid=foobar,dc=original,dc=source1") [])
                   host1
  , CollapsedEntry (SearchEntry (Dn "uid=deadbeef,dc=some,dc=thing") [])
                   (SearchEntry (Dn "uid=foobar,dc=original,dc=source2") [])
                   host2
  ]

spec :: Spec
spec = do
  describe "Collapsing list" $ do
    it "make list unique" $
      (length . makeUnique) nonUniqueList == length uniqueList
