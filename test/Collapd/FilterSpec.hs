{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Collapd.FilterSpec (spec) where

import qualified Data.ByteString.Char8 as B
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Test.Hspec
import           Test.Hspec.Attoparsec

import           Collapd.Filter as F
import           Ldap.Client as LC

deriving instance Eq LC.Filter

spec :: Spec
spec = do
  describe "Filter parsing" $ do
    it "present" $ do
      B.pack "(uid=*)" ~> ldapFilter `shouldParse` (Present (Attr "uid"))

    it "and" $ do
      B.pack "(&(uid=*)(cn=foobar))" ~> ldapFilter `shouldParse`
        (And (Present (Attr "uid") :| [Attr "cn" := "foobar"]))

    it "or" $ do
      B.pack "(|(uid=*)(cn=foobar))" ~> ldapFilter `shouldParse`
        (Or (Present (Attr "uid") :| [Attr "cn" := "foobar"]))

    it "not" $ do
      B.pack "(|(uid=*)(!(cn=foobar)))" ~> ldapFilter `shouldParse`
        (Or (Present (Attr "uid") :| [Not (Attr "cn" := "foobar")]))

    it "substring (full)" $ do
      B.pack "(uid=foo*bar*deadbeef)" ~> ldapFilter `shouldParse`
        (Attr "uid" :=* (Just "foo", ["bar"], Just "deadbeef"))

    it "substring (begin missing)" $ do
      B.pack "(uid=*bar*deadbeef)" ~> ldapFilter `shouldParse`
        (Attr "uid" :=* (Nothing, ["bar"], Just "deadbeef"))

    it "substring (begin & end missing)" $ do
      B.pack "(uid=*bar*)" ~> ldapFilter `shouldParse`
        (Attr "uid" :=* (Nothing, ["bar"], Nothing))

    it "substring (end missing)" $ do
      B.pack "(uid=foo*bar*)" ~> ldapFilter `shouldParse`
        (Attr "uid" :=* (Just "foo", ["bar"], Nothing))

    it "substring (middle missing)" $ do
      B.pack "(uid=foo*deadbeef)" ~> ldapFilter `shouldParse`
        (Attr "uid" :=* (Just "foo", [], Just "deadbeef"))

    it "substring (several in middle)" $ do
      B.pack "(uid=foo*dead*beef*bar)" ~> ldapFilter `shouldParse`
        (Attr "uid" :=* (Just "foo", ["dead", "beef"], Just "bar"))

    it "approx" $ do
      B.pack "(uid~=foobar)" ~> ldapFilter `shouldParse`
        (Attr "uid" :~= "foobar")

    it "greater" $ do
      B.pack "(uid>=foobar)" ~> ldapFilter `shouldParse`
        (Attr "uid" :>= "foobar")

    it "less" $ do
      B.pack "(uid<=foobar)" ~> ldapFilter `shouldParse`
        (Attr "uid" :<= "foobar")

    it "ascii escape" $ do
      B.pack "(|(uid=*)(cn=fo\\a3bar))" ~> ldapFilter `shouldParse`
        (Or (Present (Attr "uid") :| [Attr "cn" := "fo\\a3bar"]))
