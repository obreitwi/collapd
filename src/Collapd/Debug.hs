{-# LANGUAGE OverloadedStrings #-}

module Collapd.Debug (
  listLocal,
  myConf
) where

import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Ldap.Client as Ldap

data Conf = Conf
  { host :: String
  , port :: PortNumber
  } deriving (Show, Eq)


myConf :: Conf
myConf = Conf { host="localhost", port=3389 }

listLocal :: Conf -> IO (Either LdapError ())
listLocal conf = Ldap.with (Ldap.Plain (host conf)) (port conf) $ \l -> do
  results <- Ldap.search l
                        (Dn "ou=people,dc=kip.uni-heidelberg,dc=de")
                        (typesOnly False)
                        (Attr "objectClass" := "posixAccount")
                        [Attr "uid"]
  mapM_ (putStrLn . show) $ zip [1..] $ results
