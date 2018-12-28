{-# LANGUAGE OverloadedStrings #-}

module Collapd.Bind (
  bindEntry,
  getPassword
) where

import           Control.Exception (bracket_)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Ldap.Client as Ldap
import qualified Ldap.Client.Bind as Ldap
import qualified System.IO as IO

import           Collapd.Conf as CC
import           Collapd.Utils

bindEntry :: CollapsedEntry -> Password -> IO Bool
bindEntry entry pw = do
  conn <- Ldap.with (Ldap.Plain (unpack . host $ hEntry)) (port hEntry)
    $ \l -> do
    result <- Ldap.bindEither l bindDn pw
    case result of
      Left  _ -> return False
      Right _ -> return True
  case conn of
    -- in case of error authentification fails
    Left _ -> return False
    Right b -> return b
  where
    hEntry = backendEntry entry
    bindDn = getDn . original $ entry

prompt :: Text -> IO Text
prompt msg = do
  Text.putStr msg
  IO.hFlush IO.stdout
  Text.getLine

getPassword :: Dn -> IO Text
getPassword bindDn = bracket_ hideOutput showOutput $ do
  pwd <- prompt ("Password for ‘" <> (pack $ show bindDn) <> "’: ")
  Text.putStr "\n"
  return pwd

hideOutput, showOutput :: IO ()
hideOutput = IO.hSetEcho IO.stdout False
showOutput = IO.hSetEcho IO.stdout True
