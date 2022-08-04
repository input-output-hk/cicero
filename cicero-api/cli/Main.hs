{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import IOHK.Cicero.API
import Servant.Client
import Servant.Client.Core.BaseUrl
import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Data.ByteString.Char8 as BS
import Options.Applicative
import Control.Exception
import System.Console.Haskeline
import Servant.API.BasicAuth
import Servant.Client.Core.BasicAuth

import Action
import Fact
import Invocation
import Run

data Command
  = CmdFact !FactCommand
  | CmdAction !ActionCommand
  | CmdInvocation !InvocationCommand
  | CmdRun !RunCommand

commandParser :: Parser Command
commandParser = hsubparser
  ( command "fact" (CmdFact <$> factCommandInfo)
 <> command "action" (CmdAction <$> actionCommandInfo)
 <> command "invocation" (CmdInvocation <$> invocationCommandInfo)
 <> command "run" (CmdRun <$> runCommandInfo)
  )

data Args = Args
  { ciceroURL :: !BaseUrl
  , ciceroUser :: !(Maybe ByteString)
  , cmd :: !Command
  }

baseUrlReader :: ReadM BaseUrl
baseUrlReader = do
  urlStr <- str
  case parseBaseUrl urlStr of
    Left e -> case fromException e of
      Just (InvalidBaseUrlException s) -> readerError $ "invalid URL '" ++ urlStr ++ "': " ++ s
      Nothing -> readerError $ "exception parsing '" ++ urlStr ++ "' as a URL: " ++ displayException e
    Right b -> pure b

argsParser :: Parser Args
argsParser = Args
  <$> option baseUrlReader
        ( long "cicero-url"
       <> metavar "CICERO_URL"
       <> help "URL of the cicero server"
       <> showDefaultWith showBaseUrl
       <> (value $ BaseUrl Http "localhost" 8080 "")
        )
  <*> (optional $ option str
        ( long "user"
       <> metavar "USER"
       <> help "User name for BASIC authentication with the cicero server"
        ))
  <*> commandParser

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "cicero-cli — A tool for interacting with the Cicero API"
  )

getHandler :: Command -> Client ClientM API -> ClientEnv -> IO ()
getHandler (CmdAction acmd) apiClient = Action.handler acmd apiClient.action
getHandler (CmdFact fcmd) apiClient = Fact.handler fcmd apiClient.fact
getHandler (CmdInvocation icmd) apiClient = Invocation.handler icmd apiClient.invocation
getHandler (CmdRun rcmd) apiClient = Run.handler rcmd apiClient.run

main :: IO ()
main = do
  args <- execParser argsInfo
  auth <- case args.ciceroUser of
    Nothing -> pure Nothing
    Just u -> runInputT defaultSettings (getPassword Nothing ("password for " ++ (BS.unpack u) ++ ":")) >>= \case
      Just s -> pure . Just . BasicAuthData u $ BS.pack s
      Nothing -> fail "no password provided"
  manager <- newManager tlsManagerSettings
  let
    cEnv = mkClientEnv manager args.ciceroURL
    cEnv' = case auth of
      Nothing -> cEnv
      Just ba -> cEnv
        { makeClientRequest = \u -> defaultMakeClientRequest u . basicAuthReq ba
        }
  getHandler args.cmd
    (client $ Proxy @API)
    cEnv'
