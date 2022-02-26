{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import IOHK.Cicero.API
import Servant.Client
import Servant.Client.Core.BaseUrl
import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Options.Applicative
import Control.Exception

import Action
import Fact

data Command
  = CmdFact !FactCommand
  | CmdAction !ActionCommand

commandParser :: Parser Command
commandParser = hsubparser
  ( command "fact" (CmdFact <$> factCommandInfo)
 <> command "action" (CmdAction <$> actionCommandInfo)
  )

data Args = Args
  { ciceroURL :: !BaseUrl
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
  <*> commandParser

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "cicero-cli — A tool for interacting with the Cicero API"
  )

getHandler :: Command -> Client ClientM API -> ClientEnv -> IO ()
getHandler (CmdAction acmd) = Action.handler acmd
getHandler (CmdFact fcmd) = Fact.handler fcmd

main :: IO ()
main = do
  args <- execParser argsInfo
  manager <- newManager tlsManagerSettings
  getHandler args.cmd
    (client $ Proxy @API)
    (mkClientEnv manager args.ciceroURL)
