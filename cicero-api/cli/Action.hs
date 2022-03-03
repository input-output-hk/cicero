{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Action where

import IOHK.Cicero.API.Action
import Options.Applicative
import Servant.Client
import Data.Aeson
import System.IO (stdout)
import Data.ByteString.Lazy.Char8
import Control.Exception

namesParser :: Parser ActionNamesV1
namesParser
  =  Only <$> strOption
       ( long "name"
      <> metavar "NAME"
      <> help "the specific action to create from SOURCE (all if omitted)"
       )
 <|> pure AllNames

createActionParser :: Parser CreateActionV1
createActionParser = CreateAction
  <$> namesParser
  <*> argument str
        ( metavar "SOURCE"
       <> help "the action source as a go-getter URL"
        )

createActionInfo :: ParserInfo CreateActionV1
createActionInfo = info createActionParser
  ( fullDesc
 <> header "cicero-cli action create — Create a new action"
  )

getActionParser :: Parser ActionID
getActionParser = argument (maybeReader actionIdFromString)
  ( metavar "ACTION_ID"
 <> help "the ID of the action to get"
  )

getActionInfo :: ParserInfo ActionID
getActionInfo = info getActionParser
  ( fullDesc
 <> header "cicero-cli action get — Get information about an action"
  )

data ActionCommand
  = CmdCreateAction !CreateActionV1
  | CmdGetAction !ActionID

actionCommandParser :: Parser ActionCommand
actionCommandParser = hsubparser
  ( command "create" (CmdCreateAction <$> createActionInfo)
 <> command "get" (CmdGetAction <$> getActionInfo)
  )

actionCommandInfo :: ParserInfo ActionCommand
actionCommandInfo = info actionCommandParser
  ( fullDesc
 <> header "cicero-cli action — Manage Cicero actions"
  )

handler :: ActionCommand -> Client ClientM API -> ClientEnv -> IO ()
handler (CmdCreateAction ca) actionClient cEnv = runClientM (actionClient.create ca) cEnv >>= \case
  Left e -> throw e
  Right res -> hPutStrLn stdout $ encode res
handler (CmdGetAction id) actionClient cEnv = runClientM (actionClient.get id) cEnv >>= \case
  Left e -> throw e
  Right res -> hPutStrLn stdout $ encode res
