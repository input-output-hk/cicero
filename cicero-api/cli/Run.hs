{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
module Run where

import IOHK.Cicero.API
import Data.UUID as UUID
import Numeric.Natural
import Servant.Client
import Data.Aeson
import System.IO (stdout)
import Data.ByteString.Lazy.Char8
import Control.Exception
import Options.Applicative

data GetRunsArgs = GetRunsArgs
  { recursive :: !Bool
  , inputs :: ![ UUID ]
  , offset :: !(Maybe Natural)
  , limit :: !(Maybe Natural)
  }

getRunsArgsParser :: Parser GetRunsArgs
getRunsArgsParser = GetRunsArgs
  <$> switch
        ( long "recursive"
       <> help "get the transitive closure of runs started by facts at FACT_IDs"
        )
  <*> many (option (maybeReader UUID.fromString)
             ( long "fact-id"
            <> metavar "FACT_ID"
            <> help "only get runs started by this fact (multiple instances are ANDed together)"
             ))
  <*> optional (option auto
        ( long "offset"
       <> metavar "OFFSET"
       <> help "offset into result list to start query (for pagination)"
        ))
  <*> optional (option auto
        ( long "limit"
       <> metavar "OFFSET"
       <> help "offset into result list to start query (for pagination)"
        ))

getRunsInfo :: ParserInfo GetRunsArgs
getRunsInfo = info getRunsArgsParser
  ( fullDesc
 <> header "cicero-cli run get-all — Get information about runs"
  )

data RunCommand = CmdGetRuns !GetRunsArgs

runCommandParser :: Parser RunCommand
runCommandParser = hsubparser
  ( command "get-all" (CmdGetRuns <$> getRunsInfo)
  )

runCommandInfo :: ParserInfo RunCommand
runCommandInfo = info runCommandParser
  ( fullDesc
 <> header "cicero-cli run — Manage Cicero runs"
  )

handler :: RunCommand -> Client ClientM API -> ClientEnv -> IO ()
handler (CmdGetRuns gra) apiClient cEnv = runClientM (apiClient.getRuns gra.recursive gra.inputs gra.offset gra.limit) cEnv >>= \case
  Left e -> throw e
  Right res -> hPutStrLn stdout $ encode res.runs
