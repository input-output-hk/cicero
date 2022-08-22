{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Invocation where

import IOHK.Cicero.API.Invocation
import Options.Applicative
import Servant.Client
import Data.Aeson
import System.IO (stdout)
import Data.ByteString.Lazy.Char8
import Control.Exception

getInvocationParser :: Parser InvocationID
getInvocationParser = argument (maybeReader invocationIdFromString)
  ( metavar "INVOCATION_ID"
 <> help "the ID of the invocation to get"
  )

getInvocationInfo :: ParserInfo InvocationID
getInvocationInfo = info getInvocationParser
  ( fullDesc
 <> header "cicero-cli invocation get — Get information about an invocation"
  )

data InvocationCommand
  = CmdGetInvocation !InvocationID

invocationCommandParser :: Parser InvocationCommand
invocationCommandParser = hsubparser
  ( command "get" (CmdGetInvocation <$> getInvocationInfo)
  )

invocationCommandInfo :: ParserInfo InvocationCommand
invocationCommandInfo = info invocationCommandParser
  ( fullDesc
 <> header "cicero-cli invocation — Manage Cicero invocations"
  )

handler :: InvocationCommand -> Client ClientM API -> ClientEnv -> IO ()
handler (CmdGetInvocation id) invocationClient cEnv = runClientM (invocationClient.get id) cEnv >>= \case
  Left e -> throw e
  Right res -> hPutStrLn stdout $ encode res
