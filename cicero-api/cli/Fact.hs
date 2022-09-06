{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Fact where

import IOHK.Cicero.API.Fact
import IOHK.Cicero.API.Run hiding (API)
import Servant.Client
import Options.Applicative
import Control.Exception
import Data.Aeson
import Data.Aeson.Parser.Internal hiding (value)
import System.Posix.Types
import System.Posix.IO
import System.IO (stdout)
import Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8
import Data.Attoparsec.ByteString (feed, eitherResult, parse, IResult(Partial))
import Data.UUID

data Readable
  = File !FilePath
  | Cap !Fd

consume :: Readable -> IO ByteString
consume (File f) = LBS.readFile f
consume (Cap fd) = fdToHandle fd >>= hGetContents

readableOption :: Mod OptionFields FilePath -> Mod OptionFields Fd -> Parser Readable
readableOption fileFields capFields
  =  File <$> strOption fileFields
 <|> Cap <$> option auto capFields

data FactSource
  = LiteralFact !Value
  | ReadableFact !Readable

data CreateFactArgs = CreateFactArgs
  { factSource :: !FactSource
  , artifactSource :: !(Maybe Readable)
  }

valueReader :: ReadM Value
valueReader = do
  bytes <- str
  case eitherDecodeStrict' bytes of
    Left e -> readerError $ "error parsing " ++ show bytes ++ ": " ++ e
    Right v -> pure v

factSourceParser :: Parser FactSource
factSourceParser
  =  LiteralFact <$> option valueReader
       ( long "fact"
      <> metavar "FACT"
      <> help "the fact value"
       )
 <|> ReadableFact <$> readableOption
       ( long "fact-file"
      <> metavar "FACT_FILE"
      <> help "the path to a file containing the fact value"
       )
       ( long "fact-fd"
      <> metavar "FACT_FD"
      <> help "a file descriptor containing the fact value"
       )

createFactArgsParser :: Parser CreateFactArgs
createFactArgsParser = CreateFactArgs
  <$> factSourceParser
  <*> (optional $ readableOption
        ( long "artifact-file"
       <> metavar "ARTIFACT_FILE"
       <> help "the file containing the artifact to attach"
        )
        ( long "artifact-fd"
       <> metavar "ARTIFACT_FD"
       <> help "a file descriptor containing the artifact to attach"
        ))

createFactArgsInfo :: ParserInfo CreateFactArgs
createFactArgsInfo = info createFactArgsParser
  ( fullDesc
 <> header "cicero-cli fact create — Create a new fact"
  )

getFactsParser :: Parser RunID
getFactsParser = option (maybeReader runIdFromString)
  ( long "run-id"
 <> metavar "RUN_ID"
 <> help "the ID of the run which produced the facts"
  )

getFactsInfo :: ParserInfo RunID
getFactsInfo = info getFactsParser
  ( fullDesc
 <> header "cicero-cli fact get-all — Get information about facts"
  )

data FactCommand
  = CmdCreateFact !CreateFactArgs
  | CmdGetFacts !RunID

factCommandParser :: Parser FactCommand
factCommandParser = hsubparser
  ( command "create" (CmdCreateFact <$> createFactArgsInfo)
 <> command "get-all" (CmdGetFacts <$> getFactsInfo)
  )

factCommandInfo :: ParserInfo FactCommand
factCommandInfo = info factCommandParser
  ( fullDesc
 <> header "cicero-cli fact — Manage Cicero facts"
  )

data FactParseException = FactParseException
  { parseError :: !String
  } deriving Show

instance Exception FactParseException

createFactHandler :: CreateFactArgs -> (CreateFactV1 -> ClientM FactV1) -> ClientEnv -> IO ()
createFactHandler createArgs call cEnv = do
  fact <- case createArgs.factSource of
    LiteralFact v -> pure v
    ReadableFact f -> do
      factBytes <- consume f
      let parsedBytes = foldlChunks feed (Partial (parse jsonEOF')) factBytes
          res = feed parsedBytes ""
      case eitherResult res of
        Left e -> throw $ FactParseException e
        Right v -> pure v
  artifact <- case createArgs.artifactSource of
    Nothing -> pure Nothing
    Just f -> Just <$> consume f
  runClientM (call (CreateFact { .. })) cEnv >>= \case
    Left e -> throw e
    Right res -> hPutStrLn stdout $ encode res

handler :: FactCommand -> Client ClientM API -> ClientEnv -> IO ()
handler (CmdCreateFact createArgs) factClient cEnv =
  createFactHandler createArgs factClient.create cEnv
handler (CmdGetFacts rid) factClient cEnv = runClientM (factClient.getAll rid) cEnv >>= \case
  Left e -> throw e
  Right res -> hPutStrLn stdout $ encode res
