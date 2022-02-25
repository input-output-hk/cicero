{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module IOHK.Cicero.API.Fact where

import Data.Text
import Data.Aeson
import Data.Time.LocalTime
import Data.UUID

-- | A Cicero fact
data FactV1 = Fact
  { id :: !UUID
  , runId :: !(Maybe UUID)
  , createdAt :: !ZonedTime
  , value :: !Value
  , -- | The hash of the artifact, if any
    --
    -- This should be a proper hash type
    binaryHash :: !(Maybe Text)
  }

instance FromJSON FactV1 where
  parseJSON = withObject "FactV1" \o -> Fact
    <$> o .: "id"
    <*> o .:? "run_id"
    <*> o .: "created_at"
    <*> o .: "value"
    <*> o .:? "binary_hash"
