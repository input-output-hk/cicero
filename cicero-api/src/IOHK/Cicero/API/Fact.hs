{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  } deriving Show

instance FromJSON FactV1 where
  parseJSON = withObject "FactV1" \o -> Fact
    <$> o .: "id"
    <*> o .:? "run_id"
    <*> o .: "created_at"
    <*> o .: "value"
    <*> o .:? "binary_hash"

instance ToJSON FactV1 where
  toJSON f = object $
      [ "id" .= f.id
      , "created_at" .= f.createdAt
      , "value" .= f.value
      ] ++ runIdFields ++ binaryHashFields
    where
      runIdFields = case f.runId of
        Just rid -> [ "run_id" .= rid ]
        Nothing -> []
      binaryHashFields = case f.binaryHash of
        Just hash -> [ "binary_hash" .= hash ]
        Nothing -> []
  toEncoding f = pairs $
      ( "id" .= f.id
     <> "created_at" .= f.createdAt
     <> "value" .= f.value
      ) <> runIdFields <> binaryHashFields
    where
      runIdFields = case f.runId of
        Just rid -> "run_id" .= rid
        Nothing -> mempty
      binaryHashFields = case f.binaryHash of
        Just hash -> "binary_hash" .= hash
        Nothing -> mempty
