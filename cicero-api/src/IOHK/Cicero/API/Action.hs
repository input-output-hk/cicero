{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IOHK.Cicero.API.Action where

import Data.Text
import Data.Aeson
import Data.Aeson.Encoding
import Data.Map
import Data.Time.LocalTime
import Data.UUID
import Data.String


-- | The source of an action, as a [go-getter URL](https://github.com/hashicorp/go-getter#url-format)
newtype ActionSourceV1 = ActionSource { unActionSource :: Text } deriving (IsString)

instance FromJSON ActionSourceV1 where
  parseJSON v = ActionSource <$> parseJSON v

instance ToJSON ActionSourceV1 where
  toJSON = toJSON . unActionSource
  toEncoding = toEncoding . unActionSource

-- | The action names to add from a given 'ActionSourceV1'
data ActionNamesV1
  = -- | Just add the action with the given name
    Only !Text
  | -- | Add all names exported by the source
    AllNames

-- | Which matching facts to include
data InputDefinitionSelectV1
  = -- | Include the latest matching fact
    InputDefinitionSelectLatest
  | -- | Include all matching facts
    InputDefinitionSelectAll

instance FromJSON InputDefinitionSelectV1 where
  parseJSON = withText "InputDefinitionSelectV1" $ \s ->
    if s == "latest"
    then pure InputDefinitionSelectLatest
    else if s == "all"
    then pure InputDefinitionSelectAll
    else fail $ "expected 'latest' or 'all', got " ++ (unpack s)

instance ToJSON InputDefinitionSelectV1 where
  toJSON InputDefinitionSelectLatest = String "latest"
  toJSON InputDefinitionSelectAll = String "all"
  toEncoding InputDefinitionSelectLatest = text "latest"
  toEncoding InputDefinitionSelectAll = text "all"

-- | A [CUE lang](https://cuelang.org/) value
--
-- For now just a wrapper around 'Text' that we hope parses
newtype CUE = CUE { unCUE :: Text }

instance FromJSON CUE where
  parseJSON v = CUE <$> parseJSON v

instance ToJSON CUE where
  toJSON = toJSON . unCUE
  toEncoding = toEncoding . unCUE

-- | An input to a job
data InputV1 = Input
  { select :: !InputDefinitionSelectV1
  , -- | Negate the matching condition?
    not :: !Bool
  , -- | Can this input be missing?
    optional :: !Bool
  , -- | Which facts to match
    match :: !CUE
  }

instance FromJSON InputV1 where
  parseJSON = withObject "InputV1" \o -> Input
    <$> o .: "select"
    <*> o .: "not"
    <*> o .: "optional"
    <*> o .: "match"

instance ToJSON InputV1 where
  toJSON i = object
    [ "select" .= i.select
    , "not" .= i.not
    , "optional" .= i.optional
    , "match" .= i.match
    ]
  toEncoding i = pairs
    ( "select" .= i.select
   <> "not" .= i.not
   <> "optional" .= i.optional
   <> "match" .= i.match
    )

-- | An action
data ActionV1 = Action
  { id :: !UUID
  , name :: !Text
  , source :: !ActionSourceV1
  , createdAt :: !ZonedTime
  , active :: !Bool
  , meta :: !(Maybe (Map Text Value))
  , inputs :: !(Map Text InputV1)
  }

instance FromJSON ActionV1 where
  parseJSON = withObject "ActionV1" \o -> Action
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "source"
    <*> o .: "created_at"
    <*> o .: "active"
    <*> o .: "meta"
    <*> o .: "inputs"

instance ToJSON ActionV1 where
  toJSON a = object
    [ "id" .= a.id
    , "name" .= a.name
    , "source" .= a.source
    , "created_at" .= a.createdAt
    , "active" .= a.active
    , "meta" .= a.meta
    , "inputs" .= a.inputs
    ]
  toEncoding a = pairs
    ( "id" .= a.id
   <> "name" .= a.name
   <> "source" .= a.source
   <> "created_at" .= a.createdAt
   <> "active" .= a.active
   <> "meta" .= a.meta
   <> "inputs" .= a.inputs
    )
