{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
module IOHK.Cicero.API.Action where

import Data.Coerce
import Data.Text
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Types
import Data.Map
import Data.Time.LocalTime
import Data.UUID as UUID
import Data.String
import Servant.API
import Servant.API.Generic
import Servant.API.NamedRoutes

type API = NamedRoutes ActionRoutes

newtype ActionID = ActionID { uuid :: UUID } deriving newtype (ToHttpApiData, FromJSON, ToJSON, Eq, Ord)

actionIdFromString :: String -> Maybe ActionID
actionIdFromString = coerce . UUID.fromString

-- | Action routes in the Cicero API
data ActionRoutes mode = ActionRoutes
  { create :: mode :- ReqBody '[JSON] CreateActionV1 :> Post '[JSON] CreateActionResponseV1
  , get :: mode :- Capture "id" ActionID :> Get '[JSON] ActionV1
  } deriving stock Generic

data CreateActionV1 = CreateAction
  { names :: !ActionNamesV1
  , source :: !ActionSourceV1
  }

instance ToJSON CreateActionV1 where
  toJSON ca = object $ "source" .= ca.source : case ca.names of
    Only n -> [ "name" .= n ]
    AllNames -> []
  toEncoding ca = pairs $ "source" .= ca.source <> case ca.names of
    Only n -> "name" .= n
    AllNames -> mempty

data CreateActionResponseV1
  = -- | The specific action created
    --
    -- Returned if an 'Only' was set in 'names'
    CreateActionOnly !ActionV1
  | -- | All actions created
    --
    -- Returned if 'AllNames' was set in 'names'
    CreateActionAll !([ ActionV1 ])

instance FromJSON CreateActionResponseV1 where
  parseJSON v = prependFailure "parsing CreateActionResponseV1 failed, " $ case v of
    Object _ -> CreateActionOnly <$> parseJSON v
    Array _ -> CreateActionAll <$> parseJSON v
    _ -> typeMismatch "Object or Array" v

instance ToJSON CreateActionResponseV1 where
  toJSON (CreateActionOnly act) = toJSON act
  toJSON (CreateActionAll acts) = toJSON acts

  toEncoding (CreateActionOnly act) = toEncoding act
  toEncoding (CreateActionAll acts) = toEncoding acts

-- | The source of an action, as a [go-getter URL](https://github.com/hashicorp/go-getter#url-format)
newtype ActionSourceV1 = ActionSource { unActionSource :: Text } deriving newtype (IsString, FromJSON, ToJSON)

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
newtype CUE = CUE { expr :: Text } deriving newtype (FromJSON, ToJSON)

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
  { id :: !ActionID
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
