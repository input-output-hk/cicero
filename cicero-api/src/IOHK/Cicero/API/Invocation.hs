{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module IOHK.Cicero.API.Invocation where

import Data.Aeson
import Data.UUID as UUID
import Data.Coerce
import Data.Time.LocalTime
import Servant.API
import Servant.API.Generic
import Servant.API.NamedRoutes

import IOHK.Cicero.API.Action

newtype InvocationID = InvocationID { uuid :: UUID } deriving newtype (ToJSON, FromJSON, ToHttpApiData, Eq, Ord)

invocationIdFromString :: String -> Maybe InvocationID
invocationIdFromString = coerce . UUID.fromString

type API = NamedRoutes InvocationRoutes

data InvocationRoutes mode = InvocationRoutes
  { get :: mode :- Capture "id" InvocationID :> Get '[JSON] InvocationV1
  } deriving stock Generic

data InvocationV1 = Invocation
  { id :: !InvocationID
  , actionId :: !ActionID
  , createdAt :: !ZonedTime
  , finishedAt :: !(Maybe ZonedTime)
  }

instance FromJSON InvocationV1 where
  parseJSON = withObject "InvocationV1" \o -> Invocation
    <$> o .: "id"
    <*> o .: "action_id"
    <*> o .: "created_at"
    <*> o .:? "finished_at"

instance ToJSON InvocationV1 where
  toJSON i = object
    [ "id" .= i.id
    , "action_id" .= i.actionId
    , "created_at" .= i.createdAt
    , "finished_at" .= i.finishedAt
    ]
  toEncoding i = pairs
    ( "id" .= i.id
   <> "action_id" .= i.actionId
   <> "created_at" .= i.createdAt
   <> "finished_at" .= i.finishedAt
    )
