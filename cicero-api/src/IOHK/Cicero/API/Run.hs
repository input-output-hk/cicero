{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module IOHK.Cicero.API.Run where

import Data.Aeson
import Data.Time.LocalTime
import Data.UUID
import Numeric.Natural
import Servant.API
import Servant.API.Generic
import Servant.API.NamedRoutes

type API = NamedRoutes RunRoutes

-- | Run routes in the Cicero API
data RunRoutes mode = RunRoutes
  { getAll :: mode
           :- QueryFlag "recursive"
           :> QueryParams "input" UUID
           :> QueryParam "offset" Natural
           :> QueryParam "limit" Natural
           :> Get '[JSON] [RunV1]
  } deriving stock Generic

data RunV1 = Run
  { nomadJobId :: !UUID
  , actionId :: !UUID
  , createdAt :: !ZonedTime
  , finishedAt :: !(Maybe ZonedTime)
  }

instance FromJSON RunV1 where
  parseJSON = withObject "RunV1" \o -> Run
    <$> o .: "nomad_job_id"
    <*> o .: "action_id"
    <*> o .: "created_at"
    <*> o .: "finished_at"

instance ToJSON RunV1 where
  toJSON r = object
    [ "nomad_job_id" .= r.nomadJobId
    , "action_id" .= r.actionId
    , "created_at" .= r.createdAt
    , "finished_at" .= r.finishedAt
    ]
  toEncoding r = pairs
    ( "nomad_job_id" .= r.nomadJobId
   <> "action_id" .= r.actionId
   <> "created_at" .= r.createdAt
   <> "finished_at" .= r.finishedAt
    )
