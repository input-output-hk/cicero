{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module IOHK.Cicero.API.Run where

import Data.Aeson
import Data.Coerce
import Data.Time.LocalTime
import Data.UUID
import Numeric.Natural
import Servant.API
import Servant.API.Generic
import Servant.API.NamedRoutes

import IOHK.Cicero.API.Fact
import IOHK.Cicero.API.Invocation

newtype RunID = RunID { uuid :: UUID } deriving newtype (ToJSON, FromJSON, ToHttpApiData, Eq, Ord)

runIdFromString :: String -> Maybe RunID
runIdFromString = coerce . fromString


type API = NamedRoutes RunRoutes

-- | Run routes in the Cicero API
data RunRoutes mode = RunRoutes
  { getAll :: mode
           :- QueryFlag "recursive"
           :> QueryParams "input" FactID
           :> QueryParam "offset" Natural
           :> QueryParam "limit" Natural
           :> Get '[JSON] [RunV2]
  , createFact :: mode
               :- Capture "id" RunID
               :> "fact"
               :> ReqBody '[OctetStream] CreateFactV1
               :> Post '[JSON] FactV1
  } deriving stock Generic

data RunV2 = Run
  { nomadJobId :: !RunID
  , invocationId :: !InvocationID
  , createdAt :: !ZonedTime
  , finishedAt :: !(Maybe ZonedTime)
  }

instance FromJSON RunV2 where
  parseJSON = withObject "RunV2" \o -> Run
    <$> o .: "nomad_job_id"
    <*> o .: "invocation_id"
    <*> o .: "created_at"
    <*> o .: "finished_at"

instance ToJSON RunV2 where
  toJSON r = object
    [ "nomad_job_id" .= r.nomadJobId
    , "invocation_id" .= r.invocationId
    , "created_at" .= r.createdAt
    , "finished_at" .= r.finishedAt
    ]
  toEncoding r = pairs
    ( "nomad_job_id" .= r.nomadJobId
   <> "invocation_id" .= r.invocationId
   <> "created_at" .= r.createdAt
   <> "finished_at" .= r.finishedAt
    )
