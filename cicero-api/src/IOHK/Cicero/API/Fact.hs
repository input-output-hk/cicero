{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IOHK.Cicero.API.Fact where

import Data.Coerce
import Data.Text
import Data.Aeson
import Data.Time.LocalTime
import Data.UUID
import Data.ByteString.Lazy
import Data.Binary.Builder
import Servant.API
import Servant.API.Generic
import Servant.API.NamedRoutes

import {-# SOURCE #-} IOHK.Cicero.API.Run (RunID)

newtype FactID = FactID { uuid :: UUID } deriving newtype (ToJSON, FromJSON, ToHttpApiData, Eq, Ord)

factIdFromString :: String -> Maybe FactID
factIdFromString = coerce . fromString

type API = NamedRoutes FactRoutes

-- | Fact routes in the Cicero API
data FactRoutes mode = FactRoutes
  { create :: mode :- ReqBody '[OctetStream] CreateFactV1 :> Post '[JSON] FactV1
  , getAll :: mode :- QueryParam' '[Required, Strict] "run" RunID :> Get '[JSON] [FactV1]
  } deriving stock Generic

data CreateFactV1 = CreateFact
  { -- | The data of the fact
    fact :: !Value
  , -- | Binary blob attached to the fact
    artifact :: !(Maybe ByteString)
  }

-- | This instance assumes that @'toEncoding' \@'Value'@ has no trailing whitespace!
instance MimeRender OctetStream CreateFactV1 where
  mimeRender _ cf = toLazyByteString $ factBuilt <> artifactBuilt
    where
      factBuilt = fromEncoding $ toEncoding cf.fact
      artifactBuilt = case cf.artifact of
        Just a -> fromLazyByteString a
        Nothing -> mempty

-- | A Cicero fact
data FactV1 = Fact
  { id :: !FactID
  , runId :: !(Maybe RunID)
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
