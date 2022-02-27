{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
module IOHK.Cicero.API where

import Servant.API
import Servant.API.NamedRoutes
import Servant.API.Generic
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy
import Data.Binary.Builder
import Data.UUID
import Numeric.Natural

import IOHK.Cicero.API.Action
import IOHK.Cicero.API.Fact
import IOHK.Cicero.API.Run

-- | The Cicero API
type API = "api" :> NamedRoutes APIRoutes

-- | The routes for the Cicero API
data APIRoutes mode = APIRoutes
  { createAction :: !(mode :- "action" :> ReqBody '[JSON] CreateActionV1 :> Post '[JSON] CreateActionResponseV1)
  , createFact :: !(mode :- "fact" :> ReqBody '[OctetStream] CreateFactV1 :> Post '[JSON] CreateFactResponseV1)
  , getRuns :: !(mode :- "run" :> QueryFlag "recursive" :> QueryParams "input" UUID :> QueryParam "offset" Natural :> QueryParam "limit" Natural :> Get '[JSON] GetRunsResponseV1)
  } deriving Generic

data CreateActionV1 = CreateAction
  { names :: !ActionNamesV1
  , source :: !ActionSourceV1
  }

instance ToJSON CreateActionV1 where
  toJSON (CreateAction {..}) = object $ "source" .= source : case names of
    Only n -> [ "name" .= n ]
    AllNames -> []
  toEncoding (CreateAction {..}) = pairs $ "source" .= source <> case names of
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

data CreateFactV1 = CreateFact
  { -- | The data of the fact
    fact :: !Value
  , -- | Binary blob attached to the fact
    artifact :: !(Maybe ByteString)
  }

-- | This instance assumes that @'toEncoding' \@'Value'@ has no trailing whitespace!
instance MimeRender OctetStream CreateFactV1 where
  mimeRender _ (CreateFact {..}) = toLazyByteString $ factBuilt <> artifactBuilt
    where
      factBuilt = fromEncoding $ toEncoding fact
      artifactBuilt = case artifact of
        Just a -> fromLazyByteString a
        Nothing -> mempty

newtype CreateFactResponseV1 = CreateFactResponse { fact :: FactV1 } deriving newtype FromJSON

newtype GetRunsResponseV1 = GetRunsResponse { runs :: [ RunV1 ] } deriving newtype FromJSON
