{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module IOHK.Cicero.API where

import Servant.API
import Servant.API.NamedRoutes
import Servant.API.Generic
import Data.Aeson
import Data.Aeson.Types

import IOHK.Cicero.API.Action

-- | The Cicero API
type API = NamedRoutes NamedAPI

-- | The routes for the Cicero API
data NamedAPI mode = NamedAPI
  { createAction :: !(mode :- "api" :> "action" :> ReqBody '[JSON] CreateActionV1 :> Post '[JSON] CreateActionResponseV1)
  }

-- | A request to create a new action
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
