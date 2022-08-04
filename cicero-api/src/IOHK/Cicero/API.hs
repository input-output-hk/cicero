{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module IOHK.Cicero.API where

import Servant.API
import Servant.API.NamedRoutes
import Servant.API.Generic

import IOHK.Cicero.API.Action as Action
import IOHK.Cicero.API.Fact as Fact
import IOHK.Cicero.API.Invocation as Invocation
import IOHK.Cicero.API.Run as Run

-- | The Cicero API
type API = "api" :> NamedRoutes APIRoutes

-- | The routes for the Cicero API
data APIRoutes mode = APIRoutes
  { action :: mode :- "action" :> Action.API
  , fact :: mode :- "fact" :> Fact.API
  , invocation :: mode :- "invocation" :> Invocation.API
  , run :: mode :- "run" :> Run.API
  } deriving stock Generic
