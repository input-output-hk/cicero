module IOHK.Cicero.API.Run where

import Data.Aeson
import Data.UUID (UUID)

newtype RunID = RunID { uuid :: UUID }

instance ToJSON RunID
instance FromJSON RunID
