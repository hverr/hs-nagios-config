module Nagios.Config.EDSL (
  -- * Types
  module Nagios.Config.EDSL.Types

  -- * Serialization
, Object(..), writeConfiguration
) where

import Nagios.Config.EDSL.Types
import Nagios.Config.EDSL.Serialize (Object(..), writeConfiguration)
