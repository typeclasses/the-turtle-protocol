module TurtleProtocol.Message where

import Data.Binary
import Linear.V2 (V2 (..))
import Relude

data Request
  = SetSpeed {turtle :: Text, speed :: Text}
  | SetPenColor {turtle :: Text, color :: Text}
  | SetPenSize {turtle :: Text, size :: Double}
  | BeginFill {turtle :: Text}
  | EndFill {turtle :: Text}
  | Forward {turtle :: Text, distance :: Double}
  | Left {turtle :: Text, degrees :: Double}
  | GetPosition {turtle :: Text}
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Binary)

data Response
  = Nil
  | PositionIs {position :: V2 Double}
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Binary)
