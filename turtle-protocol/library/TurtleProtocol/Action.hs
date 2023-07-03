module TurtleProtocol.Action where

import Linear.V2 (V2 (..))
import Relude
import TurtleProtocol.Client
import TurtleProtocol.Message qualified as TM

setSpeed :: (Connected m) => Text -> Text -> m ()
setSpeed turtle speed = sendMessage TM.SetSpeed {TM.turtle, TM.speed} IgnoreResponse

setPenColor :: (Connected m) => Text -> Text -> m ()
setPenColor turtle color = sendMessage TM.SetPenColor {TM.turtle, TM.color} IgnoreResponse

setPenSize :: (Connected m) => Text -> Double -> m ()
setPenSize turtle size = sendMessage TM.SetPenSize {TM.turtle, TM.size} IgnoreResponse

beginFill :: (Connected m) => Text -> m ()
beginFill turtle = sendMessage TM.BeginFill {TM.turtle} IgnoreResponse

endFill :: (Connected m) => Text -> m ()
endFill turtle = sendMessage TM.EndFill {TM.turtle} IgnoreResponse

forward :: (Connected m) => Text -> Double -> m ()
forward turtle distance = sendMessage TM.Forward {TM.turtle, TM.distance} IgnoreResponse

left :: (Connected m) => Text -> Double -> m ()
left turtle degrees = sendMessage TM.Left {TM.turtle, TM.degrees} IgnoreResponse

getPosition :: (Connected m) => Text -> m (V2 Double)
getPosition turtle = sendMessage TM.GetPosition {TM.turtle} $ AwaitResponse \case
  TM.PositionIs {TM.position} -> Just position
  _ -> Nothing
