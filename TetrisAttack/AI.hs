module TetrisAttack.AI (
  aiCommands
) where

--------------------------------------------------------------------------------
import qualified Lambency as L
import Control.Wire hiding ((.))

import TetrisAttack.Board.Types
import TetrisAttack.Cursor
--------------------------------------------------------------------------------

aiCommands :: L.GameWire (Cursor, BoardState) [CursorCommand]
aiCommands =
  let pulse :: L.GameWire a Bool
      pulse = (mkSFN $ \_ -> (True, pure False)) >>> for 3 --> pulse
  in pulse >>> (arr $ \x -> if x then [CursorCommand'Swap] else [])
