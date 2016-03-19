module Graph where

import Jamo (Batchim)
import Data.Word (Word8)

data Inst = Fin
          | Add Inst          | Sub Inst          | Mul Inst          | Div Inst          | Mod Inst          | Comp Inst          | ConJump Inst          | Dup Inst          | Swap Inst
          | AddW Integer Inst | SubW Integer Inst | MulW Integer Inst | DivW Integer Inst | ModW Integer Inst | CompW Integer Inst | ConJumpW Integer Inst | DupW Integer Inst | SwapW Integer Inst
          | Drop Inst
          | PutChar | PutNum
          | Push Integer | GetChar | GetNum
          | Cursor Batchim | Mov Batchim
