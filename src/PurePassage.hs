module PurePassage where
import Data.Word (Word8)

class PurePassage p where
    pop :: Int -> p -> (Maybe ([Integer], p))
    push :: Integer -> p -> p
    actDup :: Word8 -> p -> p
    actSwap :: Word8 -> p -> p
