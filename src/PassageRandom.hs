{-# LANGUAGE RecordWildCards #-}

module PassageRandom(PassageRandom, newPassageRandom) where

import PurePassage (PurePassage(..))
import System.Random (newStdGen, randomR, StdGen)
import Util (if')
import Flag(SetUnset(SET))

data PassageRandom = PassageRandom { promiseDup :: Integer
                                   , needMoreCopy :: SetUnset
                                   , randomSeed :: StdGen
                                   , recent :: Integer
                                   }

instance Show PassageRandom where
    show PassageRandom{..} = "PRandom ("++show promiseDup++") next="++show recent

rn :: (Integer, Integer)
rn = (0, 32767)

instance PurePassage PassageRandom where
    actDup n p@PassageRandom { promiseDup = d, needMoreCopy = f } = p { promiseDup = d + fromIntegral (if' (f==SET && n>2) n 1) }
    actSwap _ p = p { promiseDup = 1 }
    push _ p = p
    pop n p@PassageRandom { promiseDup = dup, randomSeed = seed, recent = r } =
        Just $ case dup - fromIntegral n of
                 0 -> let (a, g) = randomR rn seed
                      in ( replicate (fromIntegral n) r
                         , p { promiseDup = 1, randomSeed = g, recent = a }
                         )
                 dn | dn > 0 -> ( replicate (fromIntegral n) r
                                , p { promiseDup = dn }
                                )
                    | otherwise -> ( replicate (fromIntegral dup) r ++ map fst ys
                                   , p { promiseDup = 1, randomSeed = g, recent = a }
                                   )
                        where xs = iterate (randomR rn . snd) $ randomR rn seed
                              (ys, (a, g):_) = splitAt (fromIntegral $ negate dn) xs

newPassageRandom :: SetUnset -> IO PassageRandom
newPassageRandom f = (\seed -> let (a, g) = randomR rn seed
                               in PassageRandom { promiseDup = 1, needMoreCopy = f, randomSeed = g, recent = a }) <$> newStdGen
