{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Interpret (interpret) where

import Data.Map.Strict (Map, fromList, union)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)

import Control.Exception (SomeException, try)
import Control.Monad (liftM, liftM2)
import Control.Arrow (second)

import Data.Char (chr, ord)
import Data.Word (Word8)

import System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure), die)

import Data.Text (Text)

import System.IO (hLookAhead, stdin)
import System.IO.Error (catchIOError, isEOFError)

import Data.Array (Array, (!), bounds)
import Data.Ix (inRange, Ix)
import Data.Sequence (Seq, ViewL((:<), EmptyL), viewl, (<|), (|>), (><), viewr, ViewR((:>), EmptyR))
import qualified Data.Sequence as Seq

import Debug.NoTrace (trace)

import Parser (codeParser, isSpace, isEndOfLine)
import Flag (Flag(..), getFlag, isFlag, reMap, SetUnset(..), WsIgnore(..), Spec(..))
import Util (if', during, during1)
import qualified PurePassage as PP
import PassageNull(PassageNull, newPassageNull)
import PassageRandom(PassageRandom, newPassageRandom)

-- Cho, Jung, Jong(J_), Moeum, Batchim, Syllable(Syllable), syllable, Jaeum, ㄱ..ㅎ, ㅏ..ㅣ
import Jamo

data Speed = U Word8 | D Word8 | L Word8 | R Word8 deriving (Eq, Show)
data Storage = Stack (Seq Integer) | Queue (Seq Integer) | PNull PassageNull | PRandom PassageRandom deriving (Show)

-- https://twitter.com/hooneu777/status/699937987761537030
-- 이름 추천 받습니다
data Tiffany = Tiffany { speed :: Speed
                       , storageP :: Batchim
                       , storages :: Map Batchim Storage
                       } deriving (Show)

-- codeParser :: Text -> Either String (Map ByteString ByteString, Array Int (Array Int (Maybe Syllable)))

(!>) :: (Show a, Ix a) => Maybe (Array a b) -> a -> Maybe b
Nothing !> _ = Nothing
Just arr !> a = if inRange (bounds arr) a
                   then Just (arr ! a)
                   else Nothing
infixl 9 !>

flipDirection :: Speed -> Speed
flipDirection (U a) = D a
flipDirection (D a) = U a
flipDirection (L a) = R a
flipDirection (R a) = L a


changeDistance :: Speed -> Word8 -> Speed
changeDistance (U _) a = U a
changeDistance (D _) a = D a
changeDistance (L _) a = L a
changeDistance (R _) a = R a

setSpeedByJung :: Jung -> Speed -> Speed
setSpeedByJung jung sp
    | jung == ㅡ = case sp of
                      U d -> D d
                      D d -> U d
                      _   -> sp
    | jung == ㅣ = case sp of
                      L d -> R d
                      R d -> L d
                      _   -> sp
    | jung == ㅢ = flipDirection sp
    | otherwise = fromMaybe sp (Map.lookup jung m)
        where m = fromList [(ㅏ, R 1), (ㅑ, R 2), (ㅓ, L 1), (ㅕ, L 2),
                            (ㅗ, U 1), (ㅛ, U 2), (ㅜ, D 1), (ㅠ, D 2)]

pop :: Int -> Storage -> Maybe ([Integer], Storage)
pop n (PNull a)   = second PNull   <$> PP.pop n a
pop n (PRandom a) = second PRandom <$> PP.pop n a
pop 0 stor = Just ([], stor)
pop n (Stack sequ) = case viewl sequ of
                       EmptyL -> Nothing
                       a :< sq -> do
                           (xs, sq') <- pop (n-1) (Stack sq)
                           Just (a:xs, sq')
pop n (Queue sequ) = case viewl sequ of
                       EmptyL -> Nothing
                       a :< sq -> do
                           (xs, sq') <- pop (n-1) (Queue sq)
                           Just (a:xs, sq')

push :: Integer -> Storage -> Storage
push v (PNull a)   = PNull   $ PP.push v a
push v (PRandom a) = PRandom $ PP.push v a
push v (Stack sq) = Stack (v <| sq)
push v (Queue sq) = Queue (sq |> v)


calc :: (Integer -> b) -> Storage -> Maybe (b, Storage)
calc f stor = do
    ([a], s) <- pop 1 stor
    Just (f a, s)

calc2 :: (Integer -> Integer -> b) -> Storage -> Maybe (b, Storage)
calc2 f stor = do
    ([a, b], s) <- pop 2 stor
    Just (f b a, s)

batchimMapCommon, batchimMapExt, batchimMapNoExt :: Map Batchim Word8
batchimMapCommon = fromList . concat $ [(J_, 0), (ㄹ, 5), (ㅄ, 6), (ㄺ, 7), (ㅀ, 8), (ㄻ, 9), (ㄼ, 9)]
                                     : map (\(xs, y) -> map (,y) xs) [([ㄱ, ㄴ, ㅅ], 2), ([ㄷ, ㅈ, ㅋ], 3), ([ㅁ, ㅂ, ㅊ, ㅌ, ㅍ, ㄲ, ㅆ], 4)]
batchimMapExt   = union batchimMapCommon $ fromList [(ㄳ, 1), (ㄶ, 10), (ㄵ, 11), (ㄽ, 12), (ㄾ, 13), (ㄿ, 14)]
batchimMapNoExt = union batchimMapCommon $ fromList [(ㄳ, 4), (ㄶ,  5), (ㄵ,  5), (ㄽ,  7), (ㄾ,  9), (ㄿ,  9)]

batchimVal :: Bool -> Batchim -> Word8
batchimVal ext = (Map.!) $ if' ext batchimMapExt batchimMapNoExt

interpret :: Text -> IO ()
interpret text = case codeParser text of
                   Left str -> die str
                   Right (flags, code) -> runCode (reMap flags) code

runCode :: Set Flag -> Array Int (Array Int (Maybe Syllable)) -> IO ()
runCode flags code = do
    passage <- case getF PASSAGE_SPEC of
                 PASSAGE_SPEC NULL -> return (PNull newPassageNull)
                 PASSAGE_SPEC RANDOM -> PRandom <$> (newPassageRandom $ if' (isF NEED_MORE_COPY SET) SET UNSET)
    codeRunner (0, 0) (Tiffany { speed = D 1, storages = Map.singleton ㅎ passage, storageP = J_ })
    where codeRunner :: (Int, Int) -> Tiffany -> IO ()
          codeRunner (row, col) tiffany@(Tiffany { speed = inertiaSpeed }) = do
              let go sp (r, c) =
                      let (rmin,  rmax) = bounds code
                          ~(cmin, cmax) = bounds (code ! r)
                      in case sp of
                           U a -> (let x = r - fromIntegral a in if' (x>=rmin) x rmax, c)
                           D a -> (let x = r + fromIntegral a in if' (x<=rmax) x rmin, c)
                           L a -> (r, let x = c - fromIntegral a in if' (x>=cmin) x cmax)
                           R a -> (r, let x = c + fromIntegral a in if' (x<=cmax) x cmin)
              case Just code !> row !> col of
                Nothing          -> codeRunner (go inertiaSpeed (row, col)) tiffany
                Just Nothing     -> codeRunner (go inertiaSpeed (row, col)) tiffany
                Just (Just syll) -> do
                    tf@(Tiffany { speed = speed' }) <- runLetter syll tiffany
                    codeRunner (go speed' (row, col)) (trace (show syll ++ show (row, col) ++ show tf) tf)

          getF :: Bounded a => (a -> Flag) -> Flag
          getF = flip getFlag flags
          wsIgnoreLevel = let (INPUT_WHITESPACE_IGNORE_LEVEL x) = getF INPUT_WHITESPACE_IGNORE_LEVEL in x
          isF f a = isFlag f a flags
          redefBatchim = isF OVERWRITE_REDEFINED_BATCHIMS SET
          calcBatchim = isF CALC_BY_BATCHIM SET
          batchimV = batchimVal redefBatchim

          runLetter :: Syllable -> Tiffany -> IO Tiffany
          runLetter (Syllable cho jung batchim) tiffany = run
              where
                  tf@(Tiffany { speed = speed', storageP = storageP', storages = storages' }) = tiffany { speed = setSpeedByJung jung (speed tiffany) }
                  storp p = case Map.lookup p storages' of
                              Just a -> a
                              Nothing -> if' (p==ㅇ) (Queue Seq.empty) (Stack Seq.empty)

                  stor = storp storageP'
                  reStorP s = Map.insert storageP' s storages'
                  cal f = if' (calcBatchim && batchim `notElem` [J_, ㅇ, ㅎ]) (calc (\x -> f x . fromIntegral $ batchimV batchim)) (calc2 f) stor
                  cal' f = case cal f of
                             Nothing -> tf { speed = flipDirection speed' }
                             Just (a, s) -> tf { storages = reStorP $ push a s }
                  calZero f = case cal (\b a -> if' (a==0) Nothing (Just $ f b a)) of
                                Nothing -> tf { speed = flipDirection speed' }
                                Just (Just a,  s) -> tf { storages = reStorP $ push a s }
                                Just (Nothing, s) -> tf { speed = flipDirection speed', storages = reStorP . if' calcBatchim id (snd . fromJust . pop 1) $ s }
                  run = fromMaybe (return tf) . lookup cho $
                          [ (ㅇ, return tf)
                          , (ㅎ, case maybe 0 (head . fst) (pop 1 stor) of
                                   0 -> exitSuccess
                                   a -> exitWith . ExitFailure $ fromInteger a
                            )
                          , (ㅉ, return $ if isF CANNON_JUMP SET
                                              then let x = batchimV batchim
                                                   in if' (x>=3) (tf { speed = changeDistance speed' x }) tf
                                              else tf
                            )
                          , (ㄷ, return $ cal' (+))
                          , (ㅌ, return $ cal' (-))
                          , (ㄸ, return $ cal' (*))
                          , (ㄴ, return $ calZero div)
                          , (ㄹ, return $ calZero mod)
                          , (ㅈ, return $ cal' (\b a -> if' (b>=a) 1 0))

                          {-
                            > For the purposes of the layout rule, Unicode characters in a source program are considered to be of the same, fixed, width as an ASCII character. - Haskell 98 Report
                            한글 음절 문자나 한글 자모 문자는 영문 알파벳 문자와 너비가 똑같다고 취급됩니다. 예로 이 코드는
                            ```
                            (ㅊ, return $ let a = b
                                              c = d
                                          in a + c
                            ```
                            다음과 같습니다.
                            ```
                            let x = ㅊ
                            in (x, return $ let a = b
                                                 c = d
                                             in a + c
                            ```
                          -}
                          , (ㅊ, return $
                                          let v = batchimV batchim
                                              changeD = if isF CONDITIONAL_CANNON_JUMP SET && (v>=3)
                                                           then flip changeDistance v
                                                           else id
                                          in case pop 1 stor of
                                               Just ([a], s) | a==0      -> tf' { speed = changeD (flipDirection speed') }
                                                             | otherwise -> tf' { speed = changeD speed' }
                                                             where tf' = tf { storages = reStorP s }
                                               _ -> tf { speed = flipDirection speed', storages = reStorP stor }
                            )
                          , (ㅃ,
                              let v = batchimV batchim
                                  nmc = isF NEED_MORE_COPY SET && batchim /= ㅇ && batchim /= ㅎ && v>=2
                                  v' = if' (batchim==ㅇ || batchim==ㅎ) 0 v
                                  f constr sq = case pop 1 stor of
                                                  Just ([a], _) -> tf { storages = reStorP . constr $ if' nmc (Seq.replicate (fromIntegral v) a >< sq) (a <| sq) }
                                                  _ -> tf { speed = flipDirection speed' }
                              in return $ case stor of
                                            PNull a   -> tf { storages = reStorP . PNull   $ PP.actDup v' a }
                                            PRandom a -> tf { storages = reStorP . PRandom $ PP.actDup v' a }
                                            Queue sq  -> f Queue sq
                                            Stack sq  -> f Queue sq
                            )
                            , (ㅍ, return $
                              let x = let v = fromIntegral $ batchimV batchim
                                      in if' (isF DEEP_EXCHANGE SET && batchim /= ㅇ && batchim /= ㅎ && v>0) v 0
                              in case stor of
                                   PNull a   -> tf { storages = reStorP . PNull   $ PP.actSwap (fromIntegral x) a }
                                   PRandom a -> tf { storages = reStorP . PRandom $ PP.actSwap (fromIntegral x) a }
                                   _ -> case pop (x+2) stor of
                                          Just (h:xs, s) ->
                                              let f constr sq = case viewr (Seq.fromList xs) of
                                                                  leftsq :> l -> tf { storages = reStorP . constr $ (l <| leftsq) >< (h <| sq) }
                                                                  EmptyR      -> tf { speed = flipDirection speed' }
                                              in case s of
                                                   Queue sq -> f Queue sq
                                                   Stack sq -> f Stack sq
                                                   _ -> undefined
                                          _ -> tf { speed = flipDirection speed' }
                            )
                          , (ㅅ, return tf { storageP = batchim })
                          , (ㅆ, return $ case pop 1 stor of
                                            Just ([a], s) -> tf { storages = Map.insert batchim (push a $ storp batchim) (reStorP s) }
                                            _ -> tf { speed = flipDirection speed' }
                            )
                          , (ㅁ, case pop 1 stor of
                                   Just ([a], s) | batchim == ㅇ -> putStr (show a) >> tf'
                                                 | batchim == ㅎ -> (try (putChar . chr $ fromInteger a) :: IO (Either SomeException ())) >> tf'
                                                 | otherwise -> tf'
                                                 where tf' = return tf { storages = reStorP s }
                                   _ -> return tf { speed = flipDirection speed' }
                            )
                          , (ㅂ,
                              let v = batchimV batchim
                              in case () of
                                   _ | batchim == ㅇ -> do
                                         (m, h) <- during1 (\(_, b) -> '0'>b || b>'9') (\(_, b) -> getChar >>= return . (b,)) (undefined, '\0')
                                         let rest = do
                                              c <- catchIOError (hLookAhead stdin) $ \e -> if isEOFError e then return '\0' else ioError e
                                              let a = '0'<=c && c<='9'
                                              if a
                                                  then getChar >> liftM (c:) rest
                                                  else return ""
                                         str <- rest
                                         return tf { storages = reStorP $ push (if' (m=='-') negate id $ read (h:str)) stor }
                                     | batchim == ㅎ -> do
                                         a <- (case wsIgnoreLevel of
                                                 ALL -> during (liftM2 (||) isSpace isEndOfLine)
                                                 WHITESPACE -> during isSpace
                                                 LINE_BREAK -> during isEndOfLine
                                                 DO_NOT_IGNORE -> id) getChar
                                         let b = fromIntegral $ ord a
                                         return tf { storages = reStorP $ push b stor }
                                     | otherwise -> return tf { storages = reStorP $ push (fromIntegral v) stor }
                            )
                          ]
