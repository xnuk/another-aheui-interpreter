module Util (if', if'', during, during1) where

if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ a = a

if'' :: (b -> Bool) -> (b -> a) -> (b -> a) -> b -> a
if'' f a b x = if' (f x) a b x

during :: Monad m => (a -> Bool) -> m a -> m a
during f m = do
    a <- m
    if f a
       then during f m
       else return a

during1 :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
during1 f mf a = do
    a' <- mf a
    if f a'
       then during1 f mf a'
       else return a'
