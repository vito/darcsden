module DarcsDen.Debug
(module DarcsDen.Debug, module Debug.Trace)
where

import Debug.Trace


debugging :: Bool
debugging = False

debug :: (Show a, Show b) => b -> a -> a
debug s v
    | debugging = trace (show s ++ ": " ++ show v) v
    | otherwise = v

dump :: (Monad m, Show a) => a -> m ()
dump x
    | debugging = trace (show x) (return ())
    | otherwise = return ()

-- | trace (print on stdout at runtime) a showable expression
-- (for easily tracing in the middle of a complex expression)
strace :: Show a => a -> a
strace a = trace (show a) a
-- | labelled trace - like strace, with a label prepended
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a
-- | monadic trace - like strace, but works as a standalone line in a monad
mtrace :: (Monad m, Show a) => a -> m a
mtrace a = strace a `seq` return a
-- | trace an expression using a custom show function
tracewith :: (a -> String) -> a -> a
tracewith f e = trace (f e) e
