module DarcsDen.Validate where

import Control.Monad.Trans
import Data.Either (lefts)
import Snap.Types
import qualified Data.Map as M

import DarcsDen.State.Session
import DarcsDen.Util (fromBS, toBS)


data Valid = Predicate String (String -> Bool) String
           | PredicateOp String String (String -> String -> Bool) String
           | Not Valid
           | Or Valid Valid
           | And Valid Valid
           | If Valid (OK -> Valid)
           | IOPred String (IO Bool)

data Invalid = Invalid [Valid]
               deriving Show
data OK = OK (M.Map String String)
          deriving Show

type Result = Either Invalid OK

instance Show Valid where
    show = explain

-- Explain a validation
explain :: Valid -> String
explain (Predicate a _ e) = a ++ " must " ++ e
explain (PredicateOp a b _ e) = a ++ " and " ++ b ++ " must " ++ e
explain (Not v) = "not: " ++ explain v
explain (Or v x) = explain v ++ " or " ++ explain x
explain (And v x) = explain v ++ " and " ++ explain x
explain (If v _) = "if (" ++ explain v ++ ") (...)"
explain (IOPred e _) = e

-- Helpers
ok :: [(String, String)] -> Result
ok = Right . OK . M.fromList

invalid :: [Valid] -> Result
invalid = Left . Invalid

param :: String -> Snap (Maybe String)
param a = do
    p <- getParam (toBS a)
    return (fmap fromBS p)

-- Verify a validation
verify :: Valid -> Snap Result
verify v@(Predicate a p _) = do
    mp <- param a
    case mp of
        Just x | p x -> return $ ok [(a, x)]
        _ -> return $ invalid [v]
verify v@(PredicateOp a b p _) = do
    ma <- param a
    mb <- param b
    case (ma, mb) of
        (Just x, Just y) | p x y -> return $ ok [(a, x), (b, y)]
        _ -> return $ invalid [v]
verify v@(Not t) = do
    r <- verify t
    return (either (const (ok [])) (const (invalid [v])) r)
verify (Or a b) = do
    x <- verify a
    case x of
        Left _ -> verify b
        Right _ -> return x
verify (And a b) = do
    x <- verify a
    y <- verify b
    case [x, y] of
        [Right (OK ra), Right (OK rb)] ->
            return $ Right $ OK (ra `M.union` rb)
        other ->
            return $ invalid (concatMap (\(Invalid i) -> i) $ lefts other)
verify (If a b) = do
    x <- verify a
    case x of
        Right o@(OK r) -> do
            t <- verify (b o)
            case t of
                Left _ -> return t
                Right (OK ts) -> return $ Right $ OK (r `M.union` ts)
        _ -> return x
verify v@(IOPred _ p) = do
    r <- liftIO p
    if r
        then return $ ok []
        else return $ invalid [v]

-- Check a bunch of validations
check :: [Valid] -> Snap Result
check = check' (ok [])
    where
      check' acc [] = return acc
      check' (Left (Invalid is)) (t:ts)
          = do v <- verify t
               case v of
                 Left (Invalid i) -> check' (invalid (is ++ i)) ts
                 _ -> check' (invalid is) ts
      check' (Right (OK vs)) (t:ts)
          = do v <- verify t
               case v of
                 Right (OK r) -> check' (Right (OK (vs `M.union` r))) ts
                 _ -> check' v ts

-- Validators
nonEmpty :: String -> Valid
nonEmpty a = Predicate a (/= "") "not be empty"

isEmpty :: String -> Valid
isEmpty a = Predicate a (== "") "be empty"

equal :: String -> String -> Valid
equal a b = PredicateOp a b (==) "be the same"

validate :: [Valid] -> (OK -> Snap a) -> (Invalid -> Snap a) -> Snap a
validate ts p f = check ts >>= either f p

predicate :: String -> (String -> Bool) -> String -> Valid
predicate = Predicate

iff :: Valid -> (OK -> Valid) -> Valid
iff = If

onlyIf :: Valid -> Valid -> Valid
onlyIf p v = Or (Not p) v

io :: String -> IO Bool -> Valid
io = IOPred

-- Notifications
notify :: MonadIO m => (String -> Notification) -> Session -> [Valid] -> m Session
notify n s vs = do
    mapM_ (flip notice s) ns
    return (s { sNotifications = sNotifications s ++ ns })
  where
    ns = map (n . explain) vs
