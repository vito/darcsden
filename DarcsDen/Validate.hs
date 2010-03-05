module DarcsDen.Validate where

import Data.Either (lefts)
import Hack (Env)
import qualified Data.Map as M

import DarcsDen.HackUtils


data Valid = Predicate String (String -> Bool) String
           | PredicateOp String String (String -> String -> Bool) String
           | Not Valid
           | Or Valid Valid
           | And Valid Valid
           | If Valid (OK -> Valid)
           | IOPred String (IO Bool)

data Invalid = Invalid [Valid]
data OK = OK (M.Map String String)

type Result = Either Invalid OK

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

-- Verify a validation
verify :: Env -> Valid -> IO Result
verify e v@(Predicate a p _) = case getInput a e of
                                 Just x | p x -> return $ ok [(a, x)]
                                 _ -> return $ invalid [v]
verify e v@(PredicateOp a b p _) = case (getInput a e,  getInput b e) of
                                     (Just x, Just y) | p x y -> return $ ok [(a, x), (b, y)]
                                     _ -> return $ invalid [v]
verify e v@(Not t) = do r <- verify e t
                        return (either (const (ok [])) (const (invalid [v])) r)
verify e (Or a b) = do x <- verify e a
                       case x of
                         Left _ -> return x
                         Right _ -> verify e b
verify e (And a b) = do x <- verify e a
                        y <- verify e b
                        case [x, y] of
                          [Right (OK ra), Right (OK rb)] ->
                              return $ Right $ OK (ra `M.union` rb)
                          other ->
                              return $ invalid (concat . map (\(Invalid i) -> i) $ lefts other)
verify e (If a b) = do x <- verify e a
                       case x of
                         Right o -> verify e (b o)
                         _ -> return x
verify _ v@(IOPred _ p) = do r <- p
                             if r
                               then return $ ok []
                               else return $ invalid [v]

-- Check a bunch of validations
check :: Env -> [Valid] -> IO Result
check = check' (ok [])
    where
      check' acc _ [] = return acc
      check' (Left (Invalid is)) e (t:ts)
          = do v <- verify e t
               case v of
                 Left (Invalid i) -> check' (invalid (is ++ i)) e ts
                 _ -> check' (invalid is) e ts
      check' (Right (OK vs)) e (t:ts)
          = do v <- verify e t
               case v of
                 Right (OK r) -> check' (Right (OK (M.union vs r))) e ts
                 _ -> check' v e ts

-- Validators
nonEmpty :: String -> Valid
nonEmpty a = Predicate a (\x -> (x /= "")) "not be empty"

equal :: String -> String -> Valid
equal a b = PredicateOp a b (==) "be the same"

validate :: Env -> [Valid] -> (OK -> IO a) -> (Invalid -> IO a) -> IO a
validate e ts p f = check e ts >>= either f p

predicate :: String -> (String -> Bool) -> String -> Valid
predicate = Predicate

when :: Valid -> (OK -> Valid) -> Valid
when = If

io :: String -> IO Bool -> Valid
io = IOPred

