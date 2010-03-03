module DarcsDen.Validate where

import Hack (Env)
import qualified Data.Map as M

import DarcsDen.HackUtils


data Valid = Predicate String (String -> Bool) String
           | PredicateOp String String (String -> String -> Bool) String
           | Not Valid
           | Or Valid Valid
           | And Valid Valid
           | If Valid (Result -> Valid)
           | IOPred (IO Bool) String

data Result = Invalid [Valid]
            | OK (M.Map String String)


-- Explain a validation
explain :: Valid -> String
explain (Predicate a _ e) = a ++ " must " ++ e
explain (PredicateOp a b _ e) = a ++ " and " ++ b ++ " must " ++ e
explain (Not v) = "not: " ++ explain v
explain (Or v x) = explain v ++ " or " ++ explain x
explain (And v x) = explain v ++ " and " ++ explain x
explain (If v x) = "if (" ++ explain v ++ ")"
explain (IOPred _ e) = e

-- Helper
ok :: [(String, String)] -> Result
ok = OK . M.fromList

-- Verify a validation
verify :: Env -> Valid -> IO Result
verify e v@(Predicate a p _) = case getInput a e of
                                 Just x | p x -> return $ ok [(a, x)]
                                 _ -> return $ Invalid [v]
verify e v@(PredicateOp a b p _) = case (getInput a e,  getInput b e) of
                                     (Just x, Just y) | p x y -> return $ ok [(a, x), (b, y)]
                                     _ -> return $ Invalid [v]
verify e v@(Not t) = do t <- verify e t
                        case t of
                          Invalid _ -> return (OK M.empty)
                          OK _ -> return (Invalid [v])
verify e v@(Or a b) = do x <- verify e a
                         case x of
                           Invalid _ -> return x
                           OK _ -> verify e b
verify e v@(And a b) = do x <- verify e a
                          y <- verify e b
                          case (x, y) of
                            (OK ra, OK rb) -> return $ OK (ra `M.union` rb)
                            (Invalid fa, Invalid fb) -> return $ Invalid (fa ++ fb)
verify e v@(If a b) = do x <- verify e a
                         case x of
                           OK r -> verify e (b x)
                           _ -> return x
verify e v@(IOPred p _) = do r <- p
                             if r
                               then return $ OK M.empty
                               else return $ Invalid [v]

-- Check a bunch of validations
check :: Env -> [Valid] -> IO Result
check e ts = check' e ts (OK M.empty)
             where
               check' e [] acc = return acc
               check' e (t:ts) (Invalid is)
                   = do v <- verify e t
                        case v of
                          Invalid i -> check' e ts (Invalid (is ++ i))
                          _ -> check' e ts (Invalid is)
               check' e (t:ts) (OK vs)
                   = do v <- verify e t
                        case v of
                          Invalid f -> check' e ts v
                          OK r -> check' e ts (OK (M.union vs r))

-- Validators
nonEmpty :: String -> Valid
nonEmpty a = Predicate a (\x -> (x /= "")) "not be empty"

equal :: String -> String -> Valid
equal a b = PredicateOp a b (==) "be the same"

validate :: Env -> [Valid] -> (Result -> IO a) -> (Result -> IO a) -> IO a
validate e ts p f = do c <- check e ts
                       case c of
                         v@(OK _) -> p v
                         fails -> f fails

predicate :: String -> (String -> Bool) -> String -> Valid
predicate = Predicate

when :: Valid -> (Result -> Valid) -> Valid
when = If

io :: IO Bool -> String -> Valid
io = IOPred

