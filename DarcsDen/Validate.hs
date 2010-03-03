module DarcsDen.Validate where

import Hack (Env)
import qualified Data.Map as M

import DarcsDen.HackUtils


data Valid = Predicate String (String -> Bool) String
           | PredicateOp String String (String -> String -> Bool) String
           | Not Valid
           | Or Valid Valid
           | And Valid Valid
           | If Valid Valid

data Result = Invalid [Valid]
            | OK (M.Map String String)

explain :: Valid -> String
explain (Predicate a _ e) = show a ++ " must " ++ e
explain (PredicateOp a b _ e) = show a ++ " and " ++ show b ++ " must " ++ e
explain (Not v) = "not: " ++ explain v
explain (Or v x) = explain v ++ " or " ++ explain x
explain (And v x) = explain v ++ " and " ++ explain x
explain (If v x) = "if (" ++ explain v ++ ") then (" ++ explain x ++ ")"

verify :: Env -> Valid -> Maybe [(String, String)]
verify e (Predicate a p _) = getInput a e >>= (\x -> if p x then Just [(a, x)] else Nothing)
verify e (PredicateOp a b p _) = do x <- getInput a e
                                    y <- getInput b e
                                    if p x y
                                      then return [(a, x), (b, y)]
                                      else Nothing
verify e (Not v) = case verify e v of
                     Nothing -> Just []
                     Just x -> Nothing
verify e (Or a b) = let x = verify e a
                        y = verify e b
                    in if x /= Nothing
                         then x
                         else y
verify e (And a b) = do x <- verify e a
                        y <- verify e b
                        return (x ++ y)
verify e (If a b) = do x <- verify e a
                       y <- verify e b
                       return y

check :: Env -> [Valid] -> Result
check e ts = check' e ts (OK M.empty)
             where
               check' e [] acc = acc
               check' e ((If v x):ts) (Invalid is) | verify e v == Nothing = check' e ts (Invalid (is ++ [v]))
                                                   | otherwise = check' e ts (Invalid is)
               check' e ((If v x):ts) (OK vs) = case verify e v of
                                                  Nothing -> check' e ts (Invalid [v])
                                                  Just y -> check' e (x:ts) (OK (M.union vs (M.fromList y)))
               check' e (t:ts) (Invalid is) | verify e t == Nothing = check' e ts (Invalid (is ++ [t]))
                                            | otherwise = check' e ts (Invalid is)
               check' e (t:ts) (OK vs) = case verify e t of
                                           Nothing -> check' e ts (Invalid [t])
                                           Just y -> check' e ts (OK (M.union vs (M.fromList y)))

nonEmpty :: String -> Valid
nonEmpty a = Predicate a (\x -> (x /= "")) "not be empty"

equal :: String -> String -> Valid
equal a b = PredicateOp a b (==) "be the same"

validate :: Env -> [Valid] -> (Result -> b) -> (Result -> b) -> b
validate e ts p f = case check e ts of
                      v@(OK _) -> p v
                      fails -> f fails

predicate :: String -> (String -> Bool) -> String -> Valid
predicate = Predicate