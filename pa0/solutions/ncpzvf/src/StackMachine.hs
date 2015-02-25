module StackMachine
( translate
) where

import Syntax

translate :: Exp -> String
translate s = trans s ++ "\ndone"

trans :: Exp -> String
trans (LitNum n)  = "push " ++ show n
trans (Add e1 e2) = trans e2 ++ "\n" ++ trans e1 ++ "\n" ++ "add"
trans (Sub e1 e2) = trans e2 ++ "\n" ++ trans e1 ++ "\n" ++ "sub"
trans (Mul e1 e2) = trans e2 ++ "\n" ++ trans e1 ++ "\n" ++ "mul"
trans (Div e1 e2) = trans e2 ++ "\n" ++ trans e1 ++ "\n" ++ "div"
