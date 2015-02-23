module Language where

-- The possible makeup of the Expression language's expressions.
data Exp = EAdd Exp Exp |
           ESub Exp Exp |
           EMul Exp Exp |
           EDiv Exp Exp |
           ENum Int

instance Show Exp where
  show (EAdd e1 e2) =
    "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (ESub e1 e2) =
    "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (EMul e1 e2) =
    "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (EDiv e1 e2) =
    "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
  show (ENum x) =
    show x

-- The possible makeup of the Stack language's expressions.
data SML = SPush Int SML |
           SAdd SML |
           SSub SML |
           SMul SML |
           SDiv SML |
           SDone

instance Show SML where
  show (SPush x s) =
    "push " ++ show x ++ "\n" ++ show s
  show (SAdd s) =
    "add\n" ++ show s
  show (SSub s) =
    "sub\n" ++ show s
  show (SMul s) =
    "mul\n" ++ show s
  show (SDiv s) =
    "div\n" ++ show s
  show SDone =
    "done"

-- Converts an Exp to its equivalent SML representation.
toSML :: Exp -> SML
toSML e =
  let f (ENum x) rest =
        SPush x rest
      f (EAdd e1 e2) rest =
        f e2 (f e1 (SAdd rest))
        --f e1 (f e2 (SAdd rest))
      f (ESub e1 e2) rest =
        f e2 (f e1 (SSub rest))
        --f e1 (f e2 (SSub rest))
      f (EMul e1 e2) rest =
        f e2 (f e1 (SMul rest))
        --f e1 (f e2 (SMul rest))
      f (EDiv e1 e2) rest =
        f e2 (f e1 (SDiv rest))
        --f e1 (f e2 (SDiv rest))
  in f e SDone
