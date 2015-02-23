module Parser (parse) where

import Data.Either
import Data.Maybe
import Language
import Lexer

-- A shorthand form for the [EAdd, ESub, ...] functions.
type ExpCreator = Exp -> Exp -> Exp

-- Converts a symbol into its according ExpCreator.
symToExpCreator :: Sym -> ExpCreator
symToExpCreator Plus     = EAdd
symToExpCreator Minus    = ESub
symToExpCreator Multiply = EMul
symToExpCreator Divide   = EDiv

-- Pops until an LParen.
-- Returns (the popped items, the remaining stack without the LParen).
popUntilLParen :: [Either Token ExpCreator]
                  -> ([Either Token ExpCreator], [Either Token ExpCreator])
popUntilLParen stack =
  let inner (Left LParen : ss) out = (out, ss)
      inner (s : ss) out           = inner ss (s : out)
      inner [] _                   = error "Expected an ending '('."
  in inner stack []

-- Converts a set of ot okens into its RPN Expression form.
tokensToRPN :: [Token] -> [Either Exp ExpCreator]
tokensToRPN tokens =
  let inner [] out ops              = out ++ map Right (rights ops)
      inner (Number x : ts) out ops = inner ts (Left (ENum x) : out) ops
      inner (Symbol s : ts) out ops'@(op : ops) =
        case op of
         Left _  -> inner ts out (Right (symToExpCreator s) : ops')
         Right e -> inner ts (Right e : out) (Right (symToExpCreator s) : ops)
      inner (Symbol s : ts) out [] =
        inner ts out [Right (symToExpCreator s)]
      inner (LParen : ts)   out ops =
        inner ts out (Left LParen : ops)
      inner (RParen : ts)   out ops =
        let (pops, rem') = popUntilLParen ops
        in inner ts (map Right (rights pops) ++ out) rem'
  in inner tokens [] []

-- Converts an expression in RPN form to a standard, single expression (tree).
rpnToExp :: [Either Exp ExpCreator] -> Exp
rpnToExp rpn =
  let inner (Right op : rpn') head' =
        if isNothing head' then
          let (r, i1) = inner rpn' Nothing
              rpn''   = drop i1 rpn'
              (l, i2) = inner rpn'' Nothing
          in case r of
              Just r' ->
                case l of
                 Just l' -> (Just (op l' r'), i1 + i2 + 1)
                 Nothing -> error "Invalid expression."
              Nothing -> error "Invalid Expression"
        else
          error "Expected expression to terminate."
      inner (Left e : _) head' =
        if isNothing head' then
          (Just e, 1)
        else
          error $ "Expected another operation [+, -, *, /], but got: " ++ show e
      inner [] _ = (Nothing, 0)
  in case inner rpn Nothing of
      (Just e, _)  -> e
      (Nothing, _) -> error "Invalid expression."

-- Takes a tokenized expression and translates it into an actual Expression.
parse :: [Token] -> Exp
parse = rpnToExp . tokensToRPN
