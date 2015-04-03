module Syntax where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

data Crate = Crate [Item]

data Item = FnDef Id [FnParam] (Maybe Type) [Stmt]
          | EnumDef Id [EnumCtorDef]
          | StructDef Id [FieldDef]

newtype Id = Id String

data FnParam     = FnParam Pattern Type
data EnumCtorDef = EnumCtorDef Id [Type]
data FieldDef    = FieldDef Id Type

data Type = TypeRef Type
          | TypeRefMut Type
          | TypeArr ArrDef
          | TypeBox Type
          | TypeI32
          | TypeU8
          | TypeBool
          | TypeUnit
          | TypeId Id
          | TypeNone

data ArrDef = ArrDef Type (Maybe Exp)

data Pattern = PatArr [Pattern]
             | PatStruct Id [FieldPat]
             | PatEnum Id Id [Pattern]
             | PatUnit
             | PatWild
             | PatDeref Pattern
             | PatId Id
             | PatRefId Id
             | PatRefMutId Id
             | PatMutId Id
             | PatLit Lit
             | PatField Id Pattern

data FieldPat = FieldPat Id Pattern

data Stmt = Let Pattern (Maybe Type) (Maybe Exp)
          | Return Exp
          | Expression Exp

data Exp = IdExp Id
         | EnumCtor Id Id [Exp]
         | Struct Id [FieldInit]
         | FieldLookup Exp Id
         | ArrIndex Exp Exp
         | FnCall Id [Exp]
         | Arr [Exp]
         | Assign AsgnType Exp Exp
         | BoolExp BExp
         | NumExp NExp
         | BoxNew [Exp]
         | Match Exp [MatchArm]
         | If Exp [Stmt] [Stmt]
         | While Exp [Stmt]
         | Loop [Stmt]
         | PointerExp PExp
         | Literal Lit
         | Unit

data FieldInit = FieldInit Id Exp
data MatchArm  = MatchArm [Pattern] [Stmt]

data BExp = Or Exp Exp
          | And Exp Exp
          | Eq Exp Exp
          | NEq Exp Exp
          | Lt Exp Exp
          | Gt Exp Exp
          | LEq Exp Exp
          | GEq Exp Exp
          | Not Exp

data NExp = Add Exp Exp
          | Sub Exp Exp
          | Mul Exp Exp
          | Div Exp Exp
          | Rem Exp Exp
          | Neg Exp

data PExp = Deref Exp
          | AddrOf Exp
          | AddrOfMut Exp

data AsgnType = Asgn
              | AsgnAdd
              | AsgnSub
              | AsgnMul
              | AsgnDiv
              | AsgnRem

data Lit = LitChar
         | LitStr
         | LitDec
         | LitTrue
         | LitFalse

showMaybe :: (Show a) => Maybe a -> String
showMaybe = fromMaybe "" . fmap show

instance Show Crate where
    show (Crate items) = "(crate" ++ show items ++ ")"

instance Show Item where
    showList ls _                 = "\n(items" ++ concatMap show ls ++ ")"
    show (FnDef n ps mt ss)       = "\n(fn-def" ++ show n ++ show ps ++
                                    showMaybe mt ++ show ss ++ ")"
    show (EnumDef n ctors)        = "\n(enum-def" ++ show n ++ show ctors ++ ")"
    show (StructDef n fs)         = "\n(struct-def" ++ show n ++ show fs ++ ")"

instance Show Id where
    show (Id n) = "\n(id\n(" ++ n ++ "))"

instance Show FnParam where
    showList [] _      = ""
    showList ls _      = "\n(fn-params" ++ concatMap show ls ++ ")"
    show (FnParam p t) = "\n(fn-param" ++ show p ++ show t ++ ")"

instance Show EnumCtorDef where
    show (EnumCtorDef n []) = "\n(enum-ctor-def" ++ show n ++ ")"
    show (EnumCtorDef n ts) = "\n(enum-ctor-def" ++ show n ++
                              "\n(enum-ctor-params" ++ show ts ++ "))"
    showList ls _           = "\n(enum-ctor-defs" ++ concatMap show ls ++ ")"

instance Show FieldDef where
    show (FieldDef n t) = "\n(field-def" ++ show n ++ show t ++ ")"
    showList ls _       = "\n(field-defs" ++ concatMap show ls ++ ")"

instance Show Type where
    showList [] _       = ""
    showList ls _       = concatMap show ls
    show (TypeRef t)    = "\n(type-ref" ++ show t ++ ")"
    show (TypeRefMut t) = "\n(type-ref-mut" ++ show t ++ ")"
    show (TypeArr d)    = "\n(type-arr" ++ show d ++ ")"
    show TypeI32        = "\n(type-i32)"
    show TypeU8         = "\n(type-u8)"
    show TypeBool       = "\n(type-bool)"
    show TypeUnit       = "\n(type-unit)"
    show (TypeBox t)    = "\n(type-box" ++ show t ++ ")"
    show (TypeId n)     = show n
    show TypeNone       = ""

instance Show ArrDef where
    show (ArrDef t me) = show t ++ showMaybe me

instance Show Pattern where
    showList ls _           = "\n(pats" ++ concatMap show ls ++ ")"
    show (PatArr ps)        = "\n(pat-arr\n(pat-arr-elems" ++ concatMap show ps ++ "))"
    show (PatStruct n fs)   = "\n(pat-struct" ++ show n ++ show fs ++ ")"
    show (PatEnum n1 n2 []) = "\n(pat-enum\n(enum-ctor" ++ show n1 ++ show n2 ++ "))"
    show (PatEnum n1 n2 ps) = "\n(pat-enum\n(enum-ctor" ++ show n1 ++ show n2 ++
                              ")\n(pat-enum-ctor-params" ++ concatMap show ps ++ "))"
    show PatUnit            = "\n(pat-unit)"
    show (PatDeref p)       = "\n(pat-deref" ++ show p ++ ")"
    show (PatId n)          = "\n(pat-id" ++ show n ++ ")"
    show (PatRefId n)       = "\n(pat-ref-id" ++ show n ++ ")"
    show (PatRefMutId n)    = "\n(pat-ref-mut-id" ++ show n ++ ")"
    show (PatMutId n)       = "\n(pat-mut-id" ++ show n ++ ")"
    show (PatLit l)         = "\n(pat-lit" ++ show l ++ ")"
    show PatWild            = "\n(pat-wild)"

instance Show FieldPat where
    showList ls _           = "\n(pat-fields" ++ concatMap show ls ++ ")"
    show (FieldPat n p)     = "\n(pat-field" ++ show n ++ show p ++ ")"

instance Show Stmt where
    showList ls _       = "\n(block" ++ concatMap show ls ++ ")"
    show (Let p mt me)  = "\n(let" ++ show p ++ showMaybe mt ++ showMaybe me ++ ")"
    show (Return e)     = "\n(return" ++ show e ++ ")"
    show (Expression e) = show e

instance Show Exp where
    showList [] _            = ""
    showList ls _            = "\n(exprs" ++ concatMap show ls ++ ")"
    show (IdExp n)           = show n
    show (EnumCtor n1 n2 es) = "\n(enum\n(enum-ctor" ++ show n1 ++ show n2 ++
                               ")" ++ show es ++ ")"
    show (Struct n fs)       = "\n(struct" ++ show n ++ show fs ++ ")"
    show (FieldLookup e n)   = "\n(field-lookup" ++ show e ++ show n ++ ")"
    show (ArrIndex e1 e2)    = "\n(arr-index" ++ show e1 ++ show e2 ++ ")"
    show (FnCall n [])       = "\n(fn-call" ++ show n ++ "\n())"
    show (FnCall n es)       = "\n(fn-call" ++ show n ++ show es ++ ")"
    show (Arr es)            = "\n(arr" ++ show es ++ ")"
    show (Assign t e1 e2)    = "\n(" ++ show t ++ show e1 ++ show e2 ++ ")"
    show (BoolExp e)         = show e
    show (NumExp e)          = show e
    show (BoxNew es)         = "\n(box-new" ++ show es ++ ")"
    show (Match e ms)        = "\n(match" ++ show e ++ show ms ++ ")"
    show (If e ts [])        = "\n(if" ++ show e ++ show ts ++ ")"
    show (If e ts fs)        = "\n(if" ++ show e ++ show ts ++ show fs ++ ")"
    show (While e ss)        = "\n(while" ++ show e ++ show ss ++ ")"
    show (Loop ss)           = "\n(loop" ++ show ss ++ ")"
    show (PointerExp e)      = show e
    show (Literal l)         = show l
    show Unit                = "\n(unit)"

instance Show FieldInit where
    showList ls _        = "\n(field-inits" ++ concatMap show ls ++ ")"
    show (FieldInit n e) = "\n(field-init" ++ show n ++ show e ++ ")"

instance Show MatchArm where
    showList ls _         = "\n(match-arms" ++ concatMap show ls ++ ")"
    show (MatchArm ps ss) = "\n(match-arm" ++ show ps ++ show ss ++ ")"

instance Show BExp where
    show (Or e1 e2)  = "\n(or" ++ show e1 ++ show e2 ++ ")"
    show (And e1 e2) = "\n(and" ++ show e1 ++ show e2 ++ ")"
    show (Eq e1 e2)  = "\n(eq" ++ show e1 ++ show e2 ++ ")"
    show (NEq e1 e2) = "\n(neq" ++ show e1 ++ show e2 ++ ")"
    show (Lt e1 e2)  = "\n(lt" ++ show e1 ++ show e2 ++ ")"
    show (Gt e1 e2)  = "\n(gt" ++ show e1 ++ show e2 ++ ")"
    show (LEq e1 e2) = "\n(leq" ++ show e1 ++ show e2 ++ ")"
    show (GEq e1 e2) = "\n(geq" ++ show e1 ++ show e2 ++ ")"
    show (Not e)     = "\n(not" ++ show e ++ ")"

instance Show NExp where
    show (Add e1 e2) = "\n(add" ++ show e1 ++ show e2 ++ ")"
    show (Sub e1 e2) = "\n(sub" ++ show e1 ++ show e2 ++ ")"
    show (Mul e1 e2) = "\n(mul" ++ show e1 ++ show e2 ++ ")"
    show (Div e1 e2) = "\n(div" ++ show e1 ++ show e2 ++ ")"
    show (Rem e1 e2) = "\n(rem" ++ show e1 ++ show e2 ++ ")"
    show (Neg e)     = "\n(neg" ++ show e ++ ")"

instance Show PExp where
    show (Deref e)     = "\n(deref" ++ show e ++ ")"
    show (AddrOf e)    = "\n(addr-of" ++ show e ++ ")"
    show (AddrOfMut e) = "\n(addr-of-mut" ++ show e ++ ")"

instance Show AsgnType where
    show Asgn     = "assign"
    show AsgnAdd  = "assign-add"
    show AsgnSub  = "assign-sub"
    show AsgnMul  = "assign-mul"
    show AsgnDiv  = "assign-div"
    show AsgnRem  = "assign-rem"

instance Show Lit where
    show LitChar  = "\n(lit-char)"
    show LitStr   = "\n(lit-str)"
    show LitDec   = "\n(lit-dec)"
    show LitTrue  = "\n(true)"
    show LitFalse = "\n(false)"
