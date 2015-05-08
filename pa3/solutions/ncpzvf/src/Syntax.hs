module Syntax where

data Crate = Crate { items :: [Item] }
             deriving (Show, Eq)

data Item = FnDef     { funcId     :: Id
                      , params     :: [FnParam]
                      , returnType :: (Maybe Type)
                      , stmts      :: [Stmt]
                      }
          | EnumDef   { enumId  :: Id
                      , ctors   :: [EnumCtorDef]
                      }
          | StructDef { structId :: Id
                      , fields   :: [FieldDef]
                      }
          deriving (Show, Eq)

newtype Id = Id { idName :: String } deriving Eq

data FnParam     = FnParam     { paramPat  :: Pattern
                               , paramType :: Type
                               } deriving Eq
data EnumCtorDef = EnumCtorDef { ctorId :: Id
                               , enumTypes :: [Type]
                               } deriving Eq
data FieldDef    = FieldDef    { fieldId :: Id
                               , fieldType :: Type
                               } deriving Eq

data Type = TypeRef Type
          | TypeRefMut Type
          | TypeMut Type
          | TypeArr ArrDef
          | TypeBox Type
          | TypeI32
          | TypeU8
          | TypeBool
          | TypeUnit
          | TypeId  { typeId :: Id }
          | TypeNone
          deriving Eq

data ArrDef = ArrDef Type (Maybe Exp) deriving Eq

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
             deriving Eq

data FieldPat = FieldPat Id Pattern deriving Eq

data Stmt = Let Pattern (Maybe Type) (Maybe Exp)
          | Return Exp
          | Expression Exp
          | ExpressionSansSemi Exp
          deriving (Show, Eq)

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
         deriving (Show, Eq)

data FieldInit = FieldInit Id Exp deriving Eq
data MatchArm  = MatchArm [Pattern] [Stmt] deriving (Show, Eq)

data BExp = Or Exp Exp
          | And Exp Exp
          | Eq Exp Exp
          | NEq Exp Exp
          | Lt Exp Exp
          | Gt Exp Exp
          | LEq Exp Exp
          | GEq Exp Exp
          | Not Exp
          deriving (Show, Eq)

data NExp = Add Exp Exp
          | Sub Exp Exp
          | Mul Exp Exp
          | Div Exp Exp
          | Rem Exp Exp
          | Neg Exp
          deriving (Show, Eq)

data PExp = Deref Exp
          | AddrOf Exp
          | AddrOfMut Exp
          deriving (Show, Eq)

data AsgnType = Asgn
              | AsgnAdd
              | AsgnSub
              | AsgnMul
              | AsgnDiv
              | AsgnRem
              deriving Eq

data Lit = LitChar
         | LitStr
         | LitDec Int
         | LitTrue
         | LitFalse
         deriving Eq

instance Show Id where
    show (Id n) = "\n(id\n(" ++ n ++ "))"

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
    show (ArrDef t (Just (Literal l))) = show t ++ "\n(lit-dec)"
    show (ArrDef t Nothing)           = show t

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

instance Show FieldInit where
    showList ls _        = "\n(field-inits" ++ concatMap show ls ++ ")"
    show (FieldInit n e) = "\n(field-init" ++ show n ++ show e ++ ")"

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

instance Show AsgnType where
    show Asgn     = "assign"
    show AsgnAdd  = "assign-add"
    show AsgnSub  = "assign-sub"
    show AsgnMul  = "assign-mul"
    show AsgnDiv  = "assign-div"
    show AsgnRem  = "assign-rem"

instance Show Lit where
    show LitChar    = "\n(lit-char)"
    show LitStr     = "\n(lit-str)"
    show (LitDec d) = "\n(lit-dec)"
    show LitTrue    = "\n(true)"
    show LitFalse   = "\n(false)"

instance Show FieldPat where
    showList ls _           = "\n(pat-fields" ++ concatMap show ls ++ ")"
    show (FieldPat n p)     = "\n(pat-field" ++ show n ++ show p ++ ")"
