module Typer where

import Syntax
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.List (find)
import Control.Applicative ((<$>))
import qualified Data.Map as Map

data Status = Error
           | Ok
           | SType PrettyType
           deriving Eq

newtype PrettyType = PrettyType Type

instance Show Status where
    show Error          = "ERROR!"
    show Ok             = "ok!"
    show (SType t)      = show t

instance Show PrettyType where
    -- TODO: this is absolutely awful
    show (PrettyType (TypeArr (ArrDef t (Just (Literal (LitDec d)))))) =
        "["  ++ show (makePretty t) ++ ";" ++ show d ++ "]"
    show (PrettyType (TypeArr (ArrDef t Nothing))) = "[" ++ show (makePretty t) ++ "]"
    show (PrettyType (TypeMut t)) = "mut " ++ show (makePretty t)
    show (PrettyType (TypeRef t)) = "&" ++ show (makePretty t)
    show (PrettyType TypeI32)     = "i32"
    show (PrettyType TypeU8)      = "u8"
    show (PrettyType TypeBool)    = "bool"
    show (PrettyType TypeUnit)    = "()"
    show (PrettyType (TypeBox t)) = "Box<" ++ show (makePretty t) ++ ">"
    show (PrettyType (TypeId n))  = idName n
    show (PrettyType TypeNone)    = ""

instance Eq PrettyType where
    PrettyType (TypeMut t1) == PrettyType (TypeMut t2) = (t1 == t2)
    PrettyType (TypeMut t1) == PrettyType t2 = (t1 == t2)
    PrettyType t1 == PrettyType (TypeMut t2) = (t1 == t2)
    PrettyType t1 == PrettyType t2 = t1 == t2

type Env    = Map.Map String Status
type Defs   = Map.Map String Item

data Result = Result { env    :: Env
                     , defs   :: Defs
                     , status :: Status
                     , output :: String
                     }

fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z

getId :: Pattern -> String
getId (PatId (Id n)) = n
getId (PatMutId (Id n)) = n

contains :: Status -> Env -> Bool
contains s = elem s . Map.elems

showMaybe :: (Show a) => Maybe a -> String
showMaybe = fromMaybe "" . fmap show

makePretty :: Type -> Status
makePretty = SType . PrettyType

getType (SType (PrettyType t)) = t

showOutputs :: [Result] -> String
showOutputs = concatMap output

envLookup :: String -> Env -> Status
envLookup s = fromMaybe Error . Map.lookup s

lookupType :: String -> String -> Defs -> Status
lookupType structName fieldName ds
    | isStruct  = fromMaybe Error (makePretty <$> fieldType')
    | otherwise = Error
    where
        mStruct    = Map.lookup structName ds
        isStruct   = isJust mStruct
        struct     = fromJust mStruct
        field      = find (\x -> idName (fieldId x) == fieldName) (fields struct)
        fieldType' = fieldType <$> field

isValidStruct :: Exp -> Env -> Defs -> Bool
isValidStruct (Struct n fs) e ds
    | isStruct  = fieldDefTypes == fieldTypes
    | otherwise = False
    where
        mStructDef    = Map.lookup (idName n) ds
        isStruct      = isJust mStructDef
        structDef     = fromJust mStructDef
        fieldDefTypes = map fieldType $ fields structDef
        fieldResults  = map (expRes e ds) $ map getFieldExp fs
        fieldTypes    = map (getType . status) fieldResults
        getFieldExp (FieldInit _ e) = e

isValidFnCall :: Exp -> Env -> Defs -> Bool
isValidFnCall (FnCall n es) e ds
    | isFunc    = paramTypes == funcParamTypes
    | otherwise = False
    where
        mFuncDef       = Map.lookup (idName n) ds
        isFunc         = isJust mFuncDef
        funcDef        = fromJust mFuncDef
        funcParamTypes = map paramType $ params funcDef
        paramResults   = map (expRes e ds) es
        paramTypes     = map (getType . status) paramResults

typeRust :: Crate -> String
typeRust = output . crateRes Map.empty Map.empty

crateRes :: Env -> Defs -> Crate -> Result
crateRes e ds (Crate items) =
    let itemResults' = itemResults e ds items
        isError      = any (== Error) $ map status itemResults'
        status'      = if isError then Error else Ok
        output'      = "(crate:" ++ show status' ++ showItems itemResults' ++ ")"
    in  Result e ds status' output'

itemResults e ds []     = []
itemResults e ds (i:is) =
    let itemRes' = itemRes e ds i
    in  itemRes' : itemResults (env itemRes') (defs itemRes') is

showItems es = "\n(items" ++ showOutputs es ++ ")"

itemRes :: Env -> Defs -> Item -> Result
itemRes e ds item@(FnDef n ps mt ss) =
    let env'    = Map.insert (idName n) (makePretty fnType) e
        defs'   = Map.insert (idName n) item ds
        bEnv    = insertFnParams env' ps
        bRes    = blockRes bEnv defs' ss
        bRet    = blockReturn bEnv defs' ss
        isError =  status bRes == Error
                || bRet /= makePretty fnType
        fnType  = fromMaybe TypeUnit mt
        status' = if isError then Error else Ok
        output' = "\n(fn-def:" ++ show status' ++ show n ++ show ps ++
                  show fnType ++ output bRes ++ ")"
    in  Result env' defs' status' output'
itemRes e ds item@(EnumDef n ctors) =
    let names    = map (idName . ctorId) ctors
        enumType = makePretty (TypeId n)
        env'     = foldl (\e n -> Map.insert n enumType e) e names
        defs'    = Map.insert (idName n) item ds
        output'  = "\n(enum-def:ok!" ++ show n ++ show ctors ++ ")"
    in Result env' defs' Ok output'
itemRes e ds item@(StructDef n fs) =
    let structType = makePretty $ TypeId n
        env'       = Map.insert (idName n) structType e
        defs'      = Map.insert (idName n) item ds
        output'    = "\n(struct-def:ok!" ++ show n ++ show fs ++ ")"
    in  Result env' defs' Ok output'

insertFnParams :: Env -> [FnParam] -> Env
insertFnParams e []                 = e
insertFnParams e ((FnParam p t):ps) =
    let name    = getId p
        status' = makePretty t
        e'      = Map.insert name status' e
    in  insertFnParams e' ps

blockRes :: Env -> Defs -> [Stmt] -> Result
blockRes e ds ss =
    let stmtResults'  = stmtResults e ds ss
        isError       = any (== Error) $ map status stmtResults'
        env'          = foldl (flip Map.union) e $ map env stmtResults'
        status'       = if isError then Error else blockType env' ds ss
        unit          = stmtRes e ds (ExpressionSansSemi Unit)
        stmtResults'' = if endsWithSemi ss
                            then stmtResults' ++ [unit]
                            else stmtResults'
        output'       = "\n(block:" ++ show status' ++ showOutputs stmtResults'' ++ ")"
    in  Result env' ds status' output'

endsWithSemi :: [Stmt] -> Bool
endsWithSemi [] = True
endsWithSemi ss = case last ss of
                      ExpressionSansSemi exp -> False
                      _                      -> True

blockReturn :: Env -> Defs -> [Stmt] -> Status
blockReturn e ds []  = makePretty TypeUnit
blockReturn e ds [s] =
    case s of
        Return ex -> status $ expRes e ds ex
        stmt      -> blockType e ds [s]
blockReturn e ds (s:ss) =
    case s of
        Return ex -> status $ expRes e ds ex
        stmt      -> let sRes = stmtRes e ds stmt
                     in  blockReturn (env sRes) ds ss

blockType :: Env -> Defs -> [Stmt] -> Status
blockType e ds [] = makePretty TypeUnit
blockType e ds ss = case last ss of
                     ExpressionSansSemi exp -> status $ expRes e ds exp
                     _                      -> makePretty TypeUnit

stmtResults e ds []     = []
stmtResults e ds (s:ss) =
    let stmtRes' = stmtRes e ds s
    in  stmtRes' : stmtResults (env stmtRes') ds ss

isMut :: Pattern -> Bool
isMut (PatMutId _) = True
isMut _            = False

stmtRes :: Env -> Defs -> Stmt -> Result
stmtRes e ds (Let p mt me) =
    let expRes'   = expRes e ds <$> me
        status'   = if typ' == Error then Error else makePretty TypeUnit
        typ       = stmtType expRes' mt
        typ'      = if isMut p && typ /= Error
                        then makePretty (TypeMut $ getType typ)
                        else typ
        env'      = Map.insert (getId p) typ' e
        expString = if isJust expRes' then output (fromJust expRes') else ""
        output'   = "\n(let:" ++ show status' ++ show p ++ showMaybe mt ++ expString ++ ")"
    in  Result env' ds status' output'
stmtRes e ds (Return ex) =
    let expRes' = expRes e ds ex
        status' = makePretty TypeUnit
        output' = "\n(return:" ++ show status' ++ output expRes' ++ ")"
    in  Result e ds status' output'
stmtRes e ds (Expression ex) =
    let expRes'  = expRes e ds ex
        env'     = env expRes'
        isError  = status expRes' == Error
        status'  = if isError then Error else makePretty TypeUnit
        output'  = "\n(stmt-exp:" ++ show status' ++ output expRes' ++ ")"
    in  Result env' ds status' output'
stmtRes e ds (ExpressionSansSemi ex) = expRes e ds ex

stmtType Nothing     Nothing      = makePretty TypeUnit
stmtType (Just eRes) Nothing      = status eRes
stmtType Nothing     (Just aType) = makePretty aType
stmtType (Just eRes) (Just aType)
    | eResError      = Error
    | eType == aType = makePretty aType
    | otherwise      = Error
    where
        eResError = status eRes == Error
        eType     = getType $ status eRes

expRes :: Env -> Defs -> Exp -> Result
expRes e ds (IdExp n) =
    let status' = envLookup (idName n) e
        output' = "\n(id:" ++ show status' ++ "\n(" ++ idName n ++ "))"
    in  Result e ds status' output'
expRes e ds exp@(Struct n fs) =
    let status'    = if isError then Error else envLookup (idName n) e
        isError    = not $ isValidStruct exp e ds
        fieldInits = showFieldInits $ map (fieldInitRes e ds) fs
        output'    = "\n(struct:" ++ show status' ++ show n ++ fieldInits ++ ")"
    in  Result e ds status' output'
-- TODO: Verify expRes is a Struct
expRes e ds (FieldLookup ex n) =
    let expRes'     = expRes e ds ex
        structType  = getType $ status expRes'
        lookupType' = lookupType (idName $ getTypeId structType) (idName n) ds
        lookupType'' = if isTypeMut (status expRes')
                           then makePretty (TypeMut $ getType lookupType')
                           else lookupType'
        isError     =  status expRes' == Error
                    || not (isStruct ds $ status expRes')
        status'     = if isError then Error else lookupType''
        output'     = "\n(field-lookup:" ++ show status' ++ output expRes' ++ show n ++ ")"
    in  Result e ds status' output'
expRes e ds (ArrIndex e1 e2) =
    let e1Res = expRes e ds e1
        e2Res = expRes e ds e2
        isError =  status e1Res == Error
                || status e2Res == Error
                || not (isArrayType $ status e1Res)
        typ     = arrIndexType (status e1Res)
        typ'    = if isTypeMut (status e1Res)
                      then makePretty (TypeMut $ getType typ)
                      else typ
        status' = if isError then Error else typ'
        output' = "\n(arr-index:" ++ show status' ++ output e1Res ++ output e2Res ++ ")"
    in  Result e ds status' output'
expRes e ds exp@(FnCall n es) =
    let expResults' = expResults e ds es
        isError     =  any (== Error) (map status expResults')
                    || not (isValidFnCall exp e ds)
        status'     = if isError then Error else envLookup (idName n) e
        esString    = if null es then "" else showExps expResults'
        output'     = "\n(fn-call:" ++ show status' ++ show n ++ esString ++ ")"
    in  Result e ds status' output'
expRes e ds (Arr es) =
    let expResults' = expResults e ds es
        statuses    = map status expResults'
        allSameType = all (== head statuses) statuses
        anyError    = any (== Error) statuses
        isError     = not allSameType || anyError
        status'     = if isError then Error else arrType (head statuses) (length es)
        output'     = "\n(arr:" ++ show status' ++ showExps expResults' ++ ")"
    in  Result e ds status' output'
expRes e ds (Assign at e1 e2) =
    let e1Res   = expRes e ds e1
        e2Res   = expRes e ds e2
        isError =  status e1Res == Error
                || status e1Res /= status e2Res
                || not (isTypeMut $ status e1Res)
        status' = if isError then Error else makePretty TypeUnit
        output' = "\n(" ++ show at ++ ":" ++ show status' ++ output e1Res ++
                  output e2Res ++ ")"
    in  Result e ds status' output'
expRes e ds (BoxNew es) =
    let expResults' = expResults e ds es
        lenError    = length es /= 1
        paramError  = status (head expResults') == Error
        isError     = lenError || paramError
        boxType     = TypeBox . getType . status $ head expResults'
        status'     = if isError then Error else makePretty boxType
        output'     = "\n(box-new:" ++ show status' ++ showExps expResults' ++ ")"
    in  Result e ds status' output'
expRes e ds (If ex ts fs) =
    let expRes'  = expRes e ds ex
        tsRes    = blockRes e ds ts
        fsRes    = blockRes e ds fs
        tsType   = blockType (env tsRes) (defs tsRes) ts
        fsType   = blockType (env fsRes) (defs fsRes) fs
        isError  =  status tsRes == Error
                 || status fsRes == Error
                 || tsType /= fsType
        status'  = if isError then Error else tsType
        fsString = if null fs then "" else output fsRes
        output'  = "\n(if:" ++ show status' ++ output expRes' ++
                   output tsRes ++ fsString ++ ")"
    in  Result e ds status' output'
expRes e ds (While ex ss) =
    let expRes' = expRes e ds ex
        bRes    = blockRes e ds ss
        isError =  status expRes' /= makePretty TypeBool
                || status bRes /= makePretty TypeUnit
        status' = if isError then Error else status bRes
        output' = "\n(while:" ++ show status' ++ output expRes' ++ output bRes ++ ")"
    in  Result e ds status' output'
expRes e ds (Loop ss) =
    let bRes    = blockRes e ds ss
        isError = status bRes /= makePretty TypeUnit
        status' = if isError then Error else status bRes
        output' = "\n(loop:" ++ show status' ++ output bRes ++ ")"
    in  Result e ds status' output'
expRes e ds (BoolExp b)    = boolExpRes e ds b
expRes e ds (NumExp  n)    = numExpRes e ds n
expRes e ds (PointerExp p) = pointerExpRes e ds p
expRes e ds (Literal l)    = litRes e ds l
expRes e ds Unit           =
    let status' = makePretty TypeUnit
        output' = "\n(unit:" ++ show status' ++ ")"
    in  Result e ds status' output'

isTypeMut (SType (PrettyType (TypeMut _))) = True
isTypeMut _                                = False

getTypeId (TypeMut t) = typeId t
getTypeId t           = typeId t

isStruct :: Defs -> Status -> Bool
isStruct ds (SType (PrettyType (TypeMut (TypeId n)))) =
    let mStruct = Map.lookup (idName n) ds
    in  isJust mStruct
isStruct ds (SType (PrettyType (TypeId n))) =
    let mStruct = Map.lookup (idName n) ds
    in  isJust mStruct
isStruct _  _                               = False

-- TODO: this might be the worst thing I'm doing in this project
arrType :: Status -> Int -> Status
arrType s l =
    let t = getType s
        e = Just . Literal $ LitDec l
    in  makePretty . TypeArr $ ArrDef t e

isArrayType (SType (PrettyType (TypeArr d)))           = True
isArrayType (SType (PrettyType (TypeMut (TypeArr d)))) = True
isArrayType _                                          = False

arrIndexType (SType (PrettyType (TypeArr (ArrDef t _)))) = makePretty t
arrIndexType (SType (PrettyType (TypeMut (TypeArr (ArrDef t _))))) = makePretty t

showExps :: [Result] -> String
showExps es = "\n(exprs" ++ showOutputs es ++ ")"

expResults e ds []     = []
expResults e ds (ex:exs) =
    let expRes' = expRes e ds ex
    in  expRes' : expResults (env expRes') ds exs

showFieldInits :: [Result] -> String
showFieldInits fs = "\n(field-inits" ++ showOutputs fs ++ ")"

fieldInitRes :: Env -> Defs -> FieldInit -> Result
fieldInitRes e ds (FieldInit n ex) =
    let expRes' = expRes e ds ex
        status' = makePretty TypeUnit
        output' = "\n(field-init" ++ show n ++ output expRes' ++ ")"
    in  Result e ds status' output'

boolExpRes :: Env -> Defs -> BExp -> Result
boolExpRes e ds (Or e1 e2)  = binExpRes e ds e1 e2 isBool TypeBool "\n(or:"
boolExpRes e ds (And e1 e2) = binExpRes e ds e1 e2 isBool TypeBool "\n(and:"
boolExpRes e ds (Eq e1 e2)  = binExpRes e ds e1 e2 (not . isError) TypeBool "\n(eq:"
boolExpRes e ds (NEq e1 e2) = binExpRes e ds e1 e2 (not . isError) TypeBool "\n(neq:"
boolExpRes e ds (Lt e1 e2)  = binExpRes e ds e1 e2 isDec TypeBool "\n(lt:"
boolExpRes e ds (Gt e1 e2)  = binExpRes e ds e1 e2 isDec TypeBool "\n(gt:"
boolExpRes e ds (LEq e1 e2) = binExpRes e ds e1 e2 isDec TypeBool "\n(leq:"
boolExpRes e ds (GEq e1 e2) = binExpRes e ds e1 e2 isDec TypeBool "\n(geq:"
boolExpRes e ds (Not ex)    = unExpRes e ds ex isBool "\n(not:"

isBool (SType (PrettyType TypeBool))           = True
isBool (SType (PrettyType (TypeMut TypeBool))) = True
isBool _                                       = False

isBoolOrDec s = isBool s || isDec s

numExpRes :: Env -> Defs -> NExp -> Result
numExpRes e ds (Add e1 e2) = binExpRes e ds e1 e2 isDec TypeI32 "\n(add:"
numExpRes e ds (Sub e1 e2) = binExpRes e ds e1 e2 isDec TypeI32 "\n(sub:"
numExpRes e ds (Mul e1 e2) = binExpRes e ds e1 e2 isDec TypeI32 "\n(mul:"
numExpRes e ds (Div e1 e2) = binExpRes e ds e1 e2 isDec TypeI32 "\n(div:"
numExpRes e ds (Rem e1 e2) = binExpRes e ds e1 e2 isDec TypeI32 "\n(rem:"
numExpRes e ds (Neg ex)    = unExpRes e ds ex isDec "\n(neg:"

isDec (SType (PrettyType TypeI32))           = True
isDec (SType (PrettyType (TypeMut TypeI32))) = True
isDec _                                      = False

pointerExpRes :: Env -> Defs -> PExp -> Result
pointerExpRes e ds (AddrOfMut ex) = unExpRes e ds ex (not . isError) "\n(addr-of-mut:&mut"
pointerExpRes e ds (AddrOf ex)    =
    let expRes' = expRes e ds ex
        isError = status expRes' == Error
        typ     = makePretty . TypeRef . getType $ status expRes'
        status' = if isError then Error else typ
        output' = "\n(addr-of:" ++ show status' ++ output expRes' ++ ")"
    in  Result e ds status' output'
pointerExpRes e ds (Deref ex)     =
    let expRes' = expRes e ds ex
        expStatus = status expRes'
        isError = expStatus == Error
        dStatus = if isBoxType expStatus then getBoxType expStatus else expStatus
        status' = if isError then Error else dStatus
        output' = "\n(deref:" ++ show status' ++ output expRes' ++ ")"
    in  Result e ds status' output'

isError Error = True
isError _     = False

isBoxType (SType (PrettyType (TypeBox t))) = True
isBoxType _                                = False

getBoxType (SType (PrettyType (TypeBox t))) = makePretty t

binExpRes :: Env -> Defs -> Exp -> Exp -> (Status -> Bool) -> Type -> String -> Result
binExpRes e ds e1 e2 isT t pre =
    let e1Res   = expRes e ds e1
        e2Res   = expRes e ds e2
        isError =  status e1Res /= status e2Res
                || not (isT $ status e1Res)
        status' = if isError then Error else makePretty t
        output' = pre ++ show status' ++ output e1Res ++ output e2Res ++ ")"
    in  Result e ds status' output'

unExpRes :: Env -> Defs -> Exp -> (Status -> Bool) -> String -> Result
unExpRes e ds ex isT pre =
    let expRes' = expRes e ds ex
        isError = not (isT $ status expRes')
        status' = if isError then Error else status expRes'
        output' = pre ++ show status' ++ output expRes' ++ ")"
    in  Result e ds status' output'

litRes :: Env -> Defs -> Lit -> Result
litRes e ds LitChar    = litRes' e ds TypeU8   "\n(lit-char:"
litRes e ds (LitDec d) = litRes' e ds TypeI32  "\n(lit-dec:"
litRes e ds LitTrue    = litRes' e ds TypeBool "\n(true:"
litRes e ds LitFalse   = litRes' e ds TypeBool "\n(false:"
litRes e ds LitStr     =
    let status' = makePretty . TypeRef . TypeArr $ ArrDef TypeU8 Nothing
    in  Result e ds status' ("\n(lit-str:" ++ show status' ++ ")")

litRes' e ds t pre =
    let status' = makePretty t
    in  Result e ds status' (pre ++ show status' ++ ")")
