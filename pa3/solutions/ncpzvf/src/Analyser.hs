module Analyser
( analyseRust
, isValid
) where

import Syntax
import Parser (parseRust)
import Control.Applicative ((<|>))
import Data.List (find)

type Analysis = Either String String

msgNoMain       = "main function not found."
msgBadMain      = "main function has the wrong type."
msgDupFunc f    = "duplicate definition of function `" ++ f ++ "`."
msgDupParam f p = "identifier `" ++ p ++ "` is bound more than once in the " ++
                  "parameter list for the function `" ++ f ++ "`."
msgDupSOrE x    = "duplicate definition of struct or enum `" ++ x ++ "`."
msgDupCtor e c  = "duplicate definition of the constructor `" ++ c ++ "` " ++
                  "for the enum `" ++ e ++ "`."
msgDupField s f = "duplicate declaration of the field `" ++ f ++ "` of " ++
                  "the struct `" ++ s ++ "`."

analyseRust :: String -> Analysis
analyseRust r = case parseRust r of
                    Left e  -> Left (show e)
                    Right s -> isValid s

isValid :: Crate -> Analysis
isValid c = do
    hasValidMain c
    areValidItems (items c)
    Right ""

hasValidMain :: Crate -> Analysis
hasValidMain c = do
    m <- maybeToEither msgNoMain (find isMain $ getFunctions c)
    boolToEither msgBadMain (hasValidMainType m)
    boolToEither msgBadMain (null $ params m)
    Right ""

areValidItems :: [Item] -> Analysis
areValidItems []     = Right ""
areValidItems (i:is) = do
    isValidItem is i
    areValidItems is

isValidItem :: [Item] -> Item -> Analysis
isValidItem is f@(FnDef {})     = isValidFunction is f
isValidItem is e@(EnumDef {})   = isValidEnum is e
isValidItem is s@(StructDef {}) = isValidStruct is s

isValidFunction :: [Item] -> Item -> Analysis
isValidFunction is f = do
    let fs = filter isFunction is
    isUniqueFunction fs f
    hasUniqueParams f
    Right ""

isValidEnum is e = do
    let nonFs = filter (not . isFunction) is
    isUniqueEnum nonFs e
    hasUniqueCtors e
    Right ""

isValidStruct is s = do
    let nonFs = filter (not . isFunction) is
    isUniqueStruct nonFs s
    hasUniqueFields s
    Right ""

getFunctions :: Crate -> [Item]
getFunctions c = filter isFunction (items c)

isFunction :: Item -> Bool
isFunction (FnDef {}) = True
isFunction _          = False

isMain item = idName (funcId item) == "main"

boolToEither :: String -> Bool -> Either String Bool
boolToEither _   True  = Right True
boolToEither msg False = Left msg

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _   (Just x) = Right x
maybeToEither msg Nothing  = Left msg

hasValidMainType :: Item -> Bool
hasValidMainType (FnDef _ _ t _) = any (t ==) [Nothing, Just TypeUnit]
hasValidMainType _               = error "Not a function"

isUniqueFunction []     _ = Right ""
isUniqueFunction (f:fs) x
    | isDup     = Left (msgDupFunc $ getFuncName x)
    | otherwise = isUniqueFunction fs x
    where
        getFuncName = idName . funcId
        isDup       = getFuncName f == getFuncName x

hasUniqueParams f =
     let getParamName = idName . patternId . paramPat
         funcName     = idName $ funcId f
         dup          = findDupBy getParamName $ params f
     in  case dup of
             Nothing -> Right ""
             Just p  -> Left (msgDupParam funcName $ getParamName p)

patternId (PatId i)       = i
patternId (PatRefId i)    = i
patternId (PatRefMutId i) = i
patternId (PatMutId i)    = i
patternId (PatDeref p)    = patternId p

isUniqueEnum is x   = isUniqueStructOrEnum is x (idName $ enumId x)
isUniqueStruct is x = isUniqueStructOrEnum is x (idName $ structId x)

isUniqueStructOrEnum :: [Item] -> Item -> String -> Analysis
isUniqueStructOrEnum []     _ _ = Right ""
isUniqueStructOrEnum (i:is) x n
    | isDup i   = Left (msgDupSOrE n)
    | otherwise = isUniqueStructOrEnum is x n
    where
        isDup (EnumDef iden _)   = (idName iden) == n
        isDup (StructDef iden _) = (idName iden) == n

hasUniqueCtors e =
    let getCtorName = idName . ctorId
        enumName    = idName $ enumId e
        dup         = findDupBy getCtorName $ ctors e
    in  case dup of
            Nothing -> Right ""
            Just c  -> Left (msgDupCtor enumName $ getCtorName c)

hasUniqueFields s =
    let getFieldName = idName . fieldId
        structName   = idName $ structId s
        dup          = findDupBy getFieldName $ fields s
    in  case dup of
            Nothing -> Right ""
            Just f  -> Left (msgDupField structName $ getFieldName f)

findDupBy f []     =  Nothing
findDupBy f (x:xs) =  find ((f x ==) . f) xs
                  <|> findDupBy f xs
