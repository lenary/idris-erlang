{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings#-}
module IRTS.CodegenErlang.Foreign where

import Idris.Core.TT
import IRTS.Lang
import IRTS.CodegenCommon
import IRTS.Defunctionalise

import Data.List (intercalate)
import Data.Maybe

evalName, applyName :: Name
evalName  = sMN 0 "EVAL"
applyName = sMN 0 "APPLY"

data ErlT = EString
          | EAtom
          | EUnit
          | EInt
          | EDouble
          | EPtr -- Something Erlang understands, and Idris doesn't
          | ERaw -- Something Idris understands, and Erlang doesn't
          | EList ErlT
          | EFun Bool ErlT [ErlT] -- (x,y,z) -> a (Bool is if a is an IO thing we should unwrap)
          deriving (Show, Eq)


erlFun :: ErlT -> Bool
erlFun (EFun _ _ _) = True
erlFun _ = False

-- I am so fucking tired of Idris names... fuck this shit.
-- Especially as everywhere names include their namespace, except for here. OF COURSE!
pattern Erl_Str  = FCon (UN "Erl_Str")
pattern Erl_Atom = FCon (UN "Erl_Atom")
pattern Erl_Ptr  = FCon (UN "Erl_Ptr")
pattern Erl_Unit = FCon (UN "Erl_Unit")

pattern Erl_List a <- FApp (UN "Erl_List") [_, a]
pattern Erl_Int    <- FApp (UN "Erl_NumT") [_, FCon (UN "Erl_IntNative")]
pattern Erl_Char   <- FApp (UN "Erl_NumT") [_, FCon (UN "Erl_IntChar")]
pattern Erl_Double <- FApp (UN "Erl_NumT") [_, FCon (UN "Erl_Double")]
pattern Erl_Raw    <- FApp (UN "Erl_Raw")  [_]
pattern Erl_FunT f <- FApp (UN "Erl_FunT") [_, f]

pattern Erl_Fun t f   <- FApp (UN "Erl_Fun") [_, _, t, f]
pattern Erl_FunBase t <- FApp (UN "Erl_FunBase") [_, t]
pattern Erl_FunIO t   <- FApp (UN "Erl_FunIO") [_, t]


fdesc_to_erlt :: FDesc -> ErlT
fdesc_to_erlt Erl_Str  = EString
fdesc_to_erlt Erl_Atom = EAtom
fdesc_to_erlt Erl_Unit = EUnit
fdesc_to_erlt Erl_Ptr  = EPtr
fdesc_to_erlt Erl_Raw  = ERaw
fdesc_to_erlt Erl_Int  = EInt
fdesc_to_erlt Erl_Char = EInt -- We represent chars as integers
fdesc_to_erlt Erl_Double = EDouble
fdesc_to_erlt (Erl_List a) = EList (fdesc_to_erlt a)
fdesc_to_erlt (Erl_FunT f) = fun_fdesc_to_erlt f []
  where fun_fdesc_to_erlt (Erl_Fun t f) args = fun_fdesc_to_erlt f (fdesc_to_erlt t:args)
        fun_fdesc_to_erlt (Erl_FunBase t) args = EFun False (fdesc_to_erlt t) (reverse args)
        fun_fdesc_to_erlt (Erl_FunIO t) args   = EFun True (fdesc_to_erlt t) (reverse args)
        fun_fdesc_to_erlt _ args = ERaw
fdesc_to_erlt _        = ERaw


check_t :: ErlT -> String -> Maybe String
check_t EString exp = Just $ "true = lists:all(fun erlang:is_number/1, "++ exp ++")"
check_t EAtom exp   = Just $ "true = is_atom("++ exp ++")"
check_t EUnit exp   = Nothing
check_t EPtr exp    = Nothing
check_t ERaw exp    = Nothing
check_t EInt exp    = Just $ "true = is_integer("++ exp ++")"
check_t EDouble exp = Just $ "true = is_float("++ exp ++")"
check_t (EList a) exp = case check_t' a of
  Just fn -> Just $ "true = lists:all("++ fn ++", "++ exp ++")"
  Nothing -> Nothing
check_t _ exp = Nothing

check_t' :: ErlT -> Maybe String
check_t' EString = Just "fun(S) -> true = lists:all(fun erlang:is_number/1, S) end"
check_t' EAtom   = Just "fun erlang:is_atom/1"
check_t' EUnit   = Just "fun(U) -> true = ({} =:= U) end"
check_t' EInt    = Just "fun erlang:is_integer/1"
check_t' EDouble = Just "fun erlang:is_float/1"
check_t' (EList a) = case check_t' a of
  Just fn -> Just $ "fun(L) -> true = lists:all("++ fn ++", L) end"
  Nothing -> Nothing
check_t' _ = Nothing


checkedFnCall :: String -> String -> FDesc -> [FDesc] -> String
checkedFnCall nm orig rety args = let argtys = map fdesc_to_erlt args
                                      argNms = argNames argtys
                                      decl = fndecl nm argNms
                                      cbks = checkedCallbackFuns (zip argtys argNms)
                                      reschk = checkedResult orig (fdesc_to_erlt rety) (zip argtys argNms)
                                    in concat $ [decl] ++ cbks ++ [reschk, "."]

argNames :: [ErlT] -> [String]
argNames tys = let itys = zip tys [1..]
               in map argName itys
  where argName (ty, ix) = (if erlFun ty then "CB" else "FC") ++ show ix

fndecl :: String -> [String] -> String
fndecl nm args = nm ++ "(" ++ (", " `intercalate` args) ++ ") ->\n"

checkedCallbackFuns :: [(ErlT,String)] -> [String]
checkedCallbackFuns = mapMaybe (uncurry checkedCallBack)


checkedCallBack :: ErlT -> String -> Maybe String
checkedCallBack ty _ | not (erlFun ty) = Nothing
checkedCallBack (EFun unwrap ret args) nm   = Just $ "Chkd_"++nm++" = fun("++ argstr ++") -> "++ body ++"end,\n"
  where args' = map (\(ty,ix) -> (ty,nm ++ "_" ++ show ix)) (zip args [1..])
        argstr = ", " `intercalate` (map snd args')
        chks = mapMaybe (uncurry check_t) args'
        finalcall True xs = "'APPLY0'("++ finalcall False xs ++", the_world)"
        finalcall False []     = nm
        finalcall False (x:xs) = "'APPLY0'("++ finalcall False xs ++", "++ x ++")"

        body = ",\n" `intercalate` (chks ++ [""++finalcall unwrap (reverse (map snd args'))++""])

checkedResult :: String -> ErlT -> [(ErlT,String)] -> String
checkedResult orig rety args = concat (catMaybes [Just ("Res = "++ orig ++ "("++ call_args ++"),\n")
                                                 ,fmap (++ ",\n") $ check_t rety "Res"
                                                 ,Just "Res\n"])
  where args' = map (\(ty,nm) -> if erlFun ty then "Chkd_" ++ nm else nm) args
        call_args = ", " `intercalate` args'
