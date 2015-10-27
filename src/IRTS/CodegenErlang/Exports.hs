{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module IRTS.CodegenErlang.Exports (fdesc_to_erlt, ErlT(..)) where

import Idris.Core.TT
import IRTS.Lang
import IRTS.CodegenCommon
import IRTS.Defunctionalise

data ErlT = EString
          | EAtom
          | EUnit
          | EInt
          | EChar
          | EDouble
          | EPtr -- Something Erlang understands, and Idris doesn't
          | ERaw -- Something Idris understands, and Erlang doesn't
          | EList ErlT
          | EPureFun ErlT [ErlT] -- (x,y,z) -> a
          | EIOFun ErlT [ErlT]   -- (x,y,z) -> IO a
          deriving (Show, Eq)


-- I am so fucking tired of Idris names... fuck this shit.
-- Especially as everywhere names include their namespace, except for here. OF COURSE!
pattern Erl_Str  <- FCon ((==) (sUN "Erl_Str")  -> True)
pattern Erl_Atom <- FCon ((==) (sUN "Erl_Atom") -> True)
pattern Erl_Ptr  <- FCon ((==) (sUN "Erl_Ptr")  -> True)
pattern Erl_Unit <- FCon ((==) (sUN "Erl_Unit") -> True)

pattern Erl_List a <- FApp ((==) (sUN "Erl_List") -> True) [_, a]
pattern Erl_Int    <- FApp ((==) (sUN "Erl_NumT") -> True) [_,
                                                            FCon ((==) (sUN "Erl_IntNative") -> True)]
pattern Erl_Char   <- FApp ((==) (sUN "Erl_NumT") -> True) [FUnknown,
                                                            FCon ((==) (sUN "Erl_IntChar") -> True)]
pattern Erl_Double <- FApp ((==) (sUN "Erl_NumT") -> True) [FUnknown,
                                                            FCon ((==) (sUN "Erl_Double") -> True)]
pattern Erl_Raw    <- FApp ((==) (sUN "Erl_Raw") -> True) [_]

pattern Erl_FunT f  <- FApp ((==) (sUN "Erl_FunT") -> True) [_,f]

pattern Erl_Fun t f <- FApp ((==) (sUN "Erl_Fun") -> True) [_,_,t,f]
pattern Erl_FunBase t <- FApp ((==) (sUN "Erl_FunBase") -> True) [_,t]
pattern Erl_FunIO t <- FApp ((==) (sUN "Erl_FunIO") -> True) [
  _,
  FApp ((==) (sUN "MkFFI") -> True) [
    FCon ((==) (sUN "Erl_Types") -> True),
    _,
    _],
  t]


fdesc_to_erlt :: FDesc -> ErlT
fdesc_to_erlt Erl_Str  = EString
fdesc_to_erlt Erl_Atom = EAtom
fdesc_to_erlt Erl_Unit = EUnit
fdesc_to_erlt Erl_Ptr  = EPtr
fdesc_to_erlt Erl_Raw  = ERaw
fdesc_to_erlt Erl_Int  = EInt
fdesc_to_erlt (Erl_List a) = EList (fdesc_to_erlt a)
fdesc_to_erlt (Erl_FunT f) = fun_fdesc_to_erlt f []
  where fun_fdesc_to_erlt (Erl_Fun t f) args = fun_fdesc_to_erlt f (fdesc_to_erlt t:args)
        fun_fdesc_to_erlt (Erl_FunBase t) args = EPureFun (fdesc_to_erlt t) (reverse args)
        fun_fdesc_to_erlt (Erl_FunIO t) args = EIOFun (fdesc_to_erlt t) (reverse args)
        fun_fdesc_to_erlt _ args = ERaw
fdesc_to_erlt _        = ERaw
