%unqualified

%access public

data ErlFn : Type -> Type where
  MkErlFun : (x : t) -> ErlFn t
%used MkErlFun x

data Atom : Type where
  MkAtom : (x : String) -> Atom

ErlPid : Type
ErlPid = Ptr

mutual
  data Erl_NumTypes: Type -> Type where
    Erl_IntChar    : Erl_NumTypes Char
    Erl_IntNative  : Erl_NumTypes Int

  data Erl_FunTypes : Type -> Type where
    Erl_Fun     : Erl_Types s -> Erl_FunTypes t -> Erl_FunTypes (s -> t)
    Erl_FunIO   : Erl_Types t -> Erl_FunTypes (IO' l t)
    Erl_FunBase : Erl_Types t -> Erl_FunTypes t

  data Erl_Types : Type -> Type where
    Erl_Str  : Erl_Types String
    Erl_Atom : Erl_Types Atom
    Erl_Ptr  : Erl_Types Ptr
    Erl_Unit : Erl_Types ()
    Erl_Any  : Erl_Types (Raw a)
    Erl_List : Erl_Types a -> Erl_Types (List a)
    Erl_Tupl : Erl_Types a -> Erl_Types b -> Erl_Types (a,b)
    -- These have to come last
    Erl_FunT : Erl_FunTypes a -> Erl_Types (ErlFn a)
    Erl_NumT : Erl_NumTypes t -> Erl_Types t

FFI_Erl : FFI
FFI_Erl = MkFFI Erl_Types String String

-- Make your "Old MacDonald" jokes here please
EIO : Type -> Type
EIO = IO' FFI_Erl

-- Annoyingly, the File struct isn't abstract so we can't use it.
abstract
data EFile = EHandle Ptr

namespace Erl
  openFile : String -> Mode -> EIO EFile
  openFile filename mode = do p <- open filename (modeStr mode)
                              return (EHandle p)
    where modeStr : Mode -> String
          modeStr Read = "r"
          modeStr Write = "w"
          modeStr ReadWrite = "rw"

          open : String -> String -> EIO Ptr
          open = foreign FFI_Erl "idris_erlang_rts:file_open" (String -> String -> EIO Ptr)


  closeFile : EFile -> EIO ()
  closeFile (EHandle p) = do x <- close p
                             return ()
    where close : Ptr -> EIO Int
          close = foreign FFI_Erl "idris_erlang_rts:file_close" (Ptr -> EIO Int)


  fgetc : EFile -> EIO Char
  fgetc (EHandle p) = do c <- getChar p
                         return (cast c)
    where getChar : Ptr -> EIO Int
          getChar = foreign FFI_Erl "idris_erlang_rts:read_chr" (Ptr -> EIO Int)


  fread : EFile -> EIO String
  fread (EHandle h) = prim_fread h

  -- TODO: primitives
  fwrite : EFile -> String -> EIO ()
  fwrite (EHandle h) s = do writeFile h s
                            return ()
    where writeFile : Ptr -> String -> EIO Int
          writeFile = foreign FFI_Erl "idris_erlang_rts:write_file" (Ptr -> String -> EIO Int)

  feof : EFile -> EIO Bool
  feof (EHandle p) = do res <- fileEOF p
                        return (res /= 0)
    where fileEOF : Ptr -> EIO Int
          fileEOF = foreign FFI_Erl "idris_erlang_rts:file_eof" (Ptr -> EIO Int)


  nullStr : String -> EIO Bool
  nullStr s = do res <- strIsNull s
                 return (res /= 0)
    where strIsNull : String -> EIO Int
          strIsNull = foreign FFI_Erl "idris_erlang_rts:str_null" (String -> EIO Int)


  nullPtr : Ptr -> EIO Bool
  nullPtr p = do res <- isNull p
                 return (res /= 0)
    where isNull : Ptr -> EIO Int
          isNull = foreign FFI_Erl "idris_erlang_rts:ptr_null" (Ptr -> EIO Int)

string_to_atom : String -> EIO Atom
string_to_atom = foreign FFI_Erl "list_to_atom" (String -> EIO Atom)

lists_reverse : List Int -> EIO (List Int)
lists_reverse = foreign FFI_Erl "lists:reverse" (List Int -> EIO (List Int))
