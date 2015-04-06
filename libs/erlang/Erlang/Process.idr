module Erlang.Process

import ErlPrelude

-- ProcRef l is a process that wants messages of type l.
data ProcRef : Type -> Type where
  MkProcRef : Ptr -> ProcRef l

-- Process l is a process that receives messages of type l.
data Process : Type -> Type* -> Type where
  MkProc : (EIO a) -> Process l a

lift : EIO a -> Process l a
lift = MkProc

run : Process l a -> EIO a
run (MkProc p) = p

map : {a,b:Type*} -> (a -> b) -> Process l a -> Process l b
map f (MkProc p) = MkProc (map f p)

instance Functor (Process l) where
 map = Process.map

pure : {a:Type*} -> a -> Process l a
pure a = MkProc (pure a)

return : {a:Type*} -> a -> Process l a
return = Process.pure

(<*>) : {a,b:Type*} -> Process l (a -> b) -> Process l a -> Process l b
(MkProc f) <*> (MkProc a) = MkProc (f <*> a)

instance Applicative (Process l) where
 pure = Process.pure
 (<*>) = Process.(<*>)

(>>=) : {a,b:Type*} -> Process l a -> (a -> Process l b) -> Process l b
(MkProc a) >>= p = MkProc (a >>= (\as => case p as of (MkProc b) => b))

instance Monad (Process l) where
  (>>=) = Process.(>>=)

self : Process l (ProcRef l)
self = do pid <- lift $ self'
          return $ MkProcRef pid
  where self' : EIO Ptr
        self' = foreign FFI_Erl "self" (EIO Ptr)

spawn : Process l () -> EIO (ProcRef l)
spawn (MkProc p) = do p <- fork p
                      return $ MkProcRef p

receive_from : ProcRef l' -> Process l l
receive_from (MkProcRef p) = do (MkERaw rec) <- lift $ receive_from' p
                                return rec
  where receive_from' : Ptr -> EIO (ErlRaw l)
        receive_from' = foreign FFI_Erl "idris_erlang_conc:receive_from" (Ptr -> EIO (ErlRaw l))

receive_with_from : Process l (UPair (ProcRef l') l)
receive_with_from = do (p,MkERaw rec) <- lift $ receive_with_from'
                       return (MkProcRef p,rec)
  where receive_with_from' : EIO (Ptr, ErlRaw l)
        receive_with_from' = foreign FFI_Erl "idris_erlang_conc:receive_any" (EIO (Ptr,ErlRaw l))

receive : Process l l
receive = do (MkUPair _ msg) <- receive_with_from
             return msg

send : ProcRef l' -> l' -> Process l ()
send (MkProcRef pid) msg = do lift $ send' pid (MkERaw msg)
                              return ()
  where
    send' : Ptr -> ErlRaw l' -> EIO ()
    send' = foreign FFI_Erl "idris_erlang_conc:send" (Ptr -> (ErlRaw l') -> EIO ())
