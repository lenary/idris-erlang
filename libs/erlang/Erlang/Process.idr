module Erlang.Process

import ErlPrelude

-- ProcRef l is a process that wants messages of type l.
data ProcRef : Type -> Type where
  MkProcRef : Ptr -> ProcRef l

-- Process l is a process that receives messages of type l.
data Process : (l:Type) -> Type -> Type where
  MkProc : (EIO a) -> Process l a

lift : EIO a -> Process l a
lift = MkProc

run : Process l a -> EIO a
run (MkProc p) = p

implementation Functor (Process l) where
 map f (MkProc p) = MkProc (map f p)

implementation Applicative (Process l) where
 pure a = MkProc (pure a)
 (MkProc f) <*> (MkProc a) = MkProc (f <*> a)

implementation Monad (Process l) where
  (MkProc a) >>= p = MkProc (a >>= (\as => case p as of (MkProc b) => b))

self : Process l (ProcRef l)
self = do pid <- lift $ self'
          pure $ MkProcRef pid
  where self' : EIO Ptr
        self' = foreign FFI_Erl "self" (EIO Ptr)

spawn : Process l () -> Process l' (ProcRef l)
spawn (MkProc p) = do p <- lift $ fork p
                      pure $ MkProcRef p

receive_from : ProcRef l' -> Process l l
receive_from (MkProcRef p) = do (MkERaw rec) <- lift $ receive_from' p
                                pure rec
  where receive_from' : Ptr -> EIO (ErlRaw l)
        receive_from' = foreign FFI_Erl "idris_erlang_conc:receive_from" (Ptr -> EIO (ErlRaw l))

receive_with_from : Process l (Ptr,l)
receive_with_from = do msg <- lift $ receive_with_from'
                       p   <- lift $ receive_get_from msg
                       (MkERaw rec) <- lift $ receive_get_rec msg
                       pure (p,rec)
  where receive_with_from' : EIO Ptr
        receive_with_from' = foreign FFI_Erl "idris_erlang_conc:receive_any" (EIO Ptr)
        receive_get_from : Ptr -> EIO Ptr
        receive_get_from = foreign FFI_Erl "idris_erlang_conc:receive_any_from" (Ptr -> EIO Ptr)
        receive_get_rec : Ptr -> EIO (ErlRaw l)
        receive_get_rec = foreign FFI_Erl "idris_erlang_conc:receive_any_msg" (Ptr -> EIO (ErlRaw l))

receive : Process l l
receive = do (_, msg) <- receive_with_from
             pure msg

send : ProcRef l' -> l' -> Process l ()
send (MkProcRef pid) msg = do lift $ send' pid (MkERaw msg)
                              pure ()
  where
    send' : Ptr -> ErlRaw l' -> EIO ()
    send' = foreign FFI_Erl "idris_erlang_conc:send" (Ptr -> (ErlRaw l') -> EIO ())
