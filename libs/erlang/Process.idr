module Process

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

-- TODO: RPC? I guess that goes somewhere else, though we may know
-- from the original Process and the one we're calling what the types
-- are. Here goes nothing.

-- This could perhaps be a UniqueType, but it's not allowed in our monad... :(
data RPCSenderTag : Type -> UniqueType where
  MkRPCSTag : Ptr -> RPCSenderTag l


rpc_send_req : ProcRef l' -> l' -> Process l (RPCSenderTag l)
rpc_send_req (MkProcRef p) req = do tag <- lift $ rpc_send_req' p (MkERaw req)
                                    return (MkRPCSTag tag)
  where rpc_send_req' : Ptr -> ErlRaw l' -> EIO Ptr
        rpc_send_req' = foreign FFI_Erl "idris_erlang_conc:rpc_send_req" (Ptr -> ErlRaw l' -> EIO Ptr)

rpc_recv_rep : RPCSenderTag l -> Process l l
rpc_recv_rep (MkRPCSTag tag) = do (MkERaw reply) <- lift $ rpc_recv_rep' tag
                                  return reply
  where rpc_recv_rep' : Ptr -> EIO (ErlRaw l)
        rpc_recv_rep' = foreign FFI_Erl "idris_erlang_conc:rpc_recv_rep" (Ptr -> EIO (ErlRaw l))


data RPCRecvTag : UniqueType where
  MkRPCRTag : Ptr -> RPCRecvTag

rpc_recv_req : Process l (RPCRecvTag,l)
rpc_recv_req = do (from,MkERaw req) <- lift $ rpc_recv_req'
                  return $ (MkRPCRTag from,req)
  where rpc_recv_req' : EIO (Ptr, ErlRaw l)
        rpc_recv_req' = foreign FFI_Erl "idris_erlang_conc:rpc_recv_req" (EIO (Ptr, ErlRaw l))

rpc_send_rep : RPCRecvTag -> l' -> Process l ()
rpc_send_rep (MkRPCRTag p) rep = lift $ rpc_send_rep' p (MkERaw rep)
  where rpc_send_rep' : Ptr -> (ErlRaw l') -> EIO ()
        rpc_send_rep' = foreign FFI_Erl "idris_erlang_conc:rpc_send_rep" (Ptr -> (ErlRaw l') -> EIO ())

rpc : ProcRef l' -> l' -> Process l l
rpc p req = do tag <- rpc_send_req p req
               rpc_recv_rep tag

handle_one_rpc : (l -> Process l (l',t)) -> Process l t
handle_one_rpc f = do (tag,req) <- rpc_recv_req
                      (rep,next) <- f req
                      rpc_send_rep tag rep
                      return next
