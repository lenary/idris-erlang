module Erlang.RPC

import ErlPrelude
import Erlang.Process

abstract
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

abstract
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

handle_rpc : (l -> Process l (l',t)) -> Process l t
handle_rpc f = do (tag,req) <- rpc_recv_req
                  (rep,next) <- f req
                  rpc_send_rep tag rep
                  return next

partial
handle_rpc_loop : (st -> l -> Process l (l',st)) -> st -> Process l ()
handle_rpc_loop f init = handle_rpc (f init) >>= handle_rpc_loop f

partial
spawn_rpc : (st -> l -> Process l (l',st)) -> st -> Process l'' (ProcRef l)
spawn_rpc f init = spawn $ handle_rpc_loop f init
