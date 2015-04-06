module OTP.GenServer

-- This gives us IWorld and PIO, which are from the paper "Interactive
-- Programs for Dependent Type Theory" by Hancock and Seltzer. They
-- are non-normalising, but do the job.
import OTP.Parameterised

-- For Atoms and Pids
import ErlPrelude

data GSL : (cl:Type) -> (cl -> Type) -> Type -> Type where
  MkGSL : (cl:Type) -> (cr:(cl -> Type)) -> (ct:Type) -> GSL cl cr ct

data GSRef : (GSL _ _ _) -> Type where
  MkGSRef : ErlPid -> GSRef l

data GSCommands : Type where
  SendCall : {l:GSL cl cr _} -> GSRef l -> cl -> GSCommands
  SendCast : {l:GSL _ _  ct} -> GSRef l -> ct -> GSCommands

GSResponses : GSCommands -> Type
GSResponses (SendCall (MkGSRef {l=MkGSL _ cr _} _) m) = cr m
GSResponses (SendCast _ _) = Unit

GSWorld : IWorld GSCommands GSResponses
GSWorld = MkIWorld GSCommands GSResponses

GSP : Type -> Type
GSP = PIO GSWorld

-- call : {l:GSL cl cr _} -> GSRef l -> (m:cl) -> GSP (cr m)
-- call p m = interact (SendCall p m)

cast : {l:GSL _ _ ct} -> GSRef l -> ct -> GSP Unit
cast p m = interact (SendCast p m)

namespace Init
  data GSInitDone : Type -> Type where
    GSInitOK : st -> GSInitDone st
    GSInitStop : Atom -> GSInitDone st

  ok : st -> GSP (GSInitDone st)
  ok s = return (GSInitOK s)

  stop : Atom -> GSP (GSInitDone st)
  stop a = return (GSInitStop a)

namespace Call
  data GSCallDone : (GSL cl _ _) -> Type -> cl -> Type where
    -- We need to supply the message we're replying to, to make sure our response has the right type.
    GSReply : {l:GSL cl cr _} -> {c:cl} -> (cr c) -> st -> GSCallDone l st c
    GSReplyStop : {l:GSL cl cr _} -> {c:cl} -> Atom -> (cr c) -> st -> GSCallDone l st c

  reply : {l:GSL cl cr _} -> {c:cl} -> (cr c) -> st -> GSP (GSCallDone l st c)
  reply r s = return (GSReply r s)

  stop : {l:GSL cl cr _} -> {c:cl} -> Atom -> (cr c) -> st -> GSP (GSCallDone l st c)
  stop a r s = return (GSReplyStop a r s)

namespace Cast
  data GSCastDone : Type -> Type where
    GSNoReply : st -> GSCastDone st
    GSNoReplyStop : Atom -> st -> GSCastDone st

  no_reply : st -> GSP (GSCastDone st)
  no_reply s = return (GSNoReply s)

  stop : Atom -> st -> GSP (GSCastDone st)
  stop a s = return (GSNoReplyStop a s)

data GS : (GSL _ _ _) -> Type -> Type -> Type where
  MkGS : {l:GSL cl cr ct} ->
         (init : i -> GSP (GSInitDone st)) ->
         (handle_call : (c:cl) -> st -> GSP (GSCallDone l st c)) ->
         (handle_cast : ct -> st -> GSP (GSCastDone st)) ->
         (terminate : Atom -> st -> GSP ()) ->
         GS l st i

-- I can't fill in the implementation as it's a foreign call to erlang
spawn : (GS l _ i) -> i -> GSRef l
spawn gs init = ?spawn_impl

-- This is how we specify a "Language" that a GenServer uses to
-- communicate, In this example, a simple echo server, it just
-- responds to a call with the same type that it recieved.
echoL : {a:Type} -> GSL a (\_ => a) ()
echoL {a} = MkGSL a (\_ => a) ()

-- And here's our implementation of the server with language echoL
echoGS : GS echoL () ()
echoGS = MkGS i hcl hct t
  where
    -- Init function: just put () into the state
    i _ = ok ()
    -- Handle Call: reply with the message you got, preserving the state
    hcl x s = reply x s
    -- Handle Cast: do nothing, preserve state
    hct _ s = no_reply s
    -- Terminate: do nothing, return ()
    t _ _ = return ()
