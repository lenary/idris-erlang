module GenServer

-- This gives us IWorld and PIO, which are from the paper "Interactive
-- Programs for Dependent Type Theory" by Hancock and Seltzer. They
-- are non-normalising, but do the job.
import Parameterised

-- For Atoms and Pids
import ErlTypes

data GSL : (cl:Type) -> (cl -> Type) -> Type -> Type where
  MkGSL : (cl:Type) -> (cr:cl -> Type) -> (ct:Type) -> GSL cl cr ct

data GSRef : (l:GSL _ _ _) -> Type where
  MkGSRef : {l:GSL _ _ _} -> ErlPid -> GSRef l

data GSCommands : Type where
  SendCall : {l':GSL cl cr _} -> GSRef l' -> cl -> GSCommands
  SendCast : {l':GSL _ _  ct} -> GSRef l' -> ct -> GSCommands

GSResponses : GSCommands -> Type
GSResponses (SendCall (MkGSRef {l=MkGSL _ cr _} _) m) = cr m
GSResponses (SendCast _ m) = Unit

GSWorld : IWorld GSCommands GSResponses
GSWorld = MkIWorld GSCommands GSResponses

GSP : Type -> Type
GSP = PIO GSWorld

namespace Init
  data GSInitDone : Type -> Type where
    GSInitOK : st -> GSInitDone st
    GSInitStop : ErlAtom -> GSInitDone st

  ok : st -> GSP (GSInitDone st)
  ok s = leaf (GSInitOK s)

  stop : ErlAtom -> GSP (GSInitDone st)
  stop a = leaf (GSInitStop a)

namespace Call
  data GSCallDone : (GSL cl _ _) -> Type -> cl -> Type where
    -- We need to supply the message we're replying to, to make sure our response has the right type.
    GSReply : {l:GSL cl cr _} -> {c:cl} -> (cr c) -> st -> GSCallDone l st c
    GSReplyStop : {l:GSL cl cr _} -> {c:cl} -> ErlAtom -> (cr c) -> st -> GSCallDone l st c

  reply : {l:GSL cl cr _} -> {c:cl} -> (cr c) -> st -> GSP (GSCallDone l st c)
  reply r s = leaf (GSReply r s)

  stop : {l:GSL cl cr _} -> {c:cl} -> ErlAtom -> (cr c) -> st -> GSP (GSCallDone l st c)
  stop a r s = leaf (GSReplyStop a r s)

namespace Cast
  data GSCastDone : Type -> Type where
    GSNoReply : st -> GSCastDone st
    GSNoReplyStop : ErlAtom -> st -> GSCastDone st

  no_reply : st -> GSP (GSCastDone st)
  no_reply s = leaf (GSNoReply s)

  stop : ErlAtom -> st -> GSP (GSCastDone st)
  stop a s = leaf (GSNoReplyStop a s)

data GS : (GSL _ _ _) -> Type -> Type -> Type where
  MkGS : {l:GSL cl cr ct} ->
         (init : i -> GSP (GSInitDone st)) ->
         (handle_call : (c:cl) -> st -> GSP (GSCallDone l st c)) ->
         (handle_cast : ct -> st -> GSP (GSCastDone st)) ->
         (terminate : ErlAtom -> st -> GSP ()) ->
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
    t _ _ = leaf ()
