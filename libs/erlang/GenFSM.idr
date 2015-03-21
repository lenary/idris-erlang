module GenFSM

-- This gives us IWorld and PIO, which are from the paper "Interactive
-- Programs for Dependent Type Theory" by Hancock and Seltzer. They
-- are non-normalising, but do the job.
import Parameterised

-- For Atoms and Pids
import ErlTypes

data GFL : (s:Type) -> (cl:Type) -> (cl -> s -> Type) -> Type -> Type where
  MkGFL : (s:Type) -> (cl:Type) -> (cr:(cl -> s -> Type)) -> (ct:Type) -> GFL s cl cr ct

data GFRef : (l:GFL _ _ _ _) -> Type where
  MkGFRef : {l:GFL _ _ _ _} -> ErlPid -> GFRef l


data GFCommands : Type where
  SendCall : {l':GFL _ cl cr _} -> GFRef l' -> cl -> GFCommands
  SendCast : {l':GFL _ _  _ ct} -> GFRef l' -> ct -> GFCommands

GFResponses : GFCommands -> Type
GFResponses (SendCall (MkGFRef {l=MkGFL s _ cr _} _) m) = (s' : s ** cr m s')
GFResponses (SendCast _ m) = Unit

GFWorld : IWorld GFCommands GFResponses
GFWorld = MkIWorld GFCommands GFResponses

GFP : Type -> Type
GFP = PIO GFWorld


namespace Init
  data GFInitDone : (GFL s _ _ _) -> Type -> Type where
    GFInitOK : st -> GFInitDone st
    GFInitStop : ErlAtom -> GFInitDone st
{-
  ok : st -> GFP (GFInitDone st)
  ok s = leaf (GFInitOK s)

  stop : ErlAtom -> GFP (GFInitDone st)
  stop a = leaf (GFInitStop a)
-}

{-
namespace Call
  data GFCallDone : (GFL cl _ _) -> Type -> cl -> Type where
    -- We need to supply the message we're replying to, to make sure our response has the right type.
    GFReply : {l:GFL cl cr _} -> {c:cl} -> (cr c) -> st -> GFCallDone l st c
    GFReplyStop : {l:GFL cl cr _} -> {c:cl} -> ErlAtom -> (cr c) -> st -> GFCallDone l st c

{-
  reply : {l:GFL cl cr _} -> {c:cl} -> (cr c) -> st -> GFP (GFCallDone l st c)
  reply r s = leaf (GFReply r s)

  stop : {l:GFL cl cr _} -> {c:cl} -> ErlAtom -> (cr c) -> st -> GFP (GFCallDone l st c)
  stop a r s = leaf (GFReplyStop a r s)
-}

namespace Cast
  data GFCastDone : Type -> Type where
    GFNoReply : st -> GFCastDone st
    GFNoReplyStop : ErlAtom -> st -> GFCastDone st

{-
  no_reply : st -> GFP (GFCastDone st)
  no_reply s = leaf (GFNoReply s)

  stop : ErlAtom -> st -> GFP (GFCastDone st)
  stop a s = leaf (GFNoReplyStop a s)
-}

data GF : (GFL _ _ _) -> Type -> Type -> Type where
  MkGF : {l:GFL cl cr ct} ->
         (init : i -> GFP (GFInitDone st)) ->
         (handle_call : (c:cl) -> st -> GFP (GFCallDone l st c)) ->
         (handle_cast : ct -> st -> GFP (GFCastDone st)) ->
         (terminate : ErlAtom -> st -> GFP ()) ->
         GF l st i

-- I can't fill in the implementation as it's a foreign call to erlang
spawn : (GF l _ i) -> i -> GFRef l
spawn gs init = ?spawn_impl

-- This is how we specify a "Language" that a GenServer uses to
-- communicate, In this example, a simple echo server, it just
-- responds to a call with the same type that it recieved.
echoL : {a:Type} -> GFL a (\_ => a) ()
echoL {a} = MkGFL a (\_ => a) ()

-- And here's our implementation of the server with language echoL
echoGF : GF echoL () ()
echoGF = MkGF i hcl hct t
  where
    -- Init function: just put () into the state
    i _ = ok ()
    -- Handle Call: reply with the message you got, preserving the state
    hcl x s = reply x s
    -- Handle Cast: do nothing, preserve state
    hct _ s = no_reply s
    -- Terminate: do nothing, return ()
    t _ _ = leaf ()
-}
