module OTP.GenFSM

-- This gives us IWorld and PIO, which are from the paper "Interactive
-- Programs for Dependent Type Theory" by Hancock and Seltzer. They
-- are non-normalising, but do the job.
import OTP.Parameterised

-- For Atoms and Pids
import ErlPrelude

data GFL : (st:Type) -> (se:Type) -> (se -> st -> Type) -> Type -> Type where
  MkGFL : (st:Type) -> (se:Type) -> (ser:(se -> st -> Type)) -> (e:Type) -> GFL st se ser e

data GFRef : (GFL _ _ _ _) -> Type where
  MkGFRef : {l:GFL _ _ _ _} -> ErlPid -> GFRef l

data GFCommands : Type where
  SendEvent :     {l':GFL _ _ _ e}  -> GFRef l' -> e  -> GFCommands
  SendSyncEvent : {l':GFL _ se _ _} -> GFRef l' -> se -> GFCommands

  -- Eventually it would be good to support these two too, but given they
  -- have the same type, they don't add anything to the implementation
  --
  -- SendAllEvent : {l':GFL _ _ _ e} -> GFRef l' -> e -> GFCommands
  -- SendAllSyncEvent : {l':GFL _ se _ _} -> GFRef l' -> se -> GFCommands

data GFSyncResp : (GFL _ se _ _) -> (e:se) -> Type where
  MkSyncResp : {l:GFL st se ser _} -> {e:se} -> (s : st) -> (r:ser e s) -> GFSyncResp l e

GFResponses : GFCommands -> Type
GFResponses (SendSyncEvent (MkGFRef {l} _) e) = GFSyncResp l e
GFResponses (SendEvent _ _) = Unit

GFWorld : IWorld GFCommands GFResponses
GFWorld = MkIWorld GFCommands GFResponses

GFP : Type -> Type
GFP = PIO GFWorld

send_event : {l:GFL _ _ _ e} -> GFRef l -> e -> GFP Unit
send_event p e = interact (SendEvent p e)

--sync_send_event : {l:GFL _ se ser _} -> GFRef l -> (e:se) -> GFP (GFSyncResp l e)
--sync_send_event p e = interact (SendSyncEvent p e)

namespace Init
  data GFInitDone : (GFL _ _ _ _) -> Type -> Type where
    GFInitOK : {l:GFL st _ _ _} -> (s:st) -> sd -> GFInitDone l sd
    GFInitStop : Atom -> GFInitDone l sd

  ok : {l:GFL s _ _ _} -> s -> sd -> GFP (GFInitDone l sd)
  ok s sd = return (GFInitOK s sd)

  stop : Atom -> GFP (GFInitDone l sd)
  stop a = return (GFInitStop a)


namespace Event
  data GFEventDone : (GFL _ _ _ _) -> Type -> Type where
    GFNextState : {l:GFL st _ _ _} -> (s:st) -> sd -> GFEventDone l sd
    GFEventStop : Atom -> sd -> GFEventDone l sd

  next_state : {l:GFL st _ _ _} -> (s:st) -> sd -> GFP (GFEventDone l sd)
  next_state s sd = return (GFNextState s sd)

  stop : Atom -> sd -> GFP (GFEventDone l sd)
  stop a sd = return (GFEventStop a sd)


namespace SyncEvent
  data GFSyncEventDone : (GFL st se ser _) -> st -> se -> Type -> Type where
    GFSyncReply : {l:GFL st se ser _} -> {c:st} -> {e:se} -> (r:ser e c) -> st -> sd -> GFSyncEventDone l c e sd
    GFSyncStop : Atom -> sd -> GFSyncEventDone l c e sd

  reply : {l:GFL st se ser _} -> {c:st} -> {e:se} -> (ser e c) -> st -> sd -> GFP (GFSyncEventDone l c e sd)
  reply r st sd = return (GFSyncReply r st sd)

  stop : Atom -> sd -> GFP (GFSyncEventDone l c e sd)
  stop a sd = return (GFSyncStop a sd)

data GF : (GFL _ _ _ _) -> Type -> Type -> Type where
  MkGF : {l:GFL st se ser e} ->
         (init : i -> GFP (GFInitDone l sd)) ->
         (state_event : st -> e -> sd -> GFP (GFEventDone l sd)) ->
         (state_sync_event : (s:st) -> (e:se) -> sd -> GFP (GFSyncEventDone l s e sd)) ->
         (terminate : Atom -> sd -> GFP ()) ->
         GF l sd i


spawn : (GF l _ i) -> i -> GFRef l
spawn gf init = ?spawn_impl

-- This is how we specify a "Language" that a GenFSM uses to
-- communicate, In this example it's a simple switch that specifies
-- whether the fsm will reply with a String or an Int.

computeReply : (Unit -> Bool -> Type)
computeReply _ True = Integer
computeReply _ False = String

flipperL : GFL Bool Unit (computeReply) Unit
flipperL = MkGFL Bool Unit computeReply Unit

flipperF : GF flipperL () ()
flipperF = MkGF i hse hsse t
  where i _ = ok True ()
        hse state () dat = next_state (not state) dat
        hsse True () dat = reply 87 True dat
        hsse False () dat = reply "87 Rules!" True dat
        t _ _ = return ()
