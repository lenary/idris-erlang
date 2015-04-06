module GenEvent

import Parameterised

import ErlPrelude

-- There are also calls, but we'll ignore them for the moment.
data GEL : Type -> Type where
  MkGEL : (e:Type) -> GEL e

data GERef : (GEL _) -> Type where
  MkGERef : (l:GEL _) -> ErlPid -> GERef l


data GECommands : Type where
  SendEvent : {l:GEL e} -> GERef l -> e -> GECommands

GEResponses : GECommands -> Type
GEResponses (SendEvent _ _) = Unit

GEWorld : IWorld GECommands GEResponses
GEWorld = MkIWorld GECommands GEResponses

GEP : Type -> Type
GEP = PIO GEWorld

notify : {l:GEL e} -> GERef l -> e -> GEP Unit
notify p e = interact (SendEvent p e)

namespace Init
  data GEInitDone : Type -> Type where
    GEInitOK : st -> GEInitDone st
    GEInitStop : Atom -> GEInitDone st

  ok : st -> GEP (GEInitDone st)
  ok s = return (GEInitOK s)

  stop : Atom -> GEP (GEInitDone st)
  stop a = return (GEInitStop a)

namespace Event
  data GEEventDone : Type -> Type where
    GEEventOK : st -> GEEventDone st
    GEEventStop : GEEventDone st

  ok : st -> GEP (GEEventDone st)
  ok s = return (GEEventOK s)

  remove_handler : GEP (GEEventDone st)
  remove_handler = return (GEEventStop)

data GE : (GEL _) -> Type -> Type -> Type where
  MkGE : {l:GEL e} ->
         (init: i -> GEP (GEInitDone st)) ->
         (handle_event: e -> st -> GEP (GEEventDone st)) ->
         (terminate : Atom -> st -> GEP ()) ->
         GE l st i


spawn : GERef l
spawn = ?spawn_impl

add_handler : (GE l _ i) -> GERef l -> Unit
add_handler ge init = ?add_handler_impl


testL : GEL ()
testL = MkGEL ()


testEventHandler : GE testL Int ()
testEventHandler = MkGE i he t
  where
    i _ = ok 1
    he () st = ok (st+1)
    t _ _ = return ()
