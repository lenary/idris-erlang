module Parameterised

import Control.Monad.Identity
import Control.Monad.State

data IWorld : (t:Type) -> (t -> Type) -> Type where
  MkIWorld : (c:Type) -> (r : (c -> Type)) -> IWorld c r

-- Parameterised over an implicit world
codata PIO : (w : IWorld _ _) -> Type -> Type where
  leaf : a -> PIO w a
  cont : {w : IWorld cs r} -> (c : cs) -> (r c -> Inf (PIO w a)) -> PIO w a

interact : {w : IWorld cs rs} -> (c : cs) -> PIO w (rs c)
interact c = cont c (\r => leaf r)

instance Functor (PIO w) where
  map m (leaf x)     = leaf (m x)
  map m (cont ca pa) = cont ca (\ra => map m (pa ra))

instance Applicative (PIO w) where
  pure = leaf

  (leaf f)     <*> x = map f x
  (cont cf pf) <*> x = cont cf (\rf => pf rf <*> x)

instance Monad (PIO w) where
  (leaf a)     >>= f = f a
  (cont ca pa) >>= f = cont ca (\ra => pa ra >>= f)

partial
repeat : {w : IWorld cs rs} -> a -> (a -> PIO w (Either b a)) -> PIO w b
repeat init p = (p init) >>= q
  where q (Left b) = leaf b
        q (Right a) = repeat a p

partial
while : {w : IWorld cs rs} -> b -> (b -> Either (PIO w b) (PIO w a)) -> PIO w a
while init p = f (p init)
  where f (Left b) = b >>= (\b' => while b' p)
        f (Right a) = a

partial
redirect : {w : IWorld cs rs} ->
           {w' : IWorld cs' rs'} ->
           PIO w a ->
           ((c:cs) -> PIO w' (rs c)) ->
           PIO w' a
redirect (leaf x) q = leaf x
redirect (cont c p) q = do qc <- q c
                           redirect (p qc) q

namespace RWExample
  -- An initial short example, from the papeer
  data Command = Write String
               | Read

  total
  Response : Command -> Type
  Response (Write _) = Unit
  Response (Read)    = String

  MyWorld : IWorld Command Response
  MyWorld = MkIWorld Command Response

  data XXX = Success | Fail

  -- equivalent to the example in the paper.
  wurzel : PIO MyWorld XXX
  wurzel = do interact (Write "Password (root):")
              s <- interact Read
              if s == "Wurzel"
              then return Success
              else return Fail

  partial
  execute : PIO MyWorld a -> IO a
  execute (leaf x) = return x
  execute (cont (Write x) p) = do putStrLn x
                                  execute (p ())
  execute (cont Read p) = do s <- getLine
                             execute (p s)


namespace StateExample

  -- A short example, to show that this is arbitrarily expandable: The
  -- state monad, as its own little world. Pretty simple in the end.
  data StateCommands s = Get | Put s

  total
  StateResponses : StateCommands s -> Type
  StateResponses {s} Get = s
  StateResponses (Put _) = Unit

  StateWorld : (s:Type) -> IWorld (StateCommands s) StateResponses
  StateWorld s = MkIWorld (StateCommands s) StateResponses

  partial
  execute : PIO (StateWorld s) a  -> State s a
  execute (leaf x)         = return x
  execute (cont Get p)     = execute (p !get)
  execute (cont (Put x) p) = execute (p !(put x))
