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
interact c = cont c (\r => Delay (leaf r))


instance Functor (PIO w) where
  -- map : (m : a -> b) -> f a -> f b
  map m (leaf x)     = leaf (m x)
  map m (cont ca pa) = cont ca (\ra => Delay (map m (pa ra)))


instance Applicative (PIO w) where
  -- pure  : a -> f a
  pure = leaf
  -- (<$>) : f (a -> b) -> f a -> f b
  (leaf f) <*> (leaf a) = leaf (f a)
  (leaf f) <*> (cont ca pa) = cont ca (\ra => Delay (map f (pa ra)))
  (cont cf pf) <*> (leaf a) = cont cf (\rf => Delay (pf rf <*> (leaf a)))
  (cont cf pf) <*> (cont ca pa) = cont cf
                                       (\rf => Delay (cont ca
                                                           (\ra => Delay (pf rf <*> pa ra))))


instance Monad (PIO w) where
  -- (>>=) : m a -> (a -> m b) -> m b
  (leaf a) >>= f = f a
  (cont ca pa) >>= f = cont ca (\ra => Delay (Force (pa ra) >>= f))



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



redirect : {w : IWorld cs rs} -> {w' : IWorld cs' rs'} -> PIO w a -> ((c:cs) -> PIO w' (rs c)) -> PIO w' a
redirect (leaf x) q = leaf x
redirect (cont c p) q = redirect (p !(q c)) q


-- Execution is parameterised over both the world and the monad you
-- want to execute in, meaning you may be able to execute the same
-- world-ly program in different monads, if it's setup correctly.
class Monad m => Executable (w : IWorld cs rs) (m : Type -> Type) where
  partial
  execute : PIO w a -> m a


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
            s <- interact Parameterised.Read
            if s == "Wurzel"
            then return Success
            else return Fail


instance Executable MyWorld IO where
  execute (leaf x) = return x
  execute (cont (Write x) p) = do putStrLn x
                                  execute (p ())
  execute (cont Read p) = do s <- getLine
                             execute (p s)

-- A short example, to show that this is arbitrarily expandable: The
-- state monad, as its own little world. Pretty simple in the end.
data StateCommands s = Get | Put s
StateResponses : StateCommands s -> Type
StateResponses {s} Get = s
StateResponses (Put _) = Unit

StateWorld : (s:Type) -> IWorld (StateCommands s) StateResponses
StateWorld s = MkIWorld (StateCommands s) StateResponses

instance Executable (StateWorld s) (StateT s Identity) where
  execute (leaf x) = return x
  execute (cont Get p) = do execute (p !get)
  execute (cont (Put x) p) = do execute (p !(put x))
