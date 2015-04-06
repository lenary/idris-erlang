module Main

import ErlPrelude
import Erlang.Process
import Erlang.RPC

counter_rpc : Int -> Maybe Int -> Process (Maybe Int) (Int,Int)
counter_rpc i Nothing  = return (i,i)
counter_rpc i (Just x) = return (i+x,i+x)

test_proc : Process Int ()
test_proc = do counter <- spawn_rpc counter_rpc 0
               x <- rpc counter (Just 3)
               lift . putStrLn $ "x (3) = " ++ (cast x)
               y <- rpc counter (Nothing)
               lift . putStrLn $ "y (3) = " ++ (cast x)
               return ()

main : EIO ()
main = run test_proc
