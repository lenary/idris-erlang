module Main

import ErlPrelude

main : EIO ()
main = do printLn (the (List Int) [])
          printLn (the (List Int) [1,2,3,4])
          x <- lists_reverse [1,2,3,4]
          printLn x

          printLn (1,"astreing")
          printLn ()
