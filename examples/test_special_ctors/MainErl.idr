module Main

import ErlPrelude

print_raw : a -> EIO ()
print_raw x = do print_raw' (MkERaw x)
                 return ()
  where print_raw' : (ErlRaw a) -> EIO ()
        print_raw' r = foreign FFI_Erl "io:format" (String -> List (ErlRaw a) -> EIO ()) "~p~n" [r]

lists_reverse : List Int -> EIO (List Int)
lists_reverse = foreign FFI_Erl "lists:reverse" (List Int -> EIO (List Int))

lists_map : (Int -> EIO Char) -> List Int -> EIO (List Char)
lists_map f = foreign FFI_Erl "lists:map" ((ErlFn (Int -> EIO Char)) -> List Int -> EIO (List Char)) (MkErlFun f)


main : EIO ()
main = do printLn (the (List Int) [])
          print_raw (the (List Int) [1,2,3,4])
          x <- lists_reverse [1,2,3,4]
          print_raw x

          print_raw "astring"
          print_raw 3
          print_raw 3.54

          print_raw ()

          y <- lists_map (\x => getChar) x
          print_raw y
