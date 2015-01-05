module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import IRTS.Compiler
import IRTS.CodegenErlang

import System.Environment
import System.Exit

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath }

showUsage = do putStrLn "Usage: idris-erlang <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "idris.erl") xs
  where
    process opts ("-o":o:xs) = process (opts {output = o}) xs
    process opts (x:xs)      = process (opts {inputs = x:inputs opts}) xs
    process opts []          = opts

erl_main :: Opts -> Idris ()
erl_main opts = do elabPrims
                   loadInputs (inputs opts) Nothing
                   mainProg <- elabMain
                   ir <- compile (Via "erlang") (output opts) mainProg
                   runIO $ codegenErlang ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
            then showUsage
            else runMain (erl_main opts)
