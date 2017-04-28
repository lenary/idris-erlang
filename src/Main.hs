module Main where

import Idris.AbsSyntax
import Idris.Core.TT
import Idris.ElabDecls
import Idris.Main
import Idris.ModeCommon
import Idris.REPL

import IRTS.Compiler
import IRTS.CodegenErlang

import System.Environment
import System.Exit

import Control.Monad (liftM)

import Paths_idris_erlang

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   show_path :: Bool,
                   interface :: Bool }

erlDefaultOpts :: Opts
erlDefaultOpts = Opts { inputs = [], output = "main.erl", show_path = False, interface = False}

showUsage = do putStrLn "Usage: idris-erlang [--interface] [--path] <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process erlDefaultOpts xs
  where
    process opts ("--interface":xs) = process (opts { interface = True }) xs
    process opts ("--path":_) = opts {show_path = True}
    process opts ("-o":o:xs)  = process (opts {output = o}) xs
    process opts (x:xs)       = process (opts {inputs = x:inputs opts}) xs
    process opts []           = opts

erl_main :: Opts -> Idris ()
erl_main opts = do elabPrims
                   loadInputs (inputs opts) Nothing
                   mainProg <- if interface opts
                               then return Nothing
                               else liftM Just elabMain
                   ir <- compile (Via IBCFormat "erlang") (output opts) mainProg
                   runIO $ codegenErlang ir

main :: IO ()
main = do opts <- getOpts
          data_dir <- getDataFileName "irts"
          if (show_path opts)
            then putStrLn ("-pa " ++ data_dir ++ "") >> exitWith ExitSuccess
            else return ()
          if (null (inputs opts))
            then showUsage
            else runMain (erl_main opts)
