module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import IRTS.Compiler
import IRTS.CodegenErlang

import System.Environment
import System.Exit

-- TODO: --path gives path to erlang files.

import Paths_idris_erlang

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   show_path :: Bool }

showUsage = do putStrLn "Usage: idris-erlang [--path] <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "idris.erl" False) xs
  where
    process opts ("--path":_) = opts {show_path = True}
    process opts ("-o":o:xs)  = process (opts {output = o}) xs
    process opts (x:xs)       = process (opts {inputs = x:inputs opts}) xs
    process opts []           = opts

erl_main :: Opts -> Idris ()
erl_main opts = do --addPkgDir "erlang" -- Hax
                   elabPrims
                   --loadModule "ErlTypes" -- Hax
                   --addAutoImport "ErlTypes" -- Hax
                   loadInputs (inputs opts) Nothing
                   mainProg <- elabMain
                   ir <- compile (Via "erlang") (output opts) (Just mainProg)
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
