module Main where

import Phil.Language.Repl (consoleRepl)
import Polysemy (runM)

main :: IO ()
main = runM consoleRepl
