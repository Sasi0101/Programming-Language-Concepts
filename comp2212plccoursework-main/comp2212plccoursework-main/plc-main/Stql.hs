module Main where

import Token
import Grammar
import Eval
import Control.Exception
import System.Environment
import System.Directory.Internal.Prelude (getArgs, stderr, hPutStr)
import LanguageTypes



main :: IO()
main = catch main' noParse

main' = do
    args <- getArgs
    prog <- readFile (head args)
    let typechek = typeCheck $ parse $ alexScanTokens prog
    let parsed = parse $ alexScanTokens prog
    let result = (finalEval $ parse $ alexScanTokens prog)
    --let result' = eval $ parse $ alexScanTokens prog
    if (typechek == "The query passed the type checking") then (putStrLn result) else (hPutStr stderr typechek)
    --print parsed
    --putStrLn result
    --print result'

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()




