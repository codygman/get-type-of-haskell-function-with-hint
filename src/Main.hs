module Main where

import Data.List
import Control.Monad
import Language.Haskell.Interpreter

main :: IO ()
main = do r <- runInterpreter testHint
          case r of
            Left err -> putStrLn $ errorString err
            Right () -> return ()

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

say :: String -> Interpreter ()
say = liftIO . putStrLn

say' = liftIO . print

emptyLine :: Interpreter ()
emptyLine = say ""

-- observe that Interpreter () is an alias for InterpreterT IO ()
testHint :: Interpreter ()
testHint = do
      loadModules ["Sample.hs"]
      say "What is exported by module X?"
      mapM_ say' =<< getModuleExports "X"
      emptyLine
      say =<< typeOf "x"
      say "done"
