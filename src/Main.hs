module Main where

import Language.Haskell.Interpreter

newtype ModName = ModName ModuleName
newtype FunctionName = FunctionName String
newtype FunctionType = FunctionType String

getTypeOf :: FilePath -> ModName -> FunctionName -> Interpreter (FunctionType)
getTypeOf fp mn fn = do
  loadModules [fp]
  let (ModName mn') = mn
  setTopLevelModules [mn']
  let (FunctionName fn') = fn
  FunctionType <$> (typeOf fn')

getTypeStringOf :: FilePath -> ModName -> FunctionName -> IO (String)
getTypeStringOf fp mn fn = do
  r <- runInterpreter $ getTypeOf fp mn fn
  case r of
    Left err -> pure (show err)
    Right fnTy -> pure . (\(FunctionType fty) -> fty) $ fnTy

main :: IO ()
main = putStrLn =<< getTypeStringOf "Sample.hs" (ModName "X") (FunctionName "g")
