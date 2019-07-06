{-# OPTIONS_GHC -fplugin Control.Edit.Plugin.Echo #-}
{-# OPTIONS_GHC -fplugin Control.Edit.Plugin.ImportDataBool #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Control.Edit.Plugin.TyDeclNames #-}

newtype FooDecl = FooConstr { unFoo :: FooDecl }

type family FooFam a

ensureDataBoolImported :: ()
ensureDataBoolImported =
  bool () () True

main :: IO ()
main = do
  putStrLn ""
  print ("FooDecl name", fooDeclName)
  print ("FooFam name", fooFamName)
  putStrLn "You should see the lines:"
  putStrLn "Control.Edit.Plugin.Echo: (DataTypeName,\"FooDecl\")"
  putStrLn "Control.Edit.Plugin.Echo: (FamTypeName,\"FooFam\")"
  putStrLn "(if the test passed)"

