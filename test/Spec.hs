{-# OPTIONS_GHC -fplugin Control.Edit.Plugin.Echo #-}
{-# LANGUAGE TypeFamilies #-}

newtype FooDecl = FooConstr { unFoo :: FooDecl }

type family FooFam a

main :: IO ()
main = do
  putStrLn "You should see the lines:"
  putStrLn "Control.Edit.Plugin.Echo: (DataTypeName,\"FooDecl\")"
  putStrLn "Control.Edit.Plugin.Echo: (FamTypeName,\"FooFam\")"
  putStrLn "(if the test passed)"

