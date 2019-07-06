[![Build Status](https://travis-ci.org/michaeljklein/ghc-edit-plugin.svg?branch=master)](https://travis-ci.org/michaeljklein/ghc-edit-plugin)

# ghc-edit-plugin

Create GHC plugins using composable `Lens`-like source edits.


## Control.Edit

- `Control.Edit` provides a `Lens`-like edit type and tools for editing Haskell source.
- `Control.Edit.Plugin` provides a method to convert an `Edited` action to a `Plugin`:

```haskell
editToPlugin :: ([CommandLineOption] -> ModSummary -> Edited HsParsedModule) -> Plugin
```


## Control.Edit.HsModule

Make a splice for `addTyClDeclTypeNameSplices` by using `spliceApp`
with the given TH function names, applied to each `TyClDecl` name.

```haskell
addTyClDeclTypeNameSpliceFunctions ::
     (TyClDeclTypeName -> EditM [(FastString, IdP GhcPs)])
  -> Edited (HsModule GhcPs)
```


### Example Plugin

An example plugin is provided in `Control.Edit.Plugin.Echo`:
it prints the name and type (e.g. type family, data, etc.)
of each type declaration.

To use:

```haskell
{-# OPTIONS_GHC -fplugin Control.Edit.Plugin.Echo #-}
```

