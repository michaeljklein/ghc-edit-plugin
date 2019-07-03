# ghc-edit-plugin

Create GHC plugins using composable `Lens`-like source edits.


## Control.Edit

- `Control.Edit` provides a `Lens`-like edit type and tools for editing Haskell source.
- `Control.Edit.Plugin` provides a method to convert an `Edited` action to a `Plugin`:

```haskell
editToPlugin :: ([CommandLineOption] -> ModSummary -> Edited HsParsedModule) -> Plugin
```


### Example Plugin

An example plugin is provided in `Control.Edit.Plugin.Echo`:
it prints the name and type (e.g. type family, data, etc.)
of each type declaration.

To use:

```haskell
{-# OPTIONS_GHC -fplugin Control.Edit.Plugin.Echo #-}
```

