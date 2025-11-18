/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Analyzer.Process.Common
import Analyzer.Types
import Analyzer.Plugin
import Analyzer.Output

open Lean Elab Meta
open Parser hiding mkIdent ident
open Command Term Frontend

namespace Analyzer

inductive PluginOption where
  | ignore : PluginOption
  | json : System.FilePath → PluginOption

def PluginOption.isPresent : PluginOption → Bool
  | .ignore => false
  | _ => true

def PluginOption.output {α : Type _} [ToJson α] (a : α) : PluginOption → IO Unit
  | .ignore => return ()
  | .json path =>
    IO.FS.withFile path .write fun h => h.putStrLn <| toJson a |>.compress

/-
  The following command defines the type that is shown as `Analyzer.Options`.
  check `impl_parseOptions` in `Main.lean` for an example.
  The code will be elaborated as

  structure Options where
    module : PluginOption
    declaration : PluginOption
    symbol : PluginOption
    elaboration : PluginOption
    line : PluginOption

  which defines a new structure.
-/
run_cmd
  let fieldDecls ← Process.plugins.mapM fun (name, _) => do
    return (← `(structExplicitBinder| ($(mkIdent name) : PluginOption)))
  let body ← `(structFields| $fieldDecls*)
  let cmd ← `(structure $(mkIdent `Options) where $body:structFields)
  elabCommand cmd

/-
  This piece of code elaborates `impl_onLoad` as
  fun (options : Options) ↦ do
    if options.declaration.isPresent then Declarations.onLoad
    if options.elaboration.isPresent then Elaboration.onLoad
    if options.line.isPresent then Line.onLoad
  `if let some p := plugin.onLoad then` checks whether the field `onLoad` exists for the
  corresponding entry, and only generates code for those with `onLoad`.
-/
elab "impl_onLoad" : term => do
  let param ← mkFreshBinderName
  let body ← Process.plugins.foldlM (init := #[]) fun a (name, plugin) => do
    if let some p := plugin.onLoad then do
      let cond := mkIdent (param ++ name ++ (Name.mkSimple "isPresent"))
      return a.push (← `(doSeqItem| if $cond then $(mkIdent p):term))
    else
      return a
  let term ← `(fun $(mkIdent param) => do $body*)
  let type ← `(Options → CommandElabM Unit)
  elabTerm term (← elabTerm type none)

/-
  This piece of code elaborates `impl_process` as
  fun (options : Options) ↦ do
    if options.module.isPresent then options.module.output (← Module.getResult)
    ...(another 4 lines are ommitted here)
-/
elab "impl_process" : term => do
  let param ← mkFreshBinderName
  let body ← Process.plugins.mapM fun (name, plugin) => do
    let cond := mkIdent (param ++ name ++ (Name.mkSimple "isPresent"))
    let action := mkIdent (param ++ name ++ (Name.mkSimple "output"))
    let term := mkIdent plugin.getResult
    return (← `(doSeqItem| if $cond then
      $action:term (← $term)
    ))
  let term ← `(fun $(mkIdent param) => do $body*)
  let type ← `(Options → CommandElabM Unit)
  elabTerm term (← elabTerm type none)

def run (options : Options) : FrontendM Unit := do
  runCommandElabM <| impl_onLoad options
  processCommands
  runCommandElabM <| withScope (fun scope => { scope with opts := setPPOptions scope.opts } ) <|
    impl_process options

end Analyzer
