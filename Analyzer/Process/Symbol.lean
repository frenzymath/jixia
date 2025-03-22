/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Analyzer.Types

open Lean hiding HashSet
open Elab Term Command Frontend Parser
open Std (HashSet)

namespace Analyzer.Process.Symbol

def references (expr : Expr) : HashSet Name :=
  go expr (.empty, .empty) |>.2.2
where
  go (expr : Expr) : StateM (HashSet UInt64 × HashSet Name) Unit := do
    let data : UInt64 := expr.data
    if !(← get).1.contains data then do
      modify fun (v, r) => (v.insert data, r)
      match expr with
      | .bvar _ => pure ()
      | .fvar _ => pure ()
      | .mvar _ => unreachable!
      | .sort _ => pure ()
      | .const name _ => modify fun (v, r) => (v, r.insert name)
      | .app e₁ e₂ => do go e₁; go e₂
      | .lam _ _ e _ => go e
      | .forallE _ t e _ => do go t; go e
      | .letE _ _ e₁ e₂ _ => do go e₁; go e₂
      | .lit _ => pure ()
      | .mdata _ e => go e
      | .proj _ _ e => go e

def getSymbolInfo (name : Name) (info : ConstantInfo) : TermElabM SymbolInfo := do
  let kind := match info with
    | .axiomInfo _ => .«axiom»
    | .defnInfo _ => .definition
    | .thmInfo _ => .«theorem»
    | .opaqueInfo _ => .«opaque»
    | .quotInfo _ => .quotient
    | .inductInfo _ => .«inductive»
    | .ctorInfo _ => .constructor
    | .recInfo _ => .recursor
  let type := info.toConstantVal.type
  let isProp ← try
    let prop := Expr.sort 0
    discard <| ensureHasType prop type
    pure true
  catch _ =>
    pure false
  let type ← try
    let format ← PrettyPrinter.ppExpr type |>.run'
    pure format.pretty
  catch _ =>
    pure type.dbgToString
  let typeReferences := references info.type
  let valueReferences := info.value?.map references
  return { kind, name, type, typeReferences, valueReferences, isProp }

def getResult (path : System.FilePath) : IO (Array SymbolInfo) := do
  let sysroot ← findSysroot
  -- initSearchPath returns IO.appDir / ".." / "src" / "lean" as its last path, which does not make any sense
  -- apparently renaming "index" to "idx" is far more important than finding bugs like these and making better build systems
  -- seriously, what is wrong with the Lean core devs???
  let ssp := (← initSrcSearchPath) ++ [sysroot / "src" / "lean"]
  let module := (← searchModuleNameOfFileName path ssp).get!
  let config := {
    fileMap := default,
    fileName := path.toString,
  : Core.Context}
  unsafe enableInitializersExecution
  let env ← importModules #[{ module }] .empty
  let index := env.allImportedModuleNames.getIdx? module
  let f a name info := do
    if env.getModuleIdxFor? name != index then return a
    let (si, _, _) ← getSymbolInfo name info |>.run' |>.toIO config { env }
    return a.push si
  let a ← env.constants.map₁.foldM f #[]
  env.constants.map₂.foldlM f a

end Analyzer.Process.Symbol
