/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Metalib.Load
import Analyzer.Types
import Analyzer.Process
import Cli

open System Cli
open Lean hiding Options
open Parser hiding mkIdent ident
open Elab Command Term
open Analyzer

/-- Global IO.Ref to store Lean `-D` options parsed from command line args. -/
initialize leanOptsRef : IO.Ref Lean.Options ← IO.mkRef {}

/-- Parse `-D` flags from command line args.
    Returns (leanOptions, remainingArgs).
    Supports formats: `-Dkey=value`, `-Dkey` (shorthand for `-Dkey=true`).
    Value types: `true`/`false` → Bool, numeric → Nat, otherwise → String. -/
def parseDFlags (args : List String) : IO (Lean.Options × List String) := do
  let mut opts : Lean.Options := {}
  let mut remaining : Array String := #[]
  for arg in args do
    if arg.startsWith "-D" then
      let optStr := arg.drop 2
      let parts := optStr.toString.splitOn "="
      match parts with
      | [key, value] =>
        let name := key.toName
        if value == "true" then
          opts := opts.setBool name true
        else if value == "false" then
          opts := opts.setBool name false
        else if let some n := value.toNat? then
          opts := opts.set name n
        else
          opts := opts.set name value
      | [key] =>
        -- `-Dkey` without value means `-Dkey=true`
        opts := opts.setBool key.toName true
      | _ => throw (IO.userError s!"Invalid -D option: {arg}")
    else
      remaining := remaining.push arg
  return (opts, remaining.toList)

def parseFlag (p : Parsed) (s : String) : PluginOption :=
  match p.flag? s with
  | none => .ignore
  | some f => .json <| .mk <| f.as! String

elab "impl_parseOptions" : term => do
  let param ← mkFreshBinderName
  let fields ← Process.plugins.mapM fun (name, _) => do
    let lval ← `(structInstLVal| $(mkIdent name):ident)
    let nameStr := Syntax.mkStrLit name.getString!
    let val ← `(parseFlag $(mkIdent param) $nameStr)
    return ← `(structInstField| $lval := $val)
  let val ← `(fun $(mkIdent param) => { $fields* })
  let type ← `(Parsed → Options)
  elabTerm val (← elabTerm type none)

/--
Suppose we continue the example, with `p` having the value given below.
then the first line parses `TestFile.lean` into a `FilePath`.

`impl_parseOptions` as implemented above, is now elaborated as (something equivalent to?)
fun p ↦ {
  module := parseFlag p "module"
  declaration := parseFlag p "declaration"
  symbol := parseFlag p "symbol"
  elaboration := parseFlag p "elaboration"
  line := parseFlag p "line"}
where the right hand side takes the value with type `Analyzer.Options`
See `Analyzer.Process` for how this is defined.
-/
def runCommand (p : Parsed) : IO UInt32 := do
  let file := FilePath.mk <| p.positionalArg! "file" |>.as! String
  let options := impl_parseOptions p
    -- If there is a flag with `longName` "initializer", then `enableInitializersExecution` will be executed.
  if p.hasFlag "initializer" then unsafe
    enableInitializersExecution
  -- Read Lean `-D` options that were parsed in `main`
  -- Always set `Elab.async=true` to match `lean` binary behavior (see `runFrontend`).
  -- Without this, some files fail with kernel errors due to a Lean bug in synchronous mode.
  let leanOpts := Elab.async.setIfNotSet (← leanOptsRef.get) true
  -- This line runs the file `file` and passes `options` as an argument.
  -- jump to `Analyzer.Process` for how `run` is implemented.
  let (_, state) ← withFile file (initState := fun header messages inputCtx =>
    handleHeader header messages inputCtx leanOpts) do
    if let some module ← searchModuleNameOfFileName file (← getSrcSearchPath) then
      Frontend.runCommandElabM <| modifyEnv (·.setMainModule module)
    run options

  -- additional plugins that does not fit into the general framework
  let optionAST := parseFlag p "ast"
  optionAST.output state.commands

  let optionSymbol := parseFlag p "symbol"
  if optionSymbol.isPresent then
    optionSymbol.output (← Process.Symbol.getResult file)

  let messages := state.commandState.messages
  messages.forM fun message => do
    IO.eprint (← message.toString)
  return 0

def jixiaCommand : Cmd := `[Cli|
  jixia VIA runCommand;
  "A static analysis tool for Lean 4."

  FLAGS:
    m, module : String;  "Module info"
    d, declaration : String;  "Declaration info"
    s, symbol : String;  "Symbol info"
    e, elaboration : String;  "Elaboration info"
    l, line : String;  "Line info"
    a, ast : String;  "AST"
    i, initializer;  "Execute initializers"

  ARGS:
    file : String;  "File to process"
]

/--
If given an input in the form
`lake exe jixia -d Test.decl.json -e Test.elab.json Test.lean`
then the `args` in the main function is `["-d", "Test.decl.json", "-e", "Test.elab.json", "Test.lean"]`,

It then passes through the function `Cli.Cmd.parse` called inside `Cli.Cmd.validate`,
and becomes a term of type `Parsed` which has the value: {
  cmd := c
  parent? := None
  flags = #[⟨⟨"d", "declaration", "Declaration info", String⟩, "Test.decl.json"⟩,
            ⟨⟨"e", "elaboration", "Elaboration info", String⟩, "Test.elab.json"⟩]
  positionalArgs = #[⟨⟨"file", "File to process", String⟩, "Test.lean"⟩]
  variableArgs = #[]
  }
`Cli.Cmd.validate` then runs the function as `runCommand p` (where `p` denotes the term of type `Parsed` above).
see the explanation for `runCommand` for what is executed after that.

-/
def main (args : List String) : IO UInt32 := do
  let (leanOpts, args) ← parseDFlags args
  leanOptsRef.set leanOpts
  jixiaCommand.validate args
