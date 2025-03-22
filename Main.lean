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

def runCommand (p : Parsed) : IO UInt32 := do
  let file := FilePath.mk <| p.positionalArg! "file" |>.as! String
  let options := impl_parseOptions p
  if p.hasFlag "initializer" then unsafe
    enableInitializersExecution
  let (_, state) ← withFile file do
    if let some module ← searchModuleNameOfFileName file (← initSrcSearchPath) then
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
    u, augmentation : String; "Augmented proofs"
    i, initializer;  "Execute initializers"

  ARGS:
    file : String;  "File to process"
]

def main (args : List String) : IO UInt32 :=
  jixiaCommand.validate args
