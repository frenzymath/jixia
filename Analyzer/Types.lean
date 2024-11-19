/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean

/-!
Note on source ranges: we encode all source positions/ranges by byte.
-/

open Lean hiding HashSet
open Elab Term
open Std (HashSet)

namespace Analyzer

structure ScopeInfo where
  varDecls : Array String
  includeVars : Array Name
  omitVars : Array Name
  levelNames : Array Name

/-- Information about a declaration command in source file. -/
structure BaseDeclarationInfo where
  kind : String
  ref : Syntax
  /-- Syntax node corresponding to the name of this declaration. -/
  id : Syntax
  /-- The identifier contained in `id`, even if it is generated. -/
  name : Name
  fullname : Name
  modifiers : Modifiers
  signature : Syntax
  params : Array BinderView
  type : Option Syntax
  value : Option Syntax
  scopeInfo : ScopeInfo

structure InductiveInfo extends BaseDeclarationInfo where
  constructors : Array BaseDeclarationInfo

inductive DeclarationInfo where
  | ofBase : BaseDeclarationInfo → DeclarationInfo
  | ofInductive : InductiveInfo → DeclarationInfo

def DeclarationInfo.toBaseDeclarationInfo : DeclarationInfo → BaseDeclarationInfo
  | .ofBase info => info
  | .ofInductive info => info.toBaseDeclarationInfo

structure SymbolInfo where
  kind : String
  name : Name
  type : String
  /-- Names of constants that the type of this symbol references.  Mathematically, this roughly means "notions
  needed to state the theorem". -/
  typeReferences : HashSet Name
  /-- In the same spirit as above, "notions used in the proof of the theorem". `null` if this symbol has no value. -/
  valueReferences : Option (HashSet Name)
  /-- Whether the type of this symbol is a proposition. -/
  isProp : Bool

structure Variable where
  id : Name
  name : Name
  type : String
  value? : Option String
  isProp : Bool

structure Goal where
  tag : Name
  context : Array Variable
  mvarId : Name
  type : String
  isProp : Bool
  extra? : Option Json := none

structure TacticElabInfo where
  /-- Names referenced in this tactic, including constants and local hypotheses. -/
  references : HashSet Name
  before : Array Goal
  after : Array Goal
  extra? : Option Json := none

inductive SpecialValue where
  | const : Name → SpecialValue
  | fvar : FVarId → SpecialValue

structure TermElabInfo where
  context : Array Variable
  type : String
  expectedType : Option String
  value : String
  special? : Option SpecialValue

structure MacroInfo where
  expanded : Syntax

inductive ElaborationInfo where
  | term : TermElabInfo → ElaborationInfo
  | tactic : TacticElabInfo → ElaborationInfo
  | macro : MacroInfo → ElaborationInfo
  | simple : String → ElaborationInfo

inductive ElaborationTree where
  | mk (info : ElaborationInfo) (ref : Syntax) (children : Array ElaborationTree) : ElaborationTree

structure LineInfo where
  start : String.Pos
  state : Array Goal


structure AugmentInfo where
  ref : Syntax
  formalStatement : String
  formalStatementFullname : String
  formalProof : String
  formalProofFullname : String
  openedNamespaces : String



end Analyzer
