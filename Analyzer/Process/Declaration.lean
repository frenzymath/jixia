/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Analyzer.Types

open Lean Elab Command Parser Term
open TSyntax.Compat

namespace Analyzer.Process.Declaration

-- simplified version of Elab.mkDeclName
def getFullname (modifiers : Modifiers) (name : Name) : CommandElabM Name := do
  let currNamespace ← getCurrNamespace
  let view := extractMacroScopes name
  let declName := if (`_root_).isPrefixOf view.name then
      { view with name := name.replacePrefix `_root_ Name.anonymous }.review
    else
      currNamespace ++ name
  return if let .private := modifiers.visibility then
    mkPrivateName (← getEnv) declName
  else
    declName

-- taken from Lean.Elab.Binders, where a bunch of functions are defined to be private
-- (for no good reason at all)
def expandBinderType (ref : Syntax) (stx : Syntax) : Syntax :=
  if stx.getNumArgs == 0 then
    mkHole ref
  else
    stx[1]

def expandBinderIdent (stx : Syntax) : TermElabM Syntax :=
  match stx with
  | `(_) => mkFreshIdent stx (canonical := true)
  | _    => pure stx

def expandOptIdent (stx : Syntax) : TermElabM Syntax := do
  if stx.isNone then
    let id ← withFreshMacroScope <| MonadQuotation.addMacroScope `inst
    return mkIdentFrom stx id
  else
    return stx[0]

def expandBinderModifier (type : Syntax) (optBinderModifier : Syntax) : TermElabM Syntax := do
  if optBinderModifier.isNone then
    return type
  else
    let modifier := optBinderModifier[0]
    let kind     := modifier.getKind
    if kind == `binderDefault then
      let defaultVal := modifier[1]
      `(optParam $type $defaultVal)
    else if kind == `binderTactic then
      let tac := modifier[2]
      let name ← declareTacticSyntax tac
      `(autoParam $type $(mkIdentFrom tac name))
    else
      throwUnsupportedSyntax

def getBinderIds (ids : Syntax) : TermElabM (Array Syntax) :=
  ids.getArgs.mapM fun id =>
    let k := id.getKind
    if k == identKind || k == `Lean.Parser.Term.hole then
      return id
    else
      throwErrorAt id "identifier or `_` expected"

def toBinderViews (stx : Syntax) : TermElabM (Array BinderView) := do
  let k := stx.getKind
  if stx.isIdent || k == ``hole then
    -- binderIdent
    return #[{ ref := stx, id := (← expandBinderIdent stx), type := mkHole stx, bi := .default }]
  else if k == ``explicitBinder then
    -- `(` binderIdent+ binderType (binderDefault <|> binderTactic)? `)`
    let ids ← getBinderIds stx[1]
    let type        := stx[2]
    let optModifier := stx[3]
    ids.mapM fun id => do pure { ref := id, id := (← expandBinderIdent id), type := (← expandBinderModifier (expandBinderType id type) optModifier), bi := .default }
  else if k == ``implicitBinder then
    -- `{` binderIdent+ binderType `}`
    let ids ← getBinderIds stx[1]
    let type := stx[2]
    ids.mapM fun id => do pure { ref := id, id := (← expandBinderIdent id), type := expandBinderType id type, bi := .implicit }
  else if k == ``strictImplicitBinder then
    -- `⦃` binderIdent+ binderType `⦄`
    let ids ← getBinderIds stx[1]
    let type := stx[2]
    ids.mapM fun id => do pure { ref := id, id := (← expandBinderIdent id), type := expandBinderType id type, bi := .strictImplicit }
  else if k == ``instBinder then
    -- `[` optIdent type `]`
    let id ← expandOptIdent stx[1]
    let type := stx[2]
    return #[ { ref := id, id := id, type := type, bi := .instImplicit } ]
  else
    throwUnsupportedSyntax
-- end of Lean.Elab.Binders

-- see Elab.elabInductive, which is of course also private
def getConstructorInfo (parentName : Name) (stx : Syntax) : CommandElabM BaseDeclarationInfo := do
    -- def ctor := leading_parser optional docComment >> "\n| " >> declModifiers >> rawIdent >> optDeclSig
    let mut modifiers ← elabModifiers stx[2]
    if let some leadingDocComment := stx[0].getOptional? then
      modifiers := { modifiers with docString? := TSyntax.getDocString ⟨leadingDocComment⟩ }
    let id := stx[3]
    let name := id.getId
    let fullname ← getFullname modifiers <| parentName ++ name
    let (binders, type) := expandOptDeclSig stx[4]
    let params ← liftTermElabM <| binders.getArgs.concatMapM toBinderViews
    return {
      kind := "ctor",
      ref := stx,
      id,
      name,
      fullname,
      modifiers,
      params,
      type,
      value := .none,
      tactics := #[],
    }

-- see Elab.elabDeclaration
def getDeclarationInfo (stx : Syntax) : CommandElabM DeclarationInfo := do
  let modifiers ← elabModifiers stx[0]
  let decl := stx[1]
  let kind := decl.getKind

  let .str _ kindStr := kind | unreachable!

  let (id, binders, type, value) := ← if isDefLike decl then do
    let defView ← mkDefView modifiers decl
    return (defView.declId, defView.binders, defView.type?, some defView.value)
  else
    let (binders, type) := match kind with
    | ``Command.«axiom» =>
      expandDeclSig decl[2] |>.map id some
    | ``Command.«inductive»
    | ``Command.classInductive =>
      expandOptDeclSig decl[2]
    | ``Command.«structure» =>
      (decl[2], decl[4])
    | _ => unreachable!
    return (decl[1], binders, type, none)

  let name := id[0].getId
  let fullname ← getFullname modifiers name
  let params ← liftTermElabM <| binders.getArgs.concatMapM toBinderViews

  let tactics := if let some value := value then
    if value.getKind == ``Command.declValSimple ∧
        value[1].getKind == ``Term.byTactic ∧
        value[1][1].getKind == ``Tactic.tacticSeq then
      let tacticSeq := value[1][1][0]
      tacticSeq[0].getArgs
    else
      #[]
  else
    #[]

  let info := {
    kind := kindStr,
    ref := stx,
    id,
    name,
    fullname,
    modifiers,
    params,
    type,
    value,
    tactics,
   : BaseDeclarationInfo}

  if kind == ``Command.«inductive» ∨ kind == ``Command.classInductive then
    let constructors ← decl[4].getArgs.mapM <| getConstructorInfo fullname
    return .ofInductive { info with constructors }

  return .ofBase info

initialize declRef : IO.Ref (Array DeclarationInfo) ← IO.mkRef #[]

def handleDeclaration (stx : Syntax) : CommandElabM Unit :=
  withEnableInfoTree false do
    let info ← getDeclarationInfo stx
    declRef.modify fun a => a.push info
    throwUnsupportedSyntax

def onLoad : CommandElabM Unit := do
  modifyEnv fun env => commandElabAttribute.ext.addEntry env {
    key := ``Parser.Command.declaration,
    declName := ``handleDeclaration,
    value := handleDeclaration,
  }

def getResult : CommandElabM (Array DeclarationInfo) := declRef.get

end Analyzer.Process.Declaration
