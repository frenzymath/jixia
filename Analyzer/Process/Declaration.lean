/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda, Blueberry
-/
import Lean
import Analyzer.Types
import Lean.Elab


open Lean Elab Command Parser Term PrettyPrinter ScopedEnvExtension
open TSyntax.Compat

namespace Analyzer.Process.Declaration


def getActiveNamespaces (env : Environment) : IO (Array Name) := do
  let exts ← scopedEnvExtensionsRef.get
  let mut nsSet : NameSet := {}
  for ext in exts do
    match ext.ext.getState env |>.stateStack with
    | top :: _ =>
      for ns in top.activeScopes.toList do
        nsSet := nsSet.insert ns
    | _ => pure ()
  return nsSet.toList.toArray


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

-- Lean.Elab.Declaration
/-- Return `true` if `stx` is a `Command.declaration`, and it is a definition that always has a name. -/
private def isNamedDef (stx : Syntax) : Bool :=
  if !stx.isOfKind ``Lean.Parser.Command.declaration then
    false
  else
    let decl := stx[1]
    let k := decl.getKind
    k == ``Lean.Parser.Command.abbrev ||
    k == ``Lean.Parser.Command.definition ||
    k == ``Lean.Parser.Command.theorem ||
    k == ``Lean.Parser.Command.opaque ||
    k == ``Lean.Parser.Command.axiom ||
    k == ``Lean.Parser.Command.inductive ||
    k == ``Lean.Parser.Command.classInductive ||
    k == ``Lean.Parser.Command.structure

/-- Return `true` if `stx` is an `instance` declaration command -/
private def isInstanceDef (stx : Syntax) : Bool :=
  stx.isOfKind ``Lean.Parser.Command.declaration &&
  stx[1].getKind == ``Lean.Parser.Command.instance

/-- Return `some name` if `stx` is a definition named `name` -/
private def getDefName? (stx : Syntax) : Option Name := do
  if isNamedDef stx then
    let (id, _) := expandDeclIdCore stx[1][1]
    some id
  else if isInstanceDef stx then
    let optDeclId := stx[1][3]
    if optDeclId.isNone then none
    else
      let (id, _) := expandDeclIdCore optDeclId[0]
      some id
  else
    none

private def hasDeclNamespace (stx : Syntax) : MacroM (Bool) := do
  let some name := getDefName? stx | return false
  let scpView := extractMacroScopes name
  match scpView.name with
  | .str .anonymous _ => return false
  | .str .. => return true
  | _ => return false
-- end of Lean.Elab.Declaration

def getScopeInfo : CommandElabM ScopeInfo := do
  let scope ← getScope
  return {
    varDecls := ← scope.varDecls.mapM fun stx => do liftCoreM <| toString <$> ppCommand (← `(variable $stx))
    includeVars := scope.includedVars.toArray.map fun name => name.eraseMacroScopes,
    omitVars := scope.omittedVars.toArray.map fun name => name.eraseMacroScopes,
    levelNames := scope.levelNames.toArray,
    currNamespace := ← getCurrNamespace,
    openDecl := ← getOpenDecls,
    scopedOpenDecl := ← liftIO <| getActiveNamespaces (← getEnv),
  }

-- see Elab.elabInductive, which is of course also private
def getConstructorInfo (parentName : Name) (stx : Syntax) : CommandElabM BaseDeclarationInfo := do
  -- def ctor := leading_parser optional docComment >> "\n| " >> declModifiers >> rawIdent >> optDeclSig
  let scopeInfo ← getScopeInfo
  let mut modifiers ← elabModifiers stx[2]
  if let some leadingDocComment := stx[0].getOptional? then
    modifiers := { modifiers with docString? := some ⟨leadingDocComment, false⟩ }
  let id := stx[3]
  let name := id.getId
  let name ← getFullname modifiers <| parentName ++ name
  let signature := stx[4]
  let (binders, type) := expandOptDeclSig signature
  let params ← liftTermElabM <| binders.getArgs.flatMapM toBinderViews

  let (ref, signature) ← liftCoreM do pure (← PPSyntax.pp `command stx, ← PPSyntax.pp `term signature)
  return {
    kind := "constructor",
    ref,
    id,
    name,
    modifiers,
    signature,
    params,
    type,
    value := none,
    scopeInfo,
  }

-- see Elab.elabDeclaration
def getDeclarationInfo (stx : Syntax) : CommandElabM DeclarationInfo := do
  let scopeInfo ← getScopeInfo

  let modifiers ← elabModifiers stx[0]
  let decl := stx[1]
  let kind := decl.getKind

  let .str _ kindStr := kind | unreachable!

  let signature := match kind with
    | ``Command.abbrev
    | ``Command.definition
    | ``Command.theorem
    | ``Command.opaque
    | ``Command.axiom
    | ``Command.inductive
    | ``Command.classInductive =>
      decl[2]
    | ``Command.instance =>
      decl[4]
    | ``Command.example =>
      decl[1]
    | ``Command.structure =>
      Syntax.node2 .none ``Command.optDeclSig decl[2] decl[4]
    | _ => unreachable!

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
  let name ← getFullname modifiers name
  let params ← liftTermElabM <| binders.getArgs.flatMapM toBinderViews

  let (ref, signature) ← liftCoreM do pure (← PPSyntax.pp `command stx, ← PPSyntax.pp `term signature)
  let info := {
    kind := kindStr,
    ref,
    id,
    name,
    modifiers,
    signature,
    params,
    type,
    value,
    scopeInfo,
   : BaseDeclarationInfo}

  if kind == ``Command.«inductive» ∨ kind == ``Command.classInductive then
    let constructors ← decl[4].getArgs.mapM <| getConstructorInfo name
    return .ofInductive { info with constructors }

  return .ofBase info

initialize declRef : IO.Ref (Array DeclarationInfo) ← IO.mkRef #[]

def handleProofWanted (stx : Syntax) : CommandElabM Unit := do
  let mods := stx[0]
  let name := stx[2]
  let sig := stx[3]
  let stx' ← `($mods:declModifiers axiom $name $sig)
  elabCommand stx'
  declRef.modify fun a => a.modify (a.size - 1) fun info =>
    .ofBase { info.toBaseDeclarationInfo with kind := "proofWanted" }

def handleDeclaration (stx : Syntax) : CommandElabM Unit :=
  withEnableInfoTree false do
    let info ← getDeclarationInfo stx
    declRef.modify fun a => a.push info
    throwUnsupportedSyntax

def onLoad : CommandElabM Unit := do
  modifyEnv fun env => env |>
    (commandElabAttribute.ext.addEntry · {
      key := ``Parser.Command.declaration,
      declName := ``handleDeclaration,
      value := handleDeclaration,
    }) |>
    (commandElabAttribute.ext.addEntry · {
      key := `proof_wanted,
      declName := ``handleProofWanted,
      value := handleProofWanted,
    })

def getResult : CommandElabM (Array DeclarationInfo) := declRef.get

end Analyzer.Process.Declaration
