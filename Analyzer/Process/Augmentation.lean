import Lean
import Metalib.Declaration

open Lean Elab Meta Command Syntax Parser Tactic
open Lean PrettyPrinter Delaborator SubExpr

namespace List

/-- Optimized version of `product`. -/
def productTR {α β : Type} (l₁ : List α) (l₂ : List β) : List (α × β) :=
  l₁.foldl (fun acc a => l₂.foldl (fun acc b => acc.push (a, b)) acc) #[] |>.toList

end List

namespace Lean.Syntax

def setArgr (stx: Syntax) (locs: List Nat) (arg: Syntax) : Syntax :=
  match locs with
  | .nil => arg
  | .cons i it => stx.setArg i $ stx[i].setArgr it arg


def setArgsr (stx: Syntax) (locs: List Nat) (args: Array Syntax) : Syntax :=
  match locs with
  | .nil => stx.setArgs args
  | .cons i it => stx.setArg i $ stx[i].setArgsr it args

-- def dedupSyntax : List Syntax → List Syntax :=
--   List.pwFilter (toString · ≠ toString ·)

def getProof (stx : Syntax) : Syntax :=
  match stx[1].getKind with
  | `Batteries.Tactic.Lemma.lemmaCmd => stx[1][1][3]
  | ``Lean.Parser.Command.theorem     => stx[1][3]
  | ``Lean.Parser.Command.definition  => stx[1][3]
  | _ => Syntax.missing

def setProof (stx : Syntax) (newProof : Syntax) : Syntax :=
  match stx[1].getKind with
  | `Batteries.Tactic.Lemma.lemmaCmd => stx.setArgr [1,1,3] newProof
  | ``Lean.Parser.Command.theorem     => stx.setArgr [1,3] newProof
  | ``Lean.Parser.Command.definition  => stx.setArgr [1,3] newProof
  | _ => stx

def getTacticSeq (stx : Syntax) : Array Syntax :=
  stx.getProof[1][1][0][0].getArgs

/-- Get the tactic sequence of a proof, excluding the tactic separator.
This is useful for manipulating the tactic sequence. -/
def getTacticSeq' (stx : Syntax) : Array Syntax :=
  let tacSep := stx.getProof[1][1][0][0][1]!
  stx.getProof[1][1][0][0].getArgs.filter (¬ · == tacSep)

def setTacticSeq (stx : Syntax) (tacSeq : Array Syntax) : Syntax :=
  stx.getProof.setArgsr [1,1,0,0] tacSeq

def getProofAfter (stx : Syntax) (pos: String.Pos) : Syntax := Id.run do
  -- let tactic_seq := stx[1][1][3][1][1][0][0].getArgs
  let tactic_seq := stx.getTacticSeq
  let tac_sep := tactic_seq[1]!
  let mut after_seq : Array Syntax := #[]
  for tac in tactic_seq do
    let tac_pos := tac.getPos?.getD 0
    if tac_pos >= pos then
      after_seq := (after_seq.push tac).push tac_sep
  stx.setTacticSeq after_seq

def getProofWithin (stx : Syntax) (pos : String.Pos) (endPos : String.Pos) : Syntax := Id.run do
  let tactic_seq := stx.getTacticSeq
  let tac_sep := tactic_seq[1]!
  let mut within_seq : Array Syntax := #[]
  for tac in tactic_seq do
    let tac_pos := tac.getPos?.getD 0
    if tac_pos >= pos ∧ tac_pos <= endPos then
      within_seq := (within_seq.push tac).push tac_sep
  stx.setTacticSeq within_seq

/-- Push a tactic at the end of a proof -/
def pushTactic (stx : Syntax) (tac : Syntax) : Syntax := Id.run do
  let tactic_seq := stx[1][1][0][0].getArgs
  let tac_sep := tactic_seq[1]!
  let new_seq := (tactic_seq.push tac).push tac_sep
  stx.setArgsr [1,1,0,0] new_seq

end Lean.Syntax

def goal2ToDecl (goal1 goal2 : MVarId) (name : Name) : MetaM (TSyntax `command) :=
  withEnableInfoTree false <| goal1.withContext do
    let type2 ← instantiateMVars (← goal2.getType)
    let newFVarId ← mkFreshFVarId
    goal1.modifyLCtx fun lctx =>
      lctx.mkLocalDecl newFVarId `hg2 type2
    goalToDecl goal1 name

@[delab app]
def delabCoeWithType : Delab := whenPPOption getPPCoercions do
  let typeStx ← withType delab
  let e ← getExpr
  let .const declName _ := e.getAppFn | failure
  let some info ← Meta.getCoeFnInfo? declName | failure
  let n := e.getAppNumArgs
  withOverApp info.numArgs do
    match info.type with
    | .coe => `(($(← withNaryArg info.coercee delab) : $typeStx))
    | .coeFun =>
      if n = info.numArgs then
        `((⇑$(← withNaryArg info.coercee delab) : $typeStx))
      else
        withNaryArg info.coercee delab
    | .coeSort => `((↥$(← withNaryArg info.coercee delab) : $typeStx))

namespace Analyzer.Process.Augmentation

initialize augProofRef : IO.Ref (Array String) ← IO.mkRef #[]

def getAugmented (stx : Syntax) : CommandElabM Unit := do
  -- elabCommand stx
  elabDeclaration stx
  let tacSeq := stx.getTacticSeq'.toList
  let trees ← getInfoTrees
--   let mut newProofs : Array Syntax := #[]
  for tree in trees do

    let bi_enumerator := (List.productTR tacSeq.enum tacSeq.enum).filter
      fun ((i, _), (j, _)) => i < j
    for ((_, tac1), (_, tac2)) in bi_enumerator do
      let posStart := tac1.getPos?.getD 0
      let posEnd   := tac2.getTailPos?.getD 0
      for g1 in tree.goalsAt? (← getFileMap) posStart do
        for g2 in tree.goalsAt? (← getFileMap) posEnd do
          let goalStart := g1.tacticInfo.goalsBefore.head!
          match g2.tacticInfo.goalsAfter.head? with
          | none              => -- If there are no goals after, we only need to augment the proof
            let name          := Name.mkSimple s!"aug_{posStart}_end"
            let statement     := (g1.ctxInfo.runMetaM {} (goalToDecl goalStart name))
            let proofAfter    := stx.getProofAfter posStart
            let newProof      := (← statement).raw.setProof proofAfter
            let strProof ← liftCoreM $ ppTactic ⟨newProof⟩
            augProofRef.modify fun a => a.push $ toString strProof-- newProofs := newProofs.push newProof
          | some goalAfter    => -- If there are goals after, we need to augment the proof and add a new goal
            let name          := Name.mkSimple s!"aug_{posStart}_{posEnd}"
            try
              let statement   := (g1.ctxInfo.runMetaM {} (goal2ToDecl goalStart goalAfter name))
              let proofWithin := stx.getProofWithin posStart posEnd
              let addedHyp    := mkIdent `hg2
              let addedExact  ← `(tactic | exact $addedHyp)
              let proofWithin := proofWithin.pushTactic addedExact.raw
              let newProof    := (← statement).raw.setProof proofWithin
              let strProof ← liftCoreM $ ppTactic ⟨newProof⟩
            --   newProofs       := newProofs.push newProof
              augProofRef.modify fun a => a.push $ toString strProof
            catch _ => continue
  throwUnsupportedSyntax

def onLoad : CommandElabM Unit := do
  enableInfoTree
  modifyEnv fun env => env |>
  (Delaborator.delabAttribute.ext.addEntry ·
  {
    key := ``Parser.Term.app,
    declName := ``delabCoeWithType,
    value := delabCoeWithType,
  }) |>
  (commandElabAttribute.ext.addEntry ·
  {
      key := ``Parser.Command.declaration,
      declName := ``getAugmented,
      value := getAugmented,
  })

def getResult : CommandElabM (Array String) := augProofRef.get

end Analyzer.Process.Augmentation
