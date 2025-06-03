/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Analyzer.Types

open Lean Elab Meta Term
open Std (HashSet HashMap)

namespace Analyzer

instance : ToJson Unit where
  toJson _ := Json.null
instance : ToJson Substring where
  toJson x := x.toString
instance : ToJson String.Pos where
  toJson x := x.1

instance {α : Type _} [BEq α] [Hashable α] [ToJson α] : ToJson (HashSet α) where
  toJson x := .arr <| x.toArray.map toJson
instance {α β : Type _} [BEq α] [Hashable α] [ToString α] [ToJson β] : ToJson (HashMap α β) where
  toJson x := .mkObj <| x.toList.map fun (a, b) => (toString a, toJson b)
instance : ToJson String.Range where
  toJson x := json% [$(x.start), $(x.stop)]
deriving instance ToJson for Visibility, RecKind, AttributeKind, BinderInfo

def _root_.Lean.Name.toArray : Name → Array Json
  | .anonymous => #[]
  | .str xs x => xs.toArray.push x
  | .num xs x => xs.toArray.push x

instance : ToJson Name where
  toJson x := toJson x.toArray

section
local instance : ToJson Syntax where
  toJson x := toJson x.getRange?
local instance : ToJson (Option Syntax) where
  toJson x := toJson <| x.bind Syntax.getRange?
local instance : ToJson (TSyntax `Lean.Parser.Command.declModifiers) where
  toJson x := toJson x.raw
local instance : ToJson (TSyntax `Lean.Parser.Command.docComment) where
  toJson x := Json.str x.getDocString
deriving instance ToJson for Attribute, Modifiers, BinderView
end

section
local instance : ToJson Syntax where
  toJson x := json% {
    range: $(x.getRange?),
    original: $(x.isOriginal)
  }
deriving instance ToJson for PPSyntax, PPSyntaxWithKind

local instance : ToJson OpenDecl where
  toJson
  | .simple ns except => json%{
    simple: {
      «namespace»: $ns,
      except: $except
    }
  }
  | .explicit id declName => json%{
    rename: {
      name: $declName,
      as: $id
    }
  }
deriving instance ToJson for ScopeInfo, BaseDeclarationInfo, InductiveInfo
instance : ToJson DeclarationInfo where
  toJson
  | .ofBase x => toJson x
  | .ofInductive x => toJson x
end

deriving instance ToJson for SymbolKind, SymbolInfo
deriving instance ToJson for Variable, Goal

deriving instance ToJson for ModuleInfo

deriving instance ToJson for LineInfo

section
deriving instance ToJson for SpecialValue

local instance : ToJson Syntax where
  toJson x := json% {
    kind: $(x.getKind),
    range: $(x.getRange?),
    original: $(x.isOriginal),
    str: $(x.prettyPrint.pretty 0)
  }
deriving instance ToJson for TacticElabInfo, TermElabInfo, MacroInfo, ElaborationInfo

private partial def go : ElaborationTree → Json
  | .mk info ref children => json% {
    info: $(info),
    ref: $(ref),
    children: $(children.map go)
  }

instance : ToJson ElaborationTree where
  toJson := go
end

deriving instance ToJson for SourceInfo, Syntax.Preresolved, Syntax

end Analyzer
