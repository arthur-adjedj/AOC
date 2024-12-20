import Lean

open Lean Elab Parser Term


/-- When called inside a file in a path `PATH` elaborates a path `$PATH/s`, this is useful
    since every solution lean file is stored with its corresponding input, so one may only write
    `IO.FS.lines #"input"#` to get the input of it's corresponding folder
-/
elab "#" s:term "#": term => do
  let dir ← getFileName
  let dir := dir.dropRightWhile (fun c => c != '/' && c != '\\')
  let s ← elabTerm s none
  return mkApp (mkConst ``System.FilePath.mk) (mkApp (mkApp (mkConst ``String.append) (mkStrLit dir)) s)

@[inline] def Substring.get! : Substring → String.Pos → Char
  | ⟨s, b, _⟩, p => s.get! (b+p)

@[inline] def Substring.get? : Substring → String.Pos → Option Char
  | ⟨s, b, _⟩, p => s.get? (b+p)

def Substring.startsWith (s : Substring) (pre : String) :=
  s.take pre.length == pre.toSubstring

instance [h₁ : Decidable P] [h₂ : Decidable Q] : Decidable (∃ _ : P, Q) :=
  match h₁,h₂ with
    | .isTrue h₁ , .isTrue h₂  => .isTrue ⟨h₁,h₂⟩
    | _          , .isFalse h₂ => .isFalse fun c => h₂ c.2
    | .isFalse h₁, _           => .isFalse fun c => h₁ c.1

instance {i : Nat × Nat}: Decidable (∃ h : i.fst < Array.size xs, i.2 < Array.size (xs[i.1]'h)) :=
  if h₁ : i.fst < Array.size xs then
    if h₂ : i.2 < Array.size (xs[i.1]'h₁) then
      .isTrue ⟨h₁,h₂⟩
    else
      .isFalse fun c => h₂ c.2
  else
    isFalse fun c => h₁ c.1

def String.toArray (s: String) : Array Char := s.data.toArray

def Array.toNat (arr : Array Char) : Nat :=
  arr.foldl (fun n c => 10*n+ c.toString.toNat!) 0

@[inline] def getElem!! [GetElem cont idx elem dom] [Inhabited elem] (xs : cont) (i : idx) [Decidable (dom xs i)] : elem :=
  if h : _ then getElem xs i h else default

macro:max x:term noWs "[" i:term "]" noWs "!!" : term => `(getElem!! $x $i)
