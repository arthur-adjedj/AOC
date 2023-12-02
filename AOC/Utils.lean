import Lean

open Lean Elab Term

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

def List.flatten : List (List α) → List α
  | [] => []
  | []::tl => tl.flatten
  | (h::tl₁)::tl₂ => h :: (tl₁::tl₂).flatten
