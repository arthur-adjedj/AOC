import Std.Internal.Parsec.String
import AOC.Utils

open Std Internal Parsec String

namespace Day3

@[inline]
def tryCatch2 (p : Parsec ι α) (csuccess : α → Parsec ι β) (cerror : Unit → Parsec ι β)
    : Parsec ι β := fun it =>
  match p it with
  | .success rem a => csuccess a rem
  | .error rem _ => cerror () rem

def correctNum (finish : Char): Parser (Option Nat) := do
  let k (n : Nat) : Parser (Option Nat):= do
    tryCatch2 (skipChar finish)
      (fun () => return some $ n)
      (fun () => do skip;return none)
  let some n₁ ← tryCatch2 digit (return some ·) (fun _ => return none) | return none
  let some n₂ ← tryCatch2 digit (return some ·) (fun _ => return none) | k $ n₁.toString.toNat!
  let some n₃ ← tryCatch2 digit (return some ·) (fun _ => return none) | k $ n₁.toString.toNat!*10+n₂.toString.toNat!
  k $ n₁.toString.toNat!*100+n₂.toString.toNat!*10+n₃.toString.toNat!

def correctMul : Parser Nat := do
  skipString "mul("
  let some n₁ ← correctNum ',' | fail "incorrect n₁"
  let some n₂ ← correctNum ')' | fail "incorrect n₂"
  return n₁*n₂


def checkAll : Parser Nat := do
  let mut res := 0
  while true do
    if ← isEof then break
    let n ← tryCatch2 correctMul (return ·) (fun _ => do skip; return 0)
    res := res+n
  return res

def part₁ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let mut res := 0
  for line in lines do
    res := res + (checkAll.run line).toOption.get!
  return res

#eval part₁
