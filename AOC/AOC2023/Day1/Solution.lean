import AOC.Utils

namespace Day1

def getNumber (s : String) : Nat :=
  let first := s.find (·.isDigit)
  let last := s.revFind (·.isDigit) |>.get!
  10 * (s.get! first).toString.toNat! + (s.get! last).toString.toNat!

def part₁ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let mut res := 0
  for line in lines do
    res := res + getNumber line
  return res

--#eval part₁

def StringNumbers : List String :=
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

def toNumber (s : Substring) : Option Nat :=
  go 1 StringNumbers s
where
  go (n : Nat)
    | [],_ => none
    | sub::tl,s => if s.startsWith sub then n else go (n+1) tl s

partial def findFirst (s : Substring) : Option Nat := do
  if s.isEmpty then none else
  if s.get 0 |>.isDigit then
    return s.get 0 |>.toString.toNat!
  if let some n := toNumber s then
    return n
  else findFirst <| s.drop 1

partial def findLast (s : Substring) : Option Nat :=  do
  if s.isEmpty then none else
  if let some res := findLast <| s.drop 1 then
    return res
  else
    findFirst s

def getNumber₂ (s : String) : Nat :=
  let first := findFirst s.toSubstring |>.get!
  let last := findLast s.toSubstring |>.get!
  10 * first + last

def part₂ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let mut res := 0
  for line in lines do
    res := res + getNumber₂ line
  return res

--#eval part₂
