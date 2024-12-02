import AOC.Utils

namespace Day1

def getNumbers (s : String) : Nat × Nat := Id.run do
  let [n1,n2] := s.splitOn "   " | unreachable!
  return (n1.toNat!,n2.toNat!)

def part₁ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let mut left := Array.mkEmpty (α := Nat) lines.size
  let mut right := Array.mkEmpty (α := Nat) lines.size
  for line in lines do
    let (n₁,n₂) := getNumbers line
    left := left.push n₁
    right := right.push n₂
  left := left.insertionSort
  right := right.insertionSort
  let mut res := 0
  for i in [:lines.size] do
    let l := left[i]!
    let r := right[i]!
    res := res + ((max l r) - (min l r))
  return res

-- #eval part₁
