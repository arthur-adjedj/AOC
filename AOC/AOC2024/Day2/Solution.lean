import AOC.Utils

namespace Day1

def checkReport (incr?: Bool): List Nat → Bool
  | [] | [_] => true
  | x₁::x₂::tl =>
    let diff := max (x₁-x₂) (x₂-x₁)
    if diff == 0 || diff > 3 then false
    else if (incr? && x₁>x₂) || (!incr? && x₁<x₂) then
      false
    else
      checkReport incr? (x₂::tl)

def part₁ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let mut res := 0
  for line in lines do
    let l@(x₁::x₂::_) := line.splitOn " " |>.map String.toNat! | unreachable!
    if checkReport (x₁<x₂) l then res := res+1
  return res

#eval part₁
