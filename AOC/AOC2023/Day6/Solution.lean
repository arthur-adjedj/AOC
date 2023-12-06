import AOC.Utils

namespace Day6


def f (time hold : Nat) := (time-hold)*hold

/--
  This is a naive algorithm. A better way to do that would be to find the roots of
  `(time-hold)*hold-record`, and use their rounding values as the `start` and `stop`s
  I don't want to bother doing that.
-/
def betterSolutions (time record : Nat) : Nat := Id.run do
  let start := Id.run do
    for hold in [0:time+1] do
      if f time hold > record then
        return hold
    return 0
  let stop := Id.run do
    for hold in [0:time+1] do
      if f time (time-hold) > record then
        return (time-hold)
    return 0
  return stop-start+1

def parseLine (s : String) : Array Nat := Id.run do
  let [_head,nums] := s.splitOn ":" | unreachable!
  let nums := nums.trim.splitOn " " |>.filterMap (·.toNat?)
  return nums.toArray

def result (times records : Array Nat) := Id.run do
  let mut res := 1
  for race in [0:times.size] do
    res := res* betterSolutions times[race]! records[race]!
  return res

def part₁ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let times   := parseLine lines[0]!
  let records := parseLine lines[1]!
  return result times records

#eval part₁

def parseLine₂ (s : String) : Nat := Id.run do
  let [_head,nums] := s.splitOn ":" | unreachable!
  let nums := nums.trim.splitOn " " |>.filter (·.toNat?.isSome)
  let num := nums.foldl String.append "" |>.toNat!
  return num

def part₂ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let time   := parseLine₂ lines[0]!
  let record := parseLine₂ lines[1]!
  return betterSolutions time record

#eval part₂
