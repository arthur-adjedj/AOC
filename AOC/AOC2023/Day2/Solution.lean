import AOC.Utils

namespace Day2

structure Count where
  red : Nat
  green : Nat
  blue : Nat
deriving Inhabited, Repr

def Count.empty : Count := ⟨0,0,0⟩

def Count.isValid (c : Count) : Bool :=
  c.red <= 12 && c.green <= 13 && c.blue <= 14

def Count.power (c : Count) : Nat := c.1*c.2*c.3

def changeCount (c : Count) (s : String) : Count := Id.run do
  let s : String := s.trim
  let [n,s] := s.splitOn " " | unreachable!
  let n := n.toNat!
  if s.endsWith "red" then
    return {c with red := max c.red n}
  else if s.endsWith "green" then
    return  {c with green := max c.green n}
  else if s.endsWith "blue" then
    return  {c with blue := max c.blue n}
  else unreachable!

def getGameId (s : String) : Nat := Id.run do
  let s := s.trim
  let [_,id] := s.splitOn " " | unreachable!
  id.toNat!

def isValidGame (s : String) : (Nat × Bool) := Id.run do
  let head::[picks] := s.splitOn ":" | unreachable!
  let gameId := getGameId head
  let picks := picks.splitOn "," |>.map (·.splitOn ";") |>.flatten
  let finalCount := picks.foldl changeCount Count.empty
  return (gameId,finalCount.isValid)

def part₁ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let mut res := 0
  for line in lines do
    let (id,true) := isValidGame line | continue
    res := res + id
  return res

--#eval part₁

def powerOfGame (s : String) : Nat := Id.run do
  let _::[picks] := s.splitOn ":" | unreachable!
  let picks := picks.splitOn "," |>.map (·.splitOn ";") |>.flatten
  let finalCount := picks.foldl changeCount Count.empty
  return finalCount.power

def part₂ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let mut res := 0
  for line in lines do
    res := res + powerOfGame line
  return res

--#eval part₂
