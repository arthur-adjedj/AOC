import AOC.Utils

namespace Day3

/-the borders of the matrix can be considered to be `'.'` -/
instance (priority := high) : Inhabited Char := ⟨'.'⟩

def Char.isSymbol (c : Char) : Bool :=
  c != '.' && !c.isDigit

abbrev Schematic := Array (Array Char)

def adjacentSymbol (s : Schematic) (i j: Nat) : Bool :=
  let edges :=
  [s[i-1]!![j]!!,s[i-1]!![j-1]!!,s[i-1]!![j+1]!!,
   s[i+1]!![j]!!,s[i+1]!![j-1]!!,s[i+1]!![j+1]!!,
   s[i]!![j-1]!!,s[i]!![j+1]!!
  ]

  edges.any Char.isSymbol

def adjacentSymbolsLine (s : Schematic) (line start stop : Nat): Nat := Id.run do
  for j in [start:stop+1] do
    if adjacentSymbol s line j then
      return s[line]![start:stop+1].toArray.toNat
  return 0

def nextNumPos (s : Schematic) (l : Nat) (pos : Nat := 0) : Option (Nat × Nat) := do
  let length := s[0]!.size
  if pos >= length then
    return ← none
  for start in [pos:length] do
    if s[l]![start]!.isDigit then
      let stop := Id.run do
        for stop in [start+1:length] do
          if !s[l]![stop]!.isDigit then
            return stop-1
        return length-1
      return (start,stop)
  none

def sumOfLine (s : Schematic) (l : Nat) : Nat := Id.run do
  let mut res := 0
  let mut pos := 0
  while pos < s[0]!.size do
    if let some (start,stop) := nextNumPos s l pos then
      res := res + adjacentSymbolsLine s l start stop
      pos := stop+1
    else break
  return res

def part₁OnArray (s : Schematic) : Nat := Id.run do
  let mut res := 0
  for line in [0:s.size] do
    res := res + sumOfLine s line
  return res

def test :=
  "467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598.."
  |>.splitOn "\n"
  |>.toArray
  |>.map String.toArray

def part₁ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let s := lines |>.map String.toArray
  return part₁OnArray s

--#eval part₁ --539433

partial def getNumber (s : Schematic) (line start? : Nat) : Nat := Id.run do
  if !s[line]!![start?]!!.isDigit then
    return 0
  if start?>0 && s[line]!![start?-1]!!.isDigit then
    return getNumber s line (start?-1)
  let mut res := 0
  for j in [start?:s[0]!.size] do
    if !s[line]!![j]!!.isDigit then
      break
    res := 10*res + s[line]!![j]!!.toString.toNat!
  return res

/-if the top char is a number, no need to check the topLeft/topRight, same for bottom -/
def toAdjacentNumbers (s : Schematic) (i j : Nat) : List Nat := Id.run do
  let mut res := []

  res := (getNumber s i (j+1))::(getNumber s i (j-1))::res

  if s[i-1]!![j]!!.isDigit then
    res := (getNumber s (i-1) j)::res
  else
    res := (getNumber s (i-1) (j-1))::(getNumber s (i-1) (j+1))::res

  if s[i+1]!![j]!!.isDigit then
    res := (getNumber s (i+1) j)::res
  else
    res := (getNumber s (i+1) (j-1))::(getNumber s (i+1) (j+1))::res

  return res.filter (· != 0)

def part₂OnArray (s : Schematic) : Nat := Id.run do
  let mut res := 0
  for line in [0:s.size] do
    for j in [0:s[0]!.size] do
    let num := Id.run do
      if s[line]![j]! != '*' then
        return 0
      let [num1,num2] := toAdjacentNumbers s line j | 0
      num1*num2
    res := res + num
  return res

def part₂ : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let s := lines |>.map String.toArray
  return part₂OnArray s

--#eval part₂ --75847567
