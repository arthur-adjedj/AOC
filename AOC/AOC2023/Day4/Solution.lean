import AOC.Utils

structure Scratchpad where
  numOfScratchpards : Nat
  winningNumbers : List Nat
  numbers : List Nat
deriving Inhabited, Repr

def Scratchpad.numOfWins (nums : Scratchpad) : Nat :=
  nums.winningNumbers.foldl (fun acc win =>
    acc + nums.numbers.foldl (fun acc x =>
      if x = win then acc + 1 else acc)
      0
  ) 0

def Scratchpad.points (nums : Scratchpad) : Nat :=
  if nums.numOfWins = 0 then 0 else 2^(nums.numOfWins-1)

def String.toScratchpad (s : String) : Scratchpad := Id.run do
  let [_head,picks]   := s.splitOn ":" | unreachable!
  let [winNums,nums] := picks.splitOn "|" | unreachable!
  let winNums := winNums.trim.splitOn " " |>.map (·.toNat!)
  let nums := nums.trim.splitOn " " |>.filterMap (·.toNat?)
  ⟨1,winNums,nums⟩

def part1 : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let mut res := 0
  for line in lines do
    res := res + line.toScratchpad.points
  return res

--#eval part1 --23028

abbrev Scratches := Array Scratchpad

def test : Scratches :=
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
|>.splitOn "\n"
|>.toArray
|>.map String.toScratchpad

def Scratches.readScratch (s : Scratches) (line : Nat) : Scratches := Id.run do
  let numOfCurrentScratches := s[line]!.numOfWins
  let toAdd := s[line]!.numOfScratchpards
  let mut res := s
  for next in [line+1 : Min.min (line + numOfCurrentScratches + 1) s.size] do
    res := res.modify next (fun s => {s with numOfScratchpards := s.numOfScratchpards + toAdd})
  return res

def Scratches.endScratches (s : Scratches) : Scratches := Id.run do
  let mut res := s
  for line in [0:res.size] do
    res := res.readScratch line
  return res

def Scratches.totalScratches (s : Scratches) : Nat :=
  s.foldl (fun acc scratch => acc+scratch.numOfScratchpards) 0


def part2 : IO Nat := do
  let lines ← IO.FS.lines #"input"#
  let s : Scratches := lines.map String.toScratchpad
  return s.endScratches.totalScratches

--#eval part2 --9236992
