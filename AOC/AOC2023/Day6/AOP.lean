inductive Expr : Type where
  | Const : Nat → Expr
  | Plus  : Expr → Expr → Expr
  | Times : Expr → Expr → Expr

def eval : Expr → Nat
  | .Const n => n
  | .Plus  e1 e2 => eval e1 + eval e2
  | .Times e1 e2 => eval e1 * eval e2

inductive Instr : Type where
  | Push : Nat → Instr
  | Add  : Instr
  | Mult : Instr

def instr_semantics : Instr → (List Nat → List Nat)
  | .Push n, s => n :: s
  | .Add,  y :: x :: s => (x + y) :: s
  | .Mult, y :: x :: s => (x * y) :: s
  | _, _ => []

def execute : List Instr → List Nat → List Nat
  | [],        s => s
  | (i :: is), s => execute is (instr_semantics i s)

def compile : Expr → List Instr
  | .Const x => [ .Push x ]
  | .Plus  e1 e2 => compile e1 ++ compile e2 ++ [ .Add  ]
  | .Times e1 e2 => compile e1 ++ compile e2 ++ [ .Mult ]

theorem execute_append : execute (a ++ b) s = execute b (execute a s) := by
  induction a generalizing s with
  | nil => rfl
  | cons hd tl ih =>
     calc execute (hd :: tl ++ b) s
     _ = execute (tl ++ b) (instr_semantics hd s) := by rfl
     _ = execute b (execute tl (instr_semantics hd s)) := by rw [ih]

theorem compile_correct_s : execute (compile e) s = (eval e)::s := by
  induction e generalizing s with
  | Const _ => rfl
  | Plus x y ihx ihy
  | Times x y ihx ihy =>
    rw [compile,List.append_assoc,execute_append,execute_append]
    simp [execute,ihx,ihy]
    rfl

theorem compile_correct : execute (compile e) [] = [ eval e ] := compile_correct_s
