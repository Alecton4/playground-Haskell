module HW03 where

data Expression
  = Var String -- Variable
  | Val Int -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement
  = Assign String Expression
  | Incr String
  | If Expression Statement Statement
  | While Expression Statement
  | For Statement Expression Statement Statement
  | Sequence Statement Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

-- NOTE: review
extend :: State -> String -> Int -> State
extend st varName varVal = \varName' ->
  if varName == varName'
    then varVal
    else st varName'

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st expr = case expr of
  Var varName -> st varName
  Val varVal -> varVal
  Op expr1 bop expr2 -> evalOp (evalE st expr1) bop (evalE st expr2)

evalOp x bop y = case bop of
  Plus -> x + y
  Minus -> x - y
  Times -> x * y
  Divide -> x `div` y
  Gt -> if x > y then 1 else 0
  Ge -> if x >= y then 1 else 0
  Lt -> if x < y then 1 else 0
  Le -> if x <= y then 1 else 0
  Eql -> if x == y then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement
  = DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign varName expr) = DAssign varName expr
desugar (Incr varName) = DAssign varName (Op (Var varName) Plus (Val 1))
desugar (If expr stmt1 stmt2) = DIf expr (desugar stmt1) (desugar stmt2)
desugar (While expr stmt) = DWhile expr (desugar stmt)
desugar (For stmtInit expr stmtUpdate stmtBody) = DSequence (desugar stmtInit) (DWhile expr (DSequence (desugar stmtBody) (desugar stmtUpdate)))
desugar (Sequence stmt1 stmt2) = DSequence (desugar stmt1) (desugar stmt2)
desugar (Skip) = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st stmt = case stmt of
  DAssign varName expr ->
    extend st varName (evalE st expr)
  DIf expr stmt1 stmt2 ->
    if evalE st expr /= 0
      then evalSimple st stmt1
      else evalSimple st stmt2
  DWhile expr stmt ->
    if evalE st expr /= 0
      then evalSimple st (DSequence stmt (DWhile expr stmt))
      else st
  DSequence stmt1 stmt2 ->
    evalSimple (evalSimple st stmt1) stmt2
  DSkip ->
    st

run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l = foldr1 Sequence l

{- Calculate the factorial of the input

  for (Out := 1; In > 0; In := In - 1) {
    Out := In * Out
  }
-}
factorial :: Statement
factorial =
  For
    (Assign "Out" (Val 1))
    (Op (Var "In") Gt (Val 0))
    (Assign "In" (Op (Var "In") Minus (Val 1)))
    (Assign "Out" (Op (Var "In") Times (Var "Out")))

{- Calculate the floor of the square root of the input

  B := 0;
  while (A >= B * B) {
    B++
  };
  B := B - 1
-}
squareRoot :: Statement
squareRoot =
  slist
    [ Assign "B" (Val 0),
      While
        (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
        (Incr "B"),
      Assign "B" (Op (Var "B") Minus (Val 1))
    ]

{- Calculate the nth Fibonacci number

  F0 := 1;
  F1 := 1;
  if (In == 0) {
    Out := F0
  } else {
    if (In == 1) {
      Out := F1
    } else {
      for (C := 2; C <= In; C++) {
        T  := F0 + F1;
        F0 := F1;
        F1 := T;
        Out := T
      }
    }
  }
-}
fibonacci :: Statement
fibonacci =
  slist
    [ Assign "F0" (Val 1),
      Assign "F1" (Val 1),
      If
        (Op (Var "In") Eql (Val 0))
        (Assign "Out" (Var "F0"))
        ( If
            (Op (Var "In") Eql (Val 1))
            (Assign "Out" (Var "F1"))
            ( For
                (Assign "C" (Val 2))
                (Op (Var "C") Le (Var "In"))
                (Incr "C")
                ( slist
                    [ Assign "T" (Op (Var "F0") Plus (Var "F1")),
                      Assign "F0" (Var "F1"),
                      Assign "F1" (Var "T"),
                      Assign "Out" (Var "T")
                    ]
                )
            )
        )
    ]
