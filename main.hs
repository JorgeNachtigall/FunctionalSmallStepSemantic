import Estado
--The state module is used to simulate an memory

data AExp = Num Int
     |Var String
     |Sum AExp AExp
     |Sub AExp AExp
     |Mul AExp AExp
  deriving(Show)

data BExp = TRUE
     | FALSE
     | Not BExp
     | And BExp BExp
     | Or  BExp BExp
     | Eq  AExp AExp
     | Leq AExp AExp
   deriving(Show)

data CExp =    While BExp CExp
     | If BExp CExp CExp
     | Seq CExp CExp
     | Attrib AExp AExp
     | RepeatUntil BExp CExp
     | DoubleAttribution AExp AExp AExp AExp
     | For AExp AExp AExp CExp
     | Skip
   deriving(Show)                


-- Arithmetic expressions
aSmallStep :: (AExp,Estado) -> (AExp,Estado)

--Var implementation
aSmallStep (Var x,s) = (Num (procuraVar s x),s)

--Sum implementation
aSmallStep (Sum (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Sum (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Sum (Num x) ef,s)
aSmallStep (Sum e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Sum ef e2,s)

--Subtraction implementation
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y),s)
aSmallStep (Sub (Num x) e2, s) = let(ef,_) = aSmallStep(e2,s)
                                 in (Sub (Num x) ef,s)
aSmallStep (Sub e1 e2,s)  = let (ef,_) = aSmallStep (e1,s)
                            in(Sub ef e2,s)

--Multiplication implementation
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y),s)
aSmallStep (Mul (Num x) e2, s) = let(ef,_) = aSmallStep(e2,s)
                                 in (Mul (Num x) ef,s)
aSmallStep (Mul e1 e2,s)  = let (ef,_) = aSmallStep (e1,s)
                            in(Mul ef e2,s)

-- Arithmetic interpreter
interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False


-- Boolean expressions
bSmallStep :: (BExp,Estado) -> (BExp,Estado)

-- Not implementation
bSmallStep (Not FALSE,s)      = (TRUE,s)
bSmallStep (Not TRUE,s)       = (FALSE, s)
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s)
                        in (Not bn ,sn)

-- And implementation
bSmallStep (And TRUE b2,s)  = (b2,s)
bSmallStep (And FALSE b2,s) = (FALSE,s)
bSmallStep (And b1 b2,s)    = let (bn,sn) = bSmallStep (b1,s)
                              in (And bn b2,sn)

-- Equals implementation
bSmallStep (Eq (Num x) (Num y), s) = if x == y then (TRUE,s) else (FALSE,s)
bSmallStep (Eq (Num x) e2, s) = let(en,_) = aSmallStep(e2, s)
                                            in(Eq (Num x) en, s)
bSmallStep (Eq e1 e2,s )  = let (en,_) = aSmallStep (e1,s)
                            in (Eq en e2, s)

-- Or implementation
bSmallStep(Or FALSE b2, s) = (b2, s)
bSmallStep(Or TRUE b2, s) = (TRUE,s)
bSmallStep (Or b1 b2,s ) = let(bn,_) = bSmallStep(b1,s)
                           in (Or bn b2, s)

-- Less than equal implementation
bSmallStep (Leq (Num x) (Num y), s) = if x <= y then (TRUE,s) else (FALSE,s)
bSmallStep (Leq (Num x) e2, s) = let(en,_) = aSmallStep(e2, s)
                                            in(Leq (Num x) en, s)
bSmallStep (Leq e1 e2,s )  = let (en,_) = aSmallStep (e1,s)
                            in (Leq en e2, s)

-- Boolean interpreter
interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False



-- Command expressions
cSmallStep :: (CExp,Estado) -> (CExp,Estado)

-- If implementation
cSmallStep (If TRUE c1 c2, s) = (c1, s)
cSmallStep (If FALSE c1 c2, s) = (c2, s)
cSmallStep (If b c1 c2,s) = let(bn,_) = bSmallStep(b, s)
                            in (If bn c1 c2, s)

-- Command sequence implementation
cSmallStep (Seq Skip c2, s) = (c2,s)
cSmallStep (Seq c1 c2,s) = let(cn,ns) = cSmallStep(c1,s)
                           in (Seq cn c2, ns)

-- Attribution implementation
cSmallStep (Attrib (Var x) (Num y), s) = (Skip, mudaVar s x y)
cSmallStep (Attrib (Var x) e, s) = let(en,_) = aSmallStep(e, s)
                                  in(Attrib (Var x) en, s)

-- While implementation
cSmallStep (While b c1, s) = (If b (Seq c1 (While b c1)) Skip, s)

-- Repeat Until implementation
cSmallStep(RepeatUntil b c1, s) = (Seq c1 (If b Skip (RepeatUntil b c1)), s)

-- Double Attribution implementation
cSmallStep(DoubleAttribution (Var x) (Var y) e1 e2, s)= (Seq (Seq (Attrib (Var x) e1) (Attrib (Var y) e2)) Skip, s) 

-- For implementation
cSmallStep(For (Var x) e1 e2 c, s) = (Seq (Attrib (Var x) e1) (If (Leq e1 e2) (Seq c (For (Var x) (Sum (Num 1) e1) e2 c)) Skip),s)

-- Command interpreter
interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c, s) else interpretC (cSmallStep(c,s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC x = False

myState :: Estado
myState = [("x",0), ("y",3), ("z",0)]


-- Examples

example :: CExp

--example = Mul (Num 3) (Mul (Var "x") (Var "y"))
--example = If FALSE (Attrib (Var "x") (Num 5)) (Attrib (Var "y") (Num 0))
--example = While (Eq (Var "x") (Var "y")) (Attrib (Var "y") (Num 0))
--example = DoubleAttribution (Var "x") (Var "y") (Num 7) (Num 8)
--example = RepeatUntil (Eq (Var "x") (Var "y")) (Attrib (Var "x") (Num 3))
example = For (Var "x") (Num 1) (Num 3) (Attrib (Var "z") (Sum (Var "z") (Num 1)))

-- RODANDO O example:
-- Hugs> interpretA (example, myState)
-- Passo a passo:
-- Hugs> aSmallStep (ou bSmallstep) (example, myState)

--example2 :: BExp
--example2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretB (example2,myState)
-- (TRUE,[("x",3),("y",0),("z",0)])


