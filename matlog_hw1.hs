-- введём сигнатуру формулы: Var - переменная, Not - отрицание, And - конъюнкция, Or - дизьюнкция
-- Imp и Eq - импликация и двойная импликация соответственно
data Formula = Var String 
        | Not Formula
        | And Formula Formula 
        | Or Formula Formula
        | Imp Formula Formula
	| Eq Formula Formula
	deriving (Eq, Show)


-- функция, которая делает формулу NNF
makeNNF :: Formula -> Formula
makeNNF (Var a) = Var a
makeNNF (Not (Not a)) = (makeNNF a)
makeNNF (And a b) = And (makeNNF a) (makeNNF b)
makeNNF (Or a b) = Or (makeNNF a) (makeNNF b)
makeNNF (Eq a b) = And (makeNNF (Imp a b)) (makeNNF (Imp b a))
makeNNF (Imp a b) = makeNNF (Or (Not a) b)
makeNNF (Not (And a b)) = makeNNF (Or (Not a) (Not b))
makeNNF (Not (Or a b)) = makeNNF (And (Not a) (Not b))
makeNNF (Not a) = Not (makeNNF a)

remakeC :: Formula -> Formula -> Formula
remakeC (And a b) c = And (remakeC a c) (remakeC b c)
remakeC a (And b c) = And (remakeC a b) (remakeC a c)
remakeC a b = (Or a b)

helperC :: Formula -> Formula
helperC (And a b) = And (helperC a) (helperC b)
helperC (Or a b) = remakeC (helperC a) (helperC b)
helperC a = a

-- функция, которая делает формулу CNF
makeCNF = helperC . makeNNF

remakeD :: Formula -> Formula -> Formula
remakeD (Or a b) c = Or (remakeD a c) (remakeD b c)
remakeD a (Or b c) = Or (remakeD a b) (remakeD a c)
remakeD a b = (And a b)

helperD :: Formula -> Formula
helperD (Or a b) = Or (helperD a) (helperD b)
helperD (And a b) = remakeD (helperD a) (helperD b)
helperD a = a

-- функция, которая делает формулу DNF
makeDNF = helperD . makeNNF

a = (Var "a")
b = (Var "b")
c = (Var "c")
d = (Var "d")

test1 = makeDNF (Eq a b)
test2 = makeCNF (Or (Eq(Not a) b) (And a (Not (Not c))))
test3 = makeDNF (And (And (Eq (Not a) b) (Eq c (Not d))) (Or b c))


