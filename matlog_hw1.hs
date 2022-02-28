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

test1 | makeDNF (Eq a b) == Or (Or (And (Not (Var "a")) (Not (Var "b"))) (And (Not (Var "a")) (Var "a"))) (Or (And (Var "b") (Not (Var "b"))) (And (Var "b") (Var "a"))) = "Test 1 passed" | otherwise = "Test 1 failed"
test2 | makeCNF (Or (Eq(Not a) b) (And a (Not (Not c)))) == And (And (Or (Or (Var "a") (Var "b")) (Var "a")) (Or (Or (Var "a") (Var "b")) (Var "c"))) (And (Or (Or (Not (Var "b")) (Not (Var "a"))) (Var "a")) (Or (Or (Not (Var "b")) (Not (Var "a"))) (Var "c"))) = "Test 2 passed" | otherwise = "Test 2 failed"
test3 | makeDNF (And (And (Eq (Not a) b) (Eq c (Not d))) (Or b c)) == Or (Or (Or (Or (Or (And (And (And (Var "a") (Not (Var "b"))) (And (Not (Var "c")) (Var "d"))) (Var "b")) (And (And (And (Var "a") (Not (Var "b"))) (And (Not (Var "c")) (Var "d"))) (Var "c"))) (Or (And (And (And (Var "a") (Not (Var "b"))) (And (Not (Var "c")) (Var "c"))) (Var "b")) (And (And (And (Var "a") (Not (Var "b"))) (And (Not (Var "c")) (Var "c"))) (Var "c")))) (Or (Or (And (And (And (Var "a") (Not (Var "b"))) (And (Not (Var "d")) (Var "d"))) (Var "b")) (And (And (And (Var "a") (Not (Var "b"))) (And (Not (Var "d")) (Var "d"))) (Var "c"))) (Or (And (And (And (Var "a") (Not (Var "b"))) (And (Not (Var "d")) (Var "c"))) (Var "b")) (And (And (And (Var "a") (Not (Var "b"))) (And (Not (Var "d")) (Var "c"))) (Var "c"))))) (Or (Or (Or (And (And (And (Var "a") (Not (Var "a"))) (And (Not (Var "c")) (Var "d"))) (Var "b")) (And (And (And (Var "a") (Not (Var "a"))) (And (Not (Var "c")) (Var "d"))) (Var "c"))) (Or (And (And (And (Var "a") (Not (Var "a"))) (And (Not (Var "c")) (Var "c"))) (Var "b")) (And (And (And (Var "a") (Not (Var "a"))) (And (Not (Var "c")) (Var "c"))) (Var "c")))) (Or (Or (And (And (And (Var "a") (Not (Var "a"))) (And (Not (Var "d")) (Var "d"))) (Var "b")) (And (And (And (Var "a") (Not (Var "a"))) (And (Not (Var "d")) (Var "d"))) (Var "c"))) (Or (And (And (And (Var "a") (Not (Var "a"))) (And (Not (Var "d")) (Var "c"))) (Var "b")) (And (And (And (Var "a") (Not (Var "a"))) (And (Not (Var "d")) (Var "c"))) (Var "c")))))) (Or (Or (Or (Or (And (And (And (Var "b") (Not (Var "b"))) (And (Not (Var "c")) (Var "d"))) (Var "b")) (And (And (And (Var "b") (Not (Var "b"))) (And (Not (Var "c")) (Var "d"))) (Var "c"))) (Or (And (And (And (Var "b") (Not (Var "b"))) (And (Not (Var "c")) (Var "c"))) (Var "b")) (And (And (And (Var "b") (Not (Var "b"))) (And (Not (Var "c")) (Var "c"))) (Var "c")))) (Or (Or (And (And (And (Var "b") (Not (Var "b"))) (And (Not (Var "d")) (Var "d"))) (Var "b")) (And (And (And (Var "b") (Not (Var "b"))) (And (Not (Var "d")) (Var "d"))) (Var "c"))) (Or (And (And (And (Var "b") (Not (Var "b"))) (And (Not (Var "d")) (Var "c"))) (Var "b")) (And (And (And (Var "b") (Not (Var "b"))) (And (Not (Var "d")) (Var "c"))) (Var "c"))))) (Or (Or (Or (And (And (And (Var "b") (Not (Var "a"))) (And (Not (Var "c")) (Var "d"))) (Var "b")) (And (And (And (Var "b") (Not (Var "a"))) (And (Not (Var "c")) (Var "d"))) (Var "c"))) (Or (And (And (And (Var "b") (Not (Var "a"))) (And (Not (Var "c")) (Var "c"))) (Var "b")) (And (And (And (Var "b") (Not (Var "a"))) (And (Not (Var "c")) (Var "c"))) (Var "c")))) (Or (Or (And (And (And (Var "b") (Not (Var "a"))) (And (Not (Var "d")) (Var "d"))) (Var "b")) (And (And (And (Var "b") (Not (Var "a"))) (And (Not (Var "d")) (Var "d"))) (Var "c"))) (Or (And (And (And (Var "b") (Not (Var "a"))) (And (Not (Var "d")) (Var "c"))) (Var "b")) (And (And (And (Var "b") (Not (Var "a"))) (And (Not (Var "d")) (Var "c"))) (Var "c")))))) = "Test 3 passed" | otherwise = "Test 3 failed"
test4 | makeNNF (Not (And (Not a) b)) == Or (Var "a") (Not (Var "b")) = "Test 4 passed" | otherwise = "Test 4 failed"
test5 | makeCNF (Or a (Eq (Or a b) (Not b))) == And (And (Or (Var "a") (Or (Not (Var "a")) (Not (Var "b")))) (Or (Var "a") (Or (Not (Var "b")) (Not (Var "b"))))) (Or (Var "a") (Or (Var "b") (Or (Var "a") (Var "b")))) = "Test 5 passed" | otherwise = "Test 5 failed"

runAll = do
	putStrLn(test1)
	putStrLn(test2)
	putStrLn(test3)
	putStrLn(test4)
	putStrLn(test5)
