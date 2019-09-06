module SHMLNormaliser where

import Data.List
import Data.Char
import SHMLParser as Parser

-- Substitution of free variables
sub :: Formula -> String -> Formula -> Formula
sub phi v psi = 
    case psi of
        LVar u
            | u == v    -> phi
            | otherwise -> psi
        Con f1 f2 -> Con (sub phi v f1) (sub phi v f2)       
        Max u f 
            | u == v    -> psi
            | otherwise -> Max u (sub phi v f)
        Nec p c f -> Nec p c (sub phi v f)
        _ -> psi


-- Replace free/bound variables of a formula
-- (Possibly introduces variable capture)
replace :: String -> String -> Formula -> Formula
replace x y phi =
    case phi of
        LVar u
            | u == x    -> LVar y
            | otherwise -> phi
        Con f1 f2 -> Con (replace x y f1) (replace x y f2)       
        Max u f 
            | u == x    -> Max y (replace x y f)
            | otherwise -> Max u (replace x y f)
        Nec p c f -> Nec p c (replace x y f)
        _ -> phi


-- Basic Logical Simplifications (step 1)
simplify :: Formula -> Formula
simplify (Con phi psi) = simplifyCon (simplify phi) (simplify psi)
    where
        simplifyCon :: Formula -> Formula -> Formula
        simplifyCon FF _ = FF
        simplifyCon _ FF = FF
        simplifyCon TT b = b
        simplifyCon b TT = b
        simplifyCon a b
            | a == b     = a
            | otherwise  = (Con a b)
simplify (Max x psi) = simplifyMax x (simplify psi)
    where
        simplifyMax :: String -> Formula -> Formula
        simplifyMax x TT = TT
        simplifyMax x FF = FF
        simplifyMax x (LVar y) 
            | x == y     = TT
            | otherwise  = Max x (LVar y)
        simplifyMax x (Con phi psi)
            | phi == LVar x = simplify (Max x psi)
            | psi == LVar x = simplify (Max x phi)
            | otherwise     = Max x (Con phi psi)
        simplifyMax x phi = Max x phi
simplify (Nec p c phi)
    | simpPhi == TT = TT
    | otherwise     = Nec p c simpPhi
    where
        simpPhi = simplify phi
simplify phi = phi


-- Standard form (step 2)
sf :: Formula -> Formula
sf f = simplify (conj (sf' f []))
    where
        conj :: (Formula, [String]) -> Formula
        conj (phi, [])  = phi
        conj (phi, v:vs) = Con phi (conj (LVar v, vs))

sf' :: Formula -> [String] -> (Formula, [String])
sf' (LVar x) bv 
    | x `elem` bv  = (LVar x, [])
    | otherwise    = (TT, [x])
sf' (Con phi1 phi2) bv = (Con psi1 psi2, nub (vars1 ++ vars2))
    where
        (psi1, vars1) = sf' phi1 bv
        (psi2, vars2) = sf' phi2 bv
sf' (Max x phi) bv = (sub (Max x psi) x psi, delete x vars)
    where
        (psi, vars) = sf' phi (x:bv)
sf' (Nec p c phi) bv = (Nec p c (sf phi), [])
sf' phi _ = (phi, [])


-- All variables which appear in formula (free or bound)
variables :: Formula -> [String]
variables = nub . variables'

variables' :: Formula -> [String]
variables' (LVar x) = [x]
variables' (Con phi psi) = (variables' phi) ++ (variables' psi)
variables' (Max x phi) = [x] ++ (variables' phi)
variables' (Nec p c phi) = variables' phi
variables' _ = []


-- Rename the variables in a formula using integers
rename :: Formula -> (Formula, [(Int, String)])
rename phi = (psi, sigma)
    where
        sigma = zip [0..] (variables phi)
        
        listReplace :: [(Int, String)] -> Formula -> Formula
        listReplace (p:ps) = 
            (listReplace ps).(replace (snd p) (show (fst p)))
        listReplace [] = id

        psi = listReplace sigma phi

  
-- Equation 'X = phi' encoded as (X, phi)
type Equation = (String, Formula)
type SoE      = ([Equation], String, [String])


-- System of Equations (step 3)
sysEq :: Formula -> (SoE, [(Int, String)])
sysEq phi = (sysEq' 0 phi', sigma)
    where
        (phi', sigma) = rename phi

sysEq' :: Int -> Formula -> SoE
sysEq' n TT = ([(x, TT)], x, [])
    where
        x = "X" ++ show n

sysEq' n FF = ([(x, FF)], x, [])
    where
        x = "X" ++ show n

sysEq' n (LVar y) = ([(x, LVar y)], x, [y])
    where
        x = "X" ++ show n

sysEq' n (Con f1 f2) = (eq, x, y1 ++ y2)
    where
        x = "X" ++ show n
        (eq1, x1, y1) = sysEq' (n+1) f1
        lastEq1 = read ((tail.fst.last) eq1) :: Int
        (eq2, x2, y2) = sysEq' (lastEq1+1) f2
        eq = [(x, Con (snd (head eq1)) (snd (head eq2)))] ++ eq1 ++ eq2

sysEq' n (Max u f) = (eq, x, y)
    where
        x = "X" ++ show n
        (eq1, x1, y1) = sysEq' (n+1) (replace u x f)
        
        expandX :: Equation -> Equation
        expandX (v, rhs) 
            | rhs == LVar x = (v, snd(head eq1))
            | otherwise     = (v, rhs)

        eq = [(x, snd(head eq1))] ++ (map expandX eq1)
        y = filter (\v->v/=x) y1

sysEq' n (Nec p c f) = (eq, x, y)
    where
        x = "X" ++ show n
        (eq1, x1, y) = sysEq' (n+1) f
        eq = [(x, Nec p c (LVar x1))] ++ eq1


-- Normalisation of System of Equations (Power Set Construction, step 4)

-- The following functions are for subset/index manipulation
-- nsubsets (Non-empty subsets, with singletons first, then lexicographical)
nsubsets :: Eq a => [a] -> [[a]]
nsubsets s = [[i]|i<-s] ++ (subsequences s \\ ([]:[[i]|i<-s]))

-- Index (subscript) of a variable Xi
idx :: String -> Int
idx (x:xs) | x == 'X'  = read xs :: Int
           | otherwise = -1

-- Subset corresponding to given index
subIdx :: Int -> Int -> [Int]
subIdx n = (!!) $ nsubsets [0..n-1] 
        
-- Index corresponding to given Subset
idxSub :: Int -> [Int] -> Int
idxSub n [k] | k < n      = k
             | otherwise  = error "Not a valid subset"
idxSub n s = binarysum (n-1) (reverse memberQSet) + n - 2 - maximum s
    where 
        binarysum k []     = 0
        binarysum k (x:xs) = (2^k * x) + binarysum (k-1) xs 
        btoi True  = 1
        btoi False = 0
        memberQSet = [btoi (i `elem` s) | i <- [0..(n-1)]] 


-- All symbolic actions in a formula
sas :: Formula -> [(Patt, BExpr)]
sas = nub . sas'

sas' :: Formula -> [(Patt, BExpr)]
sas' (Nec p c phi) = (p,c) : sas' phi
sas' (Con phi psi) = (sas' phi) ++ (sas' psi)
sas' (Max x phi) = sas' phi
sas' _ = []

-- Factor (i.e. normalise) a single equation in SoE with n equations
factor :: Int -> Equation -> Equation
factor n (v, FF) = (v, FF)
factor n (v, LVar x) = (v, LVar x)
factor n (v, rhs)
    = (v, bigWedge ((map saVarToFormula $ saVarPairs rhs) ++ (unguardedVars rhs)))
    where
        saVars (p,c) (Nec p' c' (LVar x)) 
            | p == p' && c == c' = [x]
            | otherwise          = []
        saVars (p,c) (Con phi psi) = saVars (p,c) phi ++ saVars (p,c) psi
        saVars (p,c) _ = []

        guardedVars phi = concat [saVars sa phi | sa <- sas phi]
        unguardedVars phi = map (\x -> LVar x) (variables phi \\ guardedVars phi)
        saVarPairs phi = [(sa, map idx $ saVars sa phi) | sa <- sas phi]

        saVarToFormula ((p,c), v) = Nec p c (LVar ("X" ++ show (idxSub n v)))

        bigWedge [] = FF
        bigWedge lst = foldl1 (\x y -> Con x y) lst

-- Normalisation of SoE's
norm :: (SoE, a) -> (SoE, a)
norm ((eq, x, y), sigma) = ((map (factor n) psEqs, x, y), sigma)
    where
        n = length eq
        conj = \x y -> Con x y
        lhs = ["X" ++ show i | i <- [0..2^n-2]]
        rhs = map (foldl1 conj) $ (nsubsets.snd.unzip) eq
        psEqs = zip lhs rhs


-- Formula Reconstruction (step 5)

-- Free variables
fv :: Formula -> [String]
fv (LVar x)      = [x]
fv (Con phi psi) = fv phi ++ fv psi
fv (Nec p c phi) = fv phi
fv (Max x phi)   = fv phi \\ [x] 
fv _             = []

-- Compose a list of maps
compose :: [a -> a] -> (a -> a)
compose [] = id
compose (f:fs) = f . (compose fs)

-- Reconstruction 
reconstruct :: (SoE, [(Int, String)]) -> Formula
reconstruct ((eq, x, y), sigma) = sigma' recon
    where 
        recon = sigmaSHML (LVar x) (eq, x, y)
        sigma' = compose [replace (show u) v | (u,v) <- sigma]

-- Recursive SigmaSHML Map
sigmaSHML :: Formula -> SoE -> Formula
sigmaSHML phi (eq, x, y)
    | fv phi == []      = phi
    | fv phi `subset` y = phi
    | otherwise         = sigmaSHML ((compose subs) phi) (eq, x, y)
        where
            getEq v = case lookup v eq of
                Nothing  -> TT
                Just rhs -> rhs

            subs = [sub (Max x (getEq x)) x | x <- fv phi]

            subset (a:as) b = elem a b && subset as b
            subset [] b = True


-- Redundant fixed points (step 6)
redfix :: Formula -> Formula
redfix (Max x phi) 
    | x `elem` (fv phi) = Max x (redfix phi)
    | otherwise         = redfix phi
redfix (Con phi psi) = Con (redfix phi) (redfix psi)
redfix (Nec p c phi) = Nec p c (redfix phi)
redfix phi = phi


-- Normal Form (all steps in order)
nf :: Formula -> Formula
nf = redfix . reconstruct . norm . sysEq . sf . simplify

-- Normal Form from string
nfs :: String -> Formula
nfs = nf . parseF