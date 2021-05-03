prods :: Int -> [Int]
prods n = _prods n n

_prods :: Int -> Int -> [Int]
_prods n a
  | a < n = []
  | otherwise = n*a : _prods n (a+1)

mix :: [Int] -> [Int] -> [Int]
mix [] b = b
mix a [] = a
mix (ahd:atl) (bhd:btl)
  | ahd == bhd = ahd : mix atl btl
  | ahd < bhd = ahd : mix atl (bhd:btl)
  | ahd > bhd = bhd : mix (ahd:atl) btl

sieve :: [Int] -> [Int] -> [Int]
sieve [] _ = []
sieve (nxtpot:pot) [] = nxtpot : sieve pot (prods nxtpot)
sieve (nxtpot:pot) (nxtcomp:comp)
  | nxtpot < nxtcomp = nxtpot : sieve pot (mix (prods nxtpot) (nxtcomp:comp))
  | otherwise = sieve pot comp

firstn :: Int -> [Int]
firstn n = take n $ sieve [2 ..] []

-- inclusively take until (custom implementation)
-- takeUntil :: (Int -> Bool) -> [Int] -> [Int]
-- takeUntil _ [] = []
-- takeUntil f (nxt:rst)
--   | f nxt = [nxt]
--   | otherwise = nxt : takeUntil f rst

primesto :: Int -> [Int]
primesto n = takeWhile (\p -> p <= n) $ sieve [2 ..] []

-- splitAt :: Int -> [a] -> ([a], [a])
-- splitAt 0 l = ([], l)
-- splitAt i (h:t) = let (f, s) = splitAt (i-1) t in ((h:f), s)

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [a] = [a]
mergesort lst = merge (mergesort lst1) (mergesort lst2) where
  middle = (length lst) `div` 2
  (lst1,lst2) = splitAt middle lst
  merge :: [Int] -> [Int] -> [Int]
  merge [] b = b
  merge a [] = a
  merge (ah:at) (bh:bt)
    | ah < bh = ah : (merge at (bh:bt))
    | otherwise = bh : (merge (ah:at) bt)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [a] = [a]
quicksort lst = quicksort left ++ mid ++ quicksort right where
  pivot lst = head lst -- a random number in the list
  partition _ [] = ([], [], [])
  partition p (num:rest)
    | num < p   = (num:left, mid, right)
    | num == p  = (left, num:mid, right)
    | otherwise = (left, mid, num:right)
    where 
      (left, mid, right) = partition p rest
  (left, mid, right) = partition (pivot lst) lst

-- -- split a list into two, at a certain key
-- splitBy :: (Eq a) => a -> [a] -> ([a], [a])
-- splitBy k [] = error "k not found in list"
-- splitBy k (h:t) 
--   | k == h = ([], (t))
--   | otherwise = let (f,s) = splitBy k t in (h:f, s)

-- from Dr. Denning
mywords :: String -> [String]
mywords s = parser [] s
    where
        isparen c = (c == '(' || c == ')')
        isop s = (s == '+' || s == '-' || s == '*' || s == '/')
        isdigit c = (c >= '0' && c <= '9')
        -- readint reads digits until non-digit
        -- returns ("digit","remainder of string")
        readint "" = ("","")
        readint (h:t)
            | (isdigit h) = (h:(fst (readint t)), snd (readint t))
            | otherwise  = ("", h:t)
        parser l "" = l
        parser l (h:t)
            | (isdigit h) = parser (l ++ [fst (readint (h:t))])
                                   (snd (readint (h:t)))
            | (isop h)    = parser (l ++ [h:""]) t
            | (isparen h) = parser (l ++ [h:""]) t
            | h == ' '    = parser l t

infix2rpn :: String -> String
infix2rpn inputStr = unwords $ lstinfix2rpn (mywords inputStr) [] where
  -- -- a map from operator to precedence (too complicated)
  -- operators = Map.fromList [ ("+", 1), ("-", 1), ("*", 2), ("/", 2), ("div", 2) ] 
  isop s = (s == "+" || s == "-" || s == "*" || s == "/")
  precedence "(" = 0 -- lowest precedence so never switched
  precedence "+" = 1
  precedence "-" = 1
  precedence "*" = 2
  precedence "/" = 2

  -- takes an input list, a stack of operators, returns the rpn format of the list
  lstinfix2rpn :: [String] -> [String] -> [String] 
  lstinfix2rpn [] ops = ops         -- no more atoms, return all operators on stack
  lstinfix2rpn (atom:tail) []       -- empty stack, push operator to stack, return the number
    | atom == "(" = lstinfix2rpn tail [atom]   -- push '(' to stack 
    | atom == ")" = error "unmatching parenthesis"
    | isop atom = lstinfix2rpn tail [atom]     -- atom is an operator
    | otherwise = atom : lstinfix2rpn tail []  -- atom is a number
  lstinfix2rpn (atom:tail) (op:optail)
    | atom == "(" = lstinfix2rpn tail (atom:op:optail)           -- push '(' to stack 
    | atom == ")" = if op == "("
                       then lstinfix2rpn tail optail             -- cancel out the parenthesis
                       else op : lstinfix2rpn (atom:tail) optail -- append the operator, continue searching for '('
    | isop atom = if precedence atom > precedence op
                     then lstinfix2rpn tail (atom:op:optail)     -- push atom to stack 
                     else op : lstinfix2rpn tail (atom:optail)   -- switch operator from stack
    | otherwise = atom : lstinfix2rpn tail (op:optail)           -- append number to result 

evalrpn :: String -> Int
evalrpn inputStr = lstevalrpn (mywords inputStr) [] where
  -- first arg is a list of atoms, second is the stack of numbers parsed so far
  -- when the next atom is an operator, evaluate the next two numbers
  isop s = (s == "+" || s == "-" || s == "*" || s == "/")
  compute :: Int -> String -> Int -> Int
  compute a op b
    | op == "+" = a + b
    | op == "-" = a - b
    | op == "*" = a * b
    | op == "/" = a `div` b
    | otherwise = error $ "unknown operator " ++ op
  lstevalrpn :: [String] -> [Int] -> Int
  lstevalrpn [] [num] = num
  lstevalrpn [] _ = error "too many numbers on stack"
  lstevalrpn (atom:tail) nums
    | isop atom && length nums < 2 = error $ "not enough item on stack for operator " ++ atom
    | isop atom = let (num1:num2:numtail) = nums in 
                      lstevalrpn tail ((compute num2 atom num1) : numtail) -- evaluate the first two items on stack
    | otherwise = lstevalrpn tail ((read atom :: Int) : nums)              -- push number to stack
