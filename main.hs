-- Questão 01 - Implementar Interpolation Search -- 

-- Exemplo de uso:

-- itplSearch [4,3,2,5,6] x
-- x = 8
-- Output: -1
-- x = 5
-- Output: 3

itplSearch arr x = do
  let size = (length arr)
  i <- itplSearch' arr size 0 (size - 1) x
  return i

itplSearch' arr n lo hi x = do
  if (lo <= hi && x >= (arr !! lo) && x <= (arr !! hi)) then do
    let pos = lo + round (((fromIntegral (hi-lo)) / (fromIntegral ((arr !! hi) -  (arr !! lo)))) * (fromIntegral (x - (arr !! lo)) ))
    if (arr !! pos) == x then do return pos
    else do
        if (arr !! pos) < x then do
          itplSearch' arr n (pos + 1) hi x
        else do
          itplSearch' arr n (pos - 1) hi x
  else do return (fromIntegral (-1))

  -- Questão 02 - Given a sorted array and a number x, find a pair in array whose sum is closest to x 

-- Exemplo de uso:

-- closestSum [10, 22, 28, 29, 30, 40] x
-- x = 54
-- Output: (22,30)
-- x = 32
-- Output: (10,22)

-- closestSum [1, 3, 4, 7, 10] x
-- x = 15
-- Output: (4,10)
-- x = 32
-- Output: (7,10)

closestSum arr x = do
  let diff = maxBound :: Int
  let n = length arr
  closestSum' 0 0 0 (n-1) diff arr x

closestSum' rl rr l r diff arr x = do
  if (r > l) then do
    let checkDiff = absAux((arr !! l) + (arr !! r) - x)
    if checkDiff < diff then do
      aux l r l r checkDiff arr x
    else do
      aux rl rr l r diff arr x
  else do
    return (arr !! rl, arr !! rr)

aux rl rr l r diff arr x = do
  if (arr !! l) + (arr !! r) > x then do
    closestSum' rl rr l (r-1) diff arr x
  else do
    closestSum' rl rr (l+1) r diff arr x

absAux value | value >= 0 = value | otherwise = value * (-1)

-- Questão 03 - Encontrar floor e ceil de um número x dentro de um array a.
-- O número x pode não estar no array a 

-- Exemplo de uso:

-- getFloorAndCeil [10, 22, 28, 29, 30, 40] x
-- x = 28
-- Output: (28,28)
-- x = 13
-- Output: (10,22)

getFloorAndCeil arr x = do
  getFloorAndCeil' arr x  (maxBound :: Int)  (minBound :: Int) (0)

getFloorAndCeil' arr x ceil floor index | index >= (length arr) = (floor, ceil) | otherwise = aux' arr x ceil floor index

aux' :: [Int] -> Int -> Int -> Int -> Int -> (Int, Int)
aux' arr x  ceil floor index = do
  let actualValue = (arr !! index)
  let hasCeil = (actualValue >= x &&  actualValue <= ceil)
  let hasFloor = (actualValue <= x &&  actualValue >= floor)

  if (hasCeil == True && hasFloor == True) then do
    getFloorAndCeil' arr x actualValue actualValue (index+1)
  else if (hasCeil == True && hasFloor == False) then do
    getFloorAndCeil' arr x actualValue floor (index+1)
  else if (hasCeil == False && hasFloor == True) then do
    getFloorAndCeil' arr x ceil actualValue (index+1)
  else do
    getFloorAndCeil' arr x ceil floor (index+1)
  
-- Questão 04 - Given a sorted array an element x to be searched, find position of x in the array.

-- Exemplo de uso:

-- expSearch [10, 22, 28, 29, 30, 40] x
-- x = 28
-- Output: 2
-- x = 31
-- Output: -1

expSearch arr x = do
  let size = (length arr)
  i <- expSearch' arr size x
  return i

expSearch' arr n x = do
  if ((arr !! 0)  == x)
    then return 0
  else do
    binSearch arr (((auxUpdateValue 1 arr x n) `div` 2)) (minBetween (auxUpdateValue 1 arr x n) n) x

binSearch array left right element = do
  if (right >= left) then do
    let subRightLeft = right - left
    let auxMiddle = subRightLeft `div` 2
    let mid = left + auxMiddle

    if ((array !! mid) == element) then do
      return mid
    else do
      if ((array !! mid) > element) then do
        binSearch array left (mid-1) element
      else do
        binSearch array (mid+1) right element
  else do
    return (-1)

minBetween x y | x < y = x | otherwise = y

auxUpdateValue i arr x n | i < n && ((arr !! i) <= x) = auxUpdateValue (i*2) arr x n | otherwise = i

-- Questão 06 - Implementar uma pilha e seus algoritmos -- 

-- Exemplo de uso:

-- a = createStack
-- a (pra imprimir a no prompt)
-- Output: Stack []
-- b = popStack a
-- b
-- Output: *** Exception: Não é possível realizar o pop de uma pilha vazia.
--         CallStack (from HasCallStack):
--         error, called at q6.hs:9:18 in main:Main
-- c = pushStack 2 a
-- c
-- Output: Stack [2]
-- d = popStack c
-- c
-- Output: (2,Stack [])

data Stack a = Stack [a] deriving (Show)

createStack = Stack([])

pushStack x (Stack stack) = Stack (stack ++ [x])

popStack (Stack []) = error "Não é possível realizar o pop de uma pilha vazia."
popStack (Stack stack) = (last(stack), Stack(init(stack)))

emptyStack (Stack stack) | (length stack == 0) = True
                    | otherwise = False

peekStack (Stack stack) = last(stack)

searchStack x (Stack stack) = searchStack' x stack 1
searchStack' _ ([]) _ = (-1)
searchStack' x (stack) index | last(stack) /= x =  searchStack' x  (init(stack)) (index+1) | otherwise = index

-- Questão 07 - Implementar uma fila e seus algoritmos -- 

data Queue a = Queue [a] deriving Show

createQueue = Queue([])

addQueue x (Queue queue) = Queue (queue ++ [x])

removeQueue (Queue queue) = (head(queue) , Queue(tail(queue)))

elementQueue (Queue queue) = (head(queue), Queue(queue))

peekQueue (Queue queue) = head(queue)

pollQueue (Queue queue) | ((length queue) == 0) = Nothing | otherwise = Just (head queue)