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
  