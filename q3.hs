
-- Returns (floor, ceil)
-- Se não existir ceil retorna o maior inteiro que existe
-- Se não existir floor retorna o menor inteiro que existe
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
  