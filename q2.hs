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
