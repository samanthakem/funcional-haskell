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