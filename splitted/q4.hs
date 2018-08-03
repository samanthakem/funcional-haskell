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