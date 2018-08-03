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
