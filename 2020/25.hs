modulus = 20201227
log7Mod x n j = if n == x then j else log7Mod x ((7*n) `mod` modulus) (j+1)
doPow x n j = if j == 0 then n else doPow x ((n*x) `mod` modulus) (j-1)
main = do
  txt <- readFile "in/25.txt"
  let [x1, x2] = map read $ lines txt
  print $ doPow x2 1 $ log7Mod x1 1 0
