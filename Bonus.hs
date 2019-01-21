import Data.Array

maxSplit :: [(Int,Int)] -> Int -> Int
maxSplit lengths wmax = table ! wmax
  where
    table = array (0, wmax) [(w, best w) | w <- [0..wmax]]
    best 0 = 0
    best w = maximum (0:[vi + table ! (w - wi) | (wi, vi) <- lengths, wi <= w])

ack :: (Num a, Num t, Ord a, Eq t) => a -> t -> t
ack m n
  |m == 0 = n+1
  |m > 0 && n == 0 = ack (m-1) 1
  |m > 0 && n /= 0 = ack (m-1) (ack m (n-1))

{--
The ackermann function terminates for m = 0.

for m > 0 AND n = 0, m decreases by 1 in the next iteration, meaning that it
  approaches termination.
for m > 0 AND n > 0, in the outer funciton m decreases, so if the inner function
  terminates, so does the outer function.
for the inner funciton, n decreases by 1 in each iteration, meaning that it
  eventuall reaches zero, which is a case of m > 0 AND n = 0, which we know
  terminates.

Using tables isn't a feasible solution because the same computation is never
carried out twice.
===========================================================
AUFGABE 3
ANY path that doesn't include a value higher than the current highest is optimal
if starting with that value.
[A.B,D], [A,B,E,D] both qualify as optimal give the problem statement, even though
[B,D] isn't optimal due to [B,E] being the "better" option. Therefore bellman's
principle doesn't hold for the given problem statement.
--}
divisors :: Integral a => a -> [a]  --From cheatsheet
divisors n = [d | d <- [1..(n `div` 2)], n `mod` d == 0]

isPerfect :: Int -> Bool
isPerfect n = sum(divisors(n)) == n

genPerfect :: [Int]
genPerfect = [x | x <- [1..], isPerfect x]

-- The first five perfect numbers are in the range of Int.
