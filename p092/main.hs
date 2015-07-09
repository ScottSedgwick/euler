import Data.Digits
import Data.List

c89 :: Int -> Bool
c89 1  = False
c89 89 = True
c89 n  = c89 $ sum $ map (^ 2) $ digits 10 n

findC89s :: [Int] -> [Int]
findC89s xs = foldl' (\acc x -> if c89 x then x : acc else acc) [] xs

range = [1..10000000]

main :: IO()
main = do
  putStrLn $ show $ length $ findC89s range