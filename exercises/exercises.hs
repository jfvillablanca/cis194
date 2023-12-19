-- Based on exercises from this video: https://youtu.be/Cxkqrg8FCt8

-- TODO: Exercise #1:
-- Create a function `elem'` that returns True
-- if an element is in a given list and returns
-- False otherwise

-- My solution
elem' :: (Eq t) => t -> [t] -> Bool
elem' _ [] = False
elem' e li = fd e li
  where
    fd y (x : xs)
        | y == x = True
        | null xs = False
        | otherwise = fd y xs

-- NOTE: Video solution:
-- elem' :: (Eq t) => t -> [t] -> Bool
-- elem' _ [] = False
-- elem' a (x : xs) = a == x || elem' a xs

-- TODO: Exercise #2:
-- Create a function `nub'` that removes all
-- duplicates from a given list

-- My solution
nub' :: (Eq t) => [t] -> [t]
nub' [] = []
nub' wDupes = dedupe [] wDupes
  where
    dedupe acc [] = reverse acc
    dedupe acc (x : xs)
        | x `notElem` acc = dedupe (x : acc) xs
        | otherwise = dedupe acc xs

-- NOTE: Video solution:
-- ** This solution does not preserve order **
-- nub' :: (Eq t) => [t] -> [t]
-- nub' [] = []
-- nub' (x : xs)
--     | x `elem` xs = nub' xs
--     | otherwise = x : nub' xs

-- TODO: Exercise #3:
-- Create a function `isAsc'` that returns
-- True if the list given to it is a
-- non-descending list else False

-- My solution
isAsc' :: [Int] -> Bool
isAsc' [] = True
isAsc' [x, y] = x <= y
isAsc' (x : y : xs) = x <= y && isAsc' (y : xs)

-- NOTE: Video solution:
-- isAsc' :: [Int] -> Bool
-- isAsc' [] = True
-- isAsc' [x] = True
-- isAsc' (x : y : xs) = x <= y && isAsc' (y : xs)

-- TODO: Exercise #4:
-- Create a function `hasPath'` that determines
-- if a path from one node to another exists
-- within a directed graph
-- No need to check if node is part of the graph
-- Implicitly assumed that the nodes exist
-- e.g. `hasPath' [(1,2)] 3 4 -- evaluates to False not an error

-- My solution
hasPath' :: [(Int, Int)] -> Int -> Int -> Bool
hasPath' [] _ _ = False
hasPath' ((nodeA, nodeB) : nodes) x y
    | nodeB == y = True
    | nodeA == x = hasPath' nodes nodeB y
    | otherwise = hasPath' nodes x y

-- NOTE: Video solution:
-- ** This is a different solution with a different solution space **
-- ** Creates a new list that prunes out possible source of cycles **
-- ** As a side effect, a node is always reachable to itself, i.e. x == y **
-- ** I don't have this behavior in my solution **
-- hasPath' :: [(Int, Int)] -> Int -> Int -> Bool
-- hasPath' [] x y = x == y
-- hasPath' xs x y
--     | x == y = True
--     | otherwise =
--         let xs' = [(n, m) | (n, m) <- xs, n /= x]
--          in or [hasPath' xs' m y | (n, m) <- xs, n == x]
