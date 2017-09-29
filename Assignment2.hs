module Assignment2 where
{-
Assignment2 - Hopscotch
By: Christian Mellows (10147353) Tyler Doyle (10129777)
In this assignment we create a function to find the highest scoring path in a new version of hopscotch 
where the goal is to have the highest point total.
-}

type HopPath = ([Int],Int) -- defining a tuple

--The hopscotch function finds the best scoring route through a given course
hopscotch :: [Int] -> HopPath
hopscotch [] = ([], 0) 
hopscotch [x] = ([x], x)
hopscotch course = bestPath (paths course)

--The paths function generates all possible paths on a given course
paths :: [Int] -> [[Int]]
paths [] = []
paths [x] = [[x]]
paths [x,_] = [[x]]
paths [x,_,y] = [[x,y],[x]] -- faster than recursion
paths (x:_:y:xs) = [x:w | w<-paths (y:xs)] ++ [x:z | z<-paths xs] --concatenates the possible jump by 2 and 3 paths

--finds the fastest path through the course given all the paths and returns it and its value
bestPath::[[Int]] -> HopPath
bestPath gpaths = (compsum gpaths , sum(compsum gpaths))

--will return the list with the highest sum 
compsum:: [[Int]] -> [Int]
compsum [] = []
compsum (x:xs)
    |sum(x)> sum(compsum xs) = x --if the sum of the current list is greater than the greatest of all preceding lists
    |otherwise = compsum xs
