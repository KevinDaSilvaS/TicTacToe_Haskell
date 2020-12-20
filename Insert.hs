module Insert where

insert :: Char -> Int -> Int -> [[Char]] -> [[Char]]
insert elem row pos gameMatrix
    | row < 0 || row > 2 = []
    | pos < 0 || pos > 2 = []
    | posList /= '_'     = []
    | row == 0 = [returnList, sndList, thdList] 
    | row == 1 = [fstList, returnList, thdList] 
    | row == 2 = [fstList, sndList, returnList] 
    | otherwise = []
    where 
        list = gameMatrix !! row
        fstList = head gameMatrix
        sndList = gameMatrix !! 1
        thdList = gameMatrix !! 2
        posList = list !! pos
        returnList = buildList elem list pos

buildList :: (Eq a1, Num a1) => a2 -> [a2] -> a1 -> [a2]
buildList elem (x:xs) pos
    | pos == 0  = [elem, head xs, last xs]
    | pos == 1  = [x, elem, last xs]
    | otherwise = [x, head xs, elem]




