module WinCondition where 

winCondition :: [[Char]] -> [Char]
winCondition gameMatrix
    | leftToRightFstRow /= '_' = leftToRightFstRow:" wins!!"
    | leftToRightSndRow /= '_' = leftToRightSndRow:" wins!!"
    | leftToRightThdRow /= '_' = leftToRightThdRow:" wins!!"
    | topToBottomFstRow /= '_' = topToBottomFstRow:" wins!!"
    | topToBottomSndRow /= '_' = topToBottomSndRow:" wins!!"
    | topToBottomThdRow /= '_' = topToBottomThdRow:" wins!!"
    | diagonalLeftRow   /= '_' = diagonalLeftRow:" wins!!"
    | diagonalRightRow /= '_'  = diagonalRightRow:" wins!!"
    | draw                     = "DRAW!!"
    | otherwise                = "----------------------------"
    where
        fstList = head gameMatrix
        sndList = gameMatrix !! 1
        thdList = gameMatrix !! 2
        topFstList = [head fstList, head sndList, head thdList] 
        topSndList = [fstList !! 1, sndList !! 1, thdList !! 1] 
        topThdList = [fstList !! 2, sndList !! 2, thdList !! 2] 
        diagonalLeftList  = [head fstList, sndList !! 1, thdList !! 2]
        diagonalRightList = [head thdList, sndList !! 1, fstList !! 2]
        leftToRightFstRow = filledRowCheck fstList
        leftToRightSndRow = filledRowCheck sndList
        leftToRightThdRow = filledRowCheck thdList
        topToBottomFstRow = filledRowCheck topFstList
        topToBottomSndRow = filledRowCheck topSndList
        topToBottomThdRow = filledRowCheck topThdList
        diagonalLeftRow   = filledRowCheck diagonalLeftList
        diagonalRightRow  = filledRowCheck diagonalRightList
        draw = notElem '_' fstList && notElem '_' sndList && notElem '_' thdList

filledRowCheck :: [Char] -> Char
filledRowCheck ls 
    | fstVal == sndVal && sndVal == thdVal = fstVal
    | otherwise = '_'
    where
        fstVal = head ls
        sndVal = ls !! 1
        thdVal = ls !! 2
