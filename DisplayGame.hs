module DisplayGame where

display :: [[Char]] -> IO ()
display gameMatrix = do 
    print "      |     |      "
    print ("   "++[fst]++"  |  "++[snd]++"  |  "++[thd]++"   ")
    print "------+-----+------"
    print ("   "++[fth]++"  |  "++[fft]++"  |  "++[sth]++"   ")
    print "------+-----+------"
    print ("   "++[svth]++"  |  "++[eith]++"  |  "++[nnth]++"   ")
    print "      |     |      "
    where 
        fstList = head gameMatrix
        sndList = gameMatrix !! 1
        thdList = gameMatrix !! 2
        fst  = head fstList
        snd  = fstList !! 1
        thd  = fstList !! 2
        fth  = head sndList
        fft  = sndList !! 1
        sth  = sndList !! 2
        svth = head thdList
        eith = thdList !! 1
        nnth = thdList !! 2

