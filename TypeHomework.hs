

data MonthOfYear = January | February | March | April | 
                  May | June | July | August |
                  September | October | November | December
                deriving(Show)


nextMonth :: MonthOfYear -> MonthOfYear
nextMonth m = case m of
    January         -> February
    February        -> March
    March           -> April
    April           -> May
    May             -> June
    June            -> July
    July            -> August
    August          -> September
    September       -> October
    October         -> November
    November        -> December
    December        -> January


daysInMonth :: MonthOfYear -> Integer
daysInMonth m = case m of
    January         -> 31
    February        -> 28
    March           -> 31
    April           -> 30
    May             -> 31
    June            -> 30
    July            -> 31
    August          -> 31
    September       -> 30 
    October         -> 31
    November        -> 30
    December        -> 31


nextDay :: Integer -> MonthOfYear -> (Integer, MonthOfYear)
nextDay num m
    | num+1 > daysInMonth m      = (1 , nextMonth m)
    | otherwise                 = (num+1 , m)


