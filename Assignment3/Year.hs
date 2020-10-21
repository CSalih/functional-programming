

-- |
-- = A program for printing a calendar year
module Year where
    import System.Environment (getArgs)
    import Calendar
    import Picture
    import S03
    import Data.List
    
    {-| 'rjustify n xs' right-justifies its String argument
    'xs' inside a box of width 'n'. -}
    rjustify :: Int -> String -> String
    rjustify n xs
      | l <= n = replicate (n - l) ' ' ++ xs
      | otherwise = error ("text of length " ++ show l ++ " does not fit in box of width " ++ show n)
      where
        l = length xs
    
    -- | The names of all months in a year.
    months :: [String]
    months = ["January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"]
    
    {-| 'groupsOfSize n xs' splits the list 'xs' into segments
    of length 'n' (the last one may be shorter). -}
    groupsOfSize :: Int -> [a] -> [[a]]
    groupsOfSize n xs = if null ys then [] else ys : groupsOfSize n zs
      where
        (ys, zs) = splitAt n xs
    
    -- | A list of pictures of days of a given month in a given year.
    daysOfMonth :: Month -> Year -> [Picture]
    daysOfMonth m y =
      map (row . rjustify 3 . pic) [1-d..42-d]
      where
        (d, t) = monthInfo m y
        pic n = if 1 <= n && n <= t then show n else ""
    
    -- | A picture containing all days of a given month in a given year.
    month :: Month -> Year -> Picture
    month m y = tile $ groupsOfSize 7 $ daysOfMonth m y
    
    -- | A picture of all days of a given month in a given year with weekday header.
    monthPic :: Year -> Month -> Picture
    monthPic y m = smartStack [header, weekdays, month m y]
      where
        weekdays = row " Su Mo Tu We Th Fr Sa"
        header = row (months !! (m-1))
    
    -- | A picture for all 12 months of a given year.
    yearPic :: Year -> Int -> Picture
    yearPic y c = smartTile $
      map (intersperse (row "  ")) $ -- insert 2 spaces between columns
      groupsOfSize c $ map (monthPic y) [1..12]
    
    -- | Print a given year with month distributed over a given number of columns.
    printYear y c = putStr $ showPic $ smartStack [row (show y), row "", yearPic y c]
    
    -- | The main entry point of the program.
    main = do
      args <- getArgs
      case args of
        [y]    -> printYear (read y) 3
        [y, c] -> printYear (read y) (read c)
        _      -> error "expecting 1 or 2 arguments (year and optional number of columns)"
    
    