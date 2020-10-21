{- To be compiled with

$ ghc --make -main-is NL NL.hs
-}
-- |
-- = A program adding line numbers to its input.
module NL where

    -- | Number the lines in a given 'String'.
    numberLines :: String -> String
    numberLines = unlines . map showLine . zip [1..] . lines
      where
        showLine (n, l) = rjustify 6 (show n) ++ "\t" ++ l
        rjustify i t = replicate (i - length t) ' ' ++ t
    
    -- | Apply the pure function 'numberLines' to transform standard input of the
    -- program into its standard output.
    main = interact numberLines