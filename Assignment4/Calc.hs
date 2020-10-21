

{- To be compiled with

$ ghc --make -main-is Calc Calc.hs
-}
-- |
-- = A simple stack-based calculator
module Calc where
    import Stack (Stack)
    import qualified Stack
    import System.IO
    
    -- | The supported commands of the calculator.
    data Cmd = Push Int
             | Pop
             | Add
      deriving (Show, Read)
    
    -- | Executing a single command on a given 'Stack'.
    exec :: Cmd -> Stack Int -> Stack Int
    exec (Push i) s = Stack.push i s
    exec Pop s = snd $ Stack.pop s
    exec Add s =
      let (x, s1) = Stack.pop s in
      let (y, s2) = Stack.pop s1 in
      Stack.push (x + y) s2
    
    -- | The REPL of the calculator.
    calc :: Stack Int -> IO ()
    calc s = do
      putStrLn $ "stack: " ++ show s
      putStr "> "
      hFlush stdout
      l <- getLine
      if l == "exit" then return ()
      else calc (exec (read l) s)
    
    -- | Initialize the REPL with the empty 'Stack'.
    main = calc Stack.empty
    
    