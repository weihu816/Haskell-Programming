In-class exercise (IO Monad)
============================

> {-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}
> import System.FilePath

Part 1
------

Rewrite these programs so that they do not use `do` notation.
(Make sure that you do not change their behavior!)
         
> simpleProgram :: IO ()
> simpleProgram =
>    putStrLn "This is a simple program that does IO." >>
>    putStrLn "What is your name?" >>
>    getLine >>= (\inpStr ->
>		putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
>	 )    


> lengthProgram :: IO Int
> lengthProgram =
>    let x = length [1,2,3,4,5,6] in
>    putStrLn ("The length of the list is" ++ show x) >>
>    return x     

> anotherProgram :: IO ()
> anotherProgram =
>    putStrLn "What is your name?" >>  
>    getLine >>= ( \inpStr ->
>		if (inpStr == "Haskell")
>			then do putStrLn "You rock!" >> return ()
>		else putStrLn ("Hello " ++ inpStr) >>
>			putStrLn "That's all!"  
>	 )


Part 2
------
        
Implement a simple lhs -> hs converter.

Your program should take the name of a file (such as `IOExercise.lhs`) and
create a new file with the same basename (`IOExercise.hs`).  Your converter
should only output the Haskell code in the file (i.e. it should only output
lines that start with "> " in the lhs file, and it should remove those two
characters from the beginning of the line.)

For file IO, you can use the following two functions from the standard library:
          
    readFile  :: FilePath -> IO String          
    
    writeFile :: FilePath -> String -> IO ()

A `FilePath` is just another name for `String` in Haskell. You also want to
use the following function (from `System.FilePath`) to replace the extension
of a filename:

    replaceExtension :: FilePath -> String -> FilePath

For example,

    ghci> replaceExtension "MonadExercise.lhs" "hs"
    "MonadExercise.hs"
    
> lhs2hs filename =
>	readFile filename >>= ( \x ->
>		writeFile (replaceExtension filename "hs")
>			(unlines (filter (\t -> case t of ('>':_) -> True; _ -> False) (lines x)))
>	)

