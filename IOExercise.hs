> {-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}
> import System.FilePath
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
> lhs2hs filename =
>	readFile filename >>= ( \x ->
>		writeFile (replaceExtension filename "hs")
>			(unlines (filter (\t -> case t of ('>':_) -> True; _ -> False) (lines x)))
>	)
