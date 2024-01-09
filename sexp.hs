module Main where

    main :: IO ()
    main = do
        putStrLn "Hello, World!"

    data Sexp f a = Atom a | List [Sexp f a] | Sexp f [Sexp f a]



