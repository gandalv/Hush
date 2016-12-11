module Main where

import Lang

main :: IO ()
main = do
    let s = initState [Lit (IntLit 3)]
    print s