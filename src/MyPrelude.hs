{-
Filename: MyPrelude.hs

Description:
    A collection of my custom Haskell syntactic sugars for the custom prelude's
    ugly syntax
-}
module MyPrelude
    ( if' ) where

if' :: Bool -> a -> a -> a 
if' True x _  = x
if' False _ y = y