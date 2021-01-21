
module IODemo where

import Control.Monad

import Data.Char

getTwoLines :: IO String 
getTwoLines =
    getLine >> getLine 

duplicateLine :: IO String 
duplicateLine =
    fmap (\ x -> x ++ x) getLine 

joinTwoLines :: IO String 
joinTwoLines = liftM2 (++) getLine getLine 


flipTwoLines :: IO String
flipTwoLines = 
    liftM2 (flip (++)) getLine getLine 

shout :: IO String 
shout = 
    fmap (fmap toUpper) getLine 

shoutback :: IO ()
shoutback = 
    shout >>= \x -> putStrLn x

shoutBackTwice :: IO ()
shoutBackTwice =
    shout >>= \ x -> putStrLn x >> putStrLn x


-- optioning out of doing IO

myprint :: Show a => a -> IO ()
myprint x = putStrLn (show x)
