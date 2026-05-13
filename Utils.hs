module Utils (getCh, getLine, putStr, cls, Pos, goto, writeAt, wrap, getString, startWith, identity, addNewlines, safeText) where

import System.IO hiding (getLine, putStr)
import Prelude hiding (getLine, putStr)
import Data.Char
import Data.List

type Pos= (Int, Int)

-- Display utils
--
getLine :: IO String
getLine= do {
            x <- getChar;
            if x=='\n' then
                return []
            else
                do{
                    xs <- getLine;
                    return (x:xs)
                }
            }
putStr :: String -> IO ()
putStr []= return ()
putStr (x:xs)= do {
                    putChar x;
                    putStr xs;
                }
putStrln :: String -> IO ()
putStrln xs= putStr (xs ++ "\n")

getCh :: IO Char
getCh= do{
            hSetEcho stdin False;
            x <- getChar;
            hSetEcho stdin True;
            return x;
        }

-- Screen utilities
cls :: IO()
cls= do {
            putStr "\ESC[2J";
            putStr "\ESC[H";
        }

writeAt :: Pos -> String -> IO ()
writeAt p xs= do {
                    goto p;
                    putStr xs;
                }
goto :: Pos -> IO ()
goto (x,y)= putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width, height:: Int
width = 10
height= 10

identity :: Char -> Char
identity x = x

addNewlines :: [String] -> [String]
addNewlines= map (++"\n")

getString :: [String] -> String
getString= foldr (++) ""

wrap :: (String, String) -> String -> String
wrap (l,r) s= l++s++r

startWith :: String -> String -> Bool
startWith _ ""= True
startWith "" _= False
startWith (x:xs) (y:ys) | x==y = startWith xs ys
                        | otherwise = False

toUpperStr :: String -> String
toUpperStr = map toUpper

singoletti :: [a] -> [[a]]
singoletti [] = []
singoletti (x:xs)= [x]:singoletti xs

ensureText :: String -> String

ensureText "{" = "\\{"
ensureText "}" = "\\}"
ensureText "$" = "\\$"
ensureText "\\"= "\\\\"
ensureText x= x


safeText xs= getString (map ensureText (singoletti xs))
-- safeLatex xs= getString (map ensure (singoletti xs))
