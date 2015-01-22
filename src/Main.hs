{-|
Module : Paladin
Description: A cabal project scaffolder, based on Holy-Haskell-Starter
Copyright: (c) Jan Van Uytven, 2015
License: MIT
Maintainer : ysgard@gmail.com
Stability: experimental
Portability: POSIX

Paladin is based on Yann Esposito's 'Holy-Haskell-Starter', which is here:

   http://yannesposito.com/Scratch/en/blog/Holy-Haskell-Starter/

With only minor customizations because my needs are different (and admittedly,
much simpler!)

-}
module Main where

import Data.Char       (toLower, toUpper, isNumber, isLetter)
import Data.List       (intercalate)
import Data.List.Split (splitOneOf)

import System.IO       (hFlush, stdout)
import System.Console.ANSI


-- |The 'colorPutStr' function prints a colored string
colorPutStr :: Color -> String -> IO ()
colorPutStr color str = do
  setSGR [ SetColor Foreground Dull color
         , SetConsoleIntensity NormalIntensity
         ]
  putStr str
  setSGR []

-- |The bridgekeeper, the one who would challenge you.
bk :: String -> IO ()
bk str = colorPutStr Green $ "Bridgekeeper: " ++ str ++ "\n"

-- |Bridgekeeper, with no line return
bkn :: String -> IO ()
bkn str = colorPutStr Green $ "Bridgekeeper: " ++ str

-- |Narrative 'you'
you :: String -> IO ()
you str = colorPutStr Yellow $  "You: " ++ str ++ "\n"

-- |Prompts the user and returns an answer
ask :: String -> IO String
ask info = do
  bk $ "What is your " ++ info ++ "?"
  putStr "> "
  hFlush stdout -- Because we want to ask on the same line
  getLine

-- |Verify that the project name is conformant
checkProjectName :: String -> Bool
checkProjectName [] = False
checkProjectName str =
  all (\c -> isLetter c || isNumber c || c == '-' || c == ' ') str

-- |Lowercase a string and replace its spaces with dashes.
-- For example: "Holy Haskell Starter" becomes "holy-haskell-starter"
projectNameFromString :: String -> String
projectNameFromString str = intercalate "-" $ splitOneOf " -" $ map toLower str

-- |Create a module name from a string using CamelCase
-- For example, "Holy project" becomes "HolyProject"
camelCase :: String -> String
camelCase str = concatMap capitalizeWord (splitOneOf " -" str)
  where
    capitalizeWord :: String -> String
    capitalizeWord (x:xs) = toUpper x : map toLower xs
    capitalizeWord _      = []


-- |Introduction to paladin
intro :: IO ()
intro = do
  bk "Stop!"
  bk "Who would cross the Bridge of Death"
  bk "must answer me these questions three,"
  bk "ere the other side he see."
  you "Ask me the questions, bridgekeeper, I am not afraid.\n"

-- |Called at the end of Paladin
end :: IO ()
end = do
  putStrLn "\n\n"
  bk "What... is the air-speed velocity of an unladen swallow?"
  you "What do you mean? An African or European swallow?"
  bk "Huh? I... I don't know that."
  putStrLn "[The bridgekeeper is thrown over]"
  bk "Auuuuuuuuuuuugh"
  putStrLn "Sir Bedevere: How do you know so much about swallows?"
  you "Well, you have to know these things when you're a king, you know."

-- |Entry point
main :: IO ()
main = do
  intro
  _ <- ask "project name"
  _ <- ask "name"
  _ <- ask "email"
  _ <- ask "github account"
  _ <- ask "project in less than a dozen words"
  end
  
