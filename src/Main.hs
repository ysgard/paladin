{-# LANGUAGE DeriveDataTypeable #-}
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

import Data.Char             (toLower, toUpper, isNumber, isLetter)
import Data.Data
import Data.List             (intercalate)
import Data.List.Split       (splitOneOf)
import qualified Data.Text.IO      as DT (readFile)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Data.Time             (getCurrentTime)
import Data.Time.Format      (formatTime)

import Paladin.Raw

import System.IO             (hFlush, stdout)
import System.Console.ANSI
import System.Directory
import System.FilePath.Posix (takeDirectory, (</>))
import System.Locale         (defaultTimeLocale)
import System.Process        (system)
import System.Random         (randomIO)

import Text.Hastache
import Text.Hastache.Context

import Paths_paladin_project (getDataFileName)

data Project = Project { projectName :: String
                       , moduleName  :: String
                       , author      :: String
                       , mail        :: String
                       , ghaccount   :: String
                       , synopsis    :: String
                       , year        :: String
                       } deriving (Data, Typeable)
                           

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

-- |assert function - if the given expression doesn't evaluate
-- to true, bomb with an error.
ioassert :: Bool -> String -> IO ()
ioassert True _ = return ()
ioassert False str = holyError str

-- |Fun random error generator, not needed, just fun
holyError :: String -> IO ()
holyError str = do
  r <- randomIO
  if r then do
    bk "What... is your favourite colour?"
    you "Blue. no, yel..."
    else do
    bk "What is the capital of Assyria?"
    you "I don't know that!"
  putStrLn "[You are thrown over the edge into the volcano]"
  you $ "Auuuuuuuuuuugh " ++ str 
  bk "Hee hee heh."
  error "...has been thrown over the cliff!"
  
-- |Create the project directory and populate it
createProject :: Project -> IO ()
createProject p = do
  let context = mkGenericContext p
  createDirectory $ projectName p
  setCurrentDirectory $ projectName p
  genFile context gitignoreRaw $ ".gitignore"
  genFile context projectcabalRaw $ (projectName p) ++ ".cabal"
  genFile context mainRaw $ "src" </> "Main.hs"
  genFile context licenseRaw $ "LICENSE"
  genFile context setupRaw $ "Setup.hs"

-- |Load a file from the scaffold, and then fill
-- the template with values provided
genFile :: MuContext IO -> String -> FilePath -> IO ()
genFile context template outputFileName = do
  transformedFile <- hastacheStr defaultConfig (encodeStr template) context
  createDirectoryIfMissing True $ takeDirectory outputFileName
  LT.writeFile outputFileName transformedFile


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

-- |Return the current year
getCurrentYear :: IO String
getCurrentYear = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y" now


-- |Entry point
main :: IO ()
main = do
  intro

  project <- ask "project name"
  ioassert (checkProjectName project) "Use only letters, numbers, spaces and dashes please"
  let projectName = projectNameFromString project
      moduleName = camelCase project

  in_author <- ask "name"
  in_email <- ask "email"
  in_ghaccount <- ask "github account"
  in_synopsis <- ask "project in less than a dozen words"
  current_year <- getCurrentYear
  createProject $ Project projectName moduleName in_author in_email in_ghaccount in_synopsis current_year

  -- Initialize git and cabal, and run tests
  _ <- system "git init ."
  _ <- system "cabal sandbox init"
  _ <- system "cabal install"
  _ <- system "cabal test"
  _ <- system $ "./.cabal-sandbox/bin/test-" ++ projectName
  end

  
