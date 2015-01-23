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

import Control.Exception
import Control.Monad         (guard)

import Data.Char             (toLower, toUpper, isNumber, isLetter)
import Data.Data
import Data.List             (intercalate)
import Data.List.Split       (splitOneOf)
import Data.Maybe            (fromJust)
import qualified Data.Text         as T
import qualified Data.Text.IO      as DT
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as LT
import Data.Time             (getCurrentTime)
import Data.Time.Format      (formatTime)

import Paladin.Raw

import System.IO             (hFlush, stdout)
import System.Console.ANSI
import System.Directory
import System.Environment    (getEnv)
import System.FilePath.Posix (takeDirectory, (</>))
import System.IO.Error
import System.Locale         (defaultTimeLocale)
import System.Process        (system)
import System.Random         (randomIO)

import Text.Hastache
import Text.Hastache.Context

-- import Paths_paladin_project (getDataFileName)

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

-- |Prompts the user with a question (and an optional hint)
-- and returns an answer
ask :: String -> Maybe String -> IO String
ask info hint = do
  bk $ "What is your " ++ info ++ "?" ++ (maybe "" (\h -> " ("++h++")") hint)
  putStr "> "
  hFlush stdout -- Because we want to ask on the same line
  response <- getLine
  return $ check response
    where check :: String -> String
          check x
            | x == "" = maybe "<unknown>" (\h -> h) hint
            | otherwise = x



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

-- |Safely read the ~/.gitconfig file
safeReadGitConfig :: IO L.Text
safeReadGitConfig = do
  e <- tryJust (guard . isDoesNotExistError)
       (do home <-getEnv "HOME"
           LT.readFile $ home </> ".gitconfig")
  return $ either (const (L.empty)) id e

-- |From a given string (obtained from gitconfig) extract the user's
-- name and address (if they exist)
getNameAndMail :: L.Text -> (Maybe String, Maybe String)
getNameAndMail gitConfigContent = (getFirstValueFor splitted "name",
                                   getFirstValueFor splitted "email")
  where
    -- make lines of words
    splitted :: [[L.Text]]
    splitted = map L.words $ L.lines gitConfigContent

-- |Get the first line which starts with 'elem ='
-- and return the third field (value)
getFirstValueFor :: [[L.Text]] -> String -> Maybe String
getFirstValueFor splitted key = firstJust $ map (getValueForKey key) splitted

-- |Return the first Just value of a list of Maybe
firstJust :: (Eq a) => [Maybe a] -> Maybe a
firstJust l = case dropWhile (==Nothing) l of
  [] -> Nothing
  (j:_) -> j

-- |Given a line of words ("word1":"word2":rest) getValue will return
-- rest if word1 == key 'elem =' or Nothing otherwise
getValueForKey :: String          -- key
                  -> [L.Text]     -- line of words
                  -> Maybe String -- the value if found
getValueForKey el (n:e:xs)
  | n == (L.pack el) && e == (L.pack "=") = Just $ L.unpack $ L.unwords xs
  | otherwise = Nothing
getValueForKey _ _ = Nothing


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

  project <- ask "project name" Nothing
  ioassert (checkProjectName project) "Use only letters, numbers, spaces and dashes please"
  let projectName = projectNameFromString project
      moduleName = camelCase project
  gitconfig <- safeReadGitConfig
  let (name, email) = getNameAndMail gitconfig
  in_author <- ask "name" name
  in_email <- ask "email" email
  in_ghaccount <- ask "github account" Nothing
  in_synopsis <- ask "project in less than a dozen words" Nothing
  current_year <- getCurrentYear
  createProject $ Project projectName moduleName in_author in_email in_ghaccount in_synopsis current_year

  -- Initialize git and cabal, and run tests
  _ <- system "git init ."
  _ <- system "cabal sandbox init"
  _ <- system "cabal install"
  _ <- system "cabal test"
  _ <- system $ "./.cabal-sandbox/bin/test-" ++ projectName
  end

  
