{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Lens.Operators ((^?))
import Control.Monad          (guard, (<=<))

import Data.Aeson.Encode      (encodeToTextBuilder)
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BL
import Data.Char              (toLower, toUpper, isNumber, isLetter)
import Data.Data
import Data.List              (intercalate)
import Data.List.Split        (splitOneOf)
import Data.Maybe             (fromJust)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TI
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as TLI
import Data.Time              (getCurrentTime)
import Data.Time.Format       (formatTime)

import Network.HTTP.Conduit

import Paladin.Raw

import System.IO              (hFlush, stdout)
import System.Console.ANSI
import System.Directory
import System.Environment     (getEnv)
import System.FilePath.Posix  (takeDirectory, (</>))
import System.IO.Error
import System.Locale          (defaultTimeLocale)
import System.Process         (system)
import System.Random          (randomIO)

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
  setSGR [ SetColor Foreground Vivid color
         , SetConsoleIntensity NormalIntensity
         ]
  putStr str
  setSGR []

-- |Prompts the user with a question (and an optional hint)
-- and returns an answer
ask :: String -> Maybe String -> IO String
ask info hint = do
  colorPutStr Yellow $ "What is your " ++ info ++ "?" ++ (maybe "" (\h -> " ("++h++")") hint)
  putStr " "
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
    colorPutStr Red "What... is your favourite colour?"
    putStrLn "Blue. no, yel..."
    else do
    colorPutStr Red "What is the capital of Assyria?"
    putStrLn "I don't know that!"
  colorPutStr Cyan "[You are thrown over the edge into the volcano]"
  putStrLn $ "Auuuuuuuuuuugh " ++ str 
  colorPutStr Red "Hee hee heh."
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
  TLI.writeFile outputFileName transformedFile

-- |Safely read the ~/.gitconfig file
safeReadGitConfig :: IO TL.Text
safeReadGitConfig = do
  e <- tryJust (guard . isDoesNotExistError)
       (do home <-getEnv "HOME"
           TLI.readFile $ home </> ".gitconfig")
  return $ either (const (TL.empty)) id e

-- |From a given string (obtained from gitconfig) extract the user's
-- name and address (if they exist)
getNameAndMail :: TL.Text -> (Maybe String, Maybe String)
getNameAndMail gitConfigContent = (getFirstValueFor splitted "name",
                                   getFirstValueFor splitted "email")
  where
    -- make lines of words
    splitted :: [[TL.Text]]
    splitted = map TL.words $ TL.lines gitConfigContent

-- |Get the first line which starts with 'elem ='
-- and return the third field (value)
getFirstValueFor :: [[TL.Text]] -> String -> Maybe String
getFirstValueFor splitted key = firstJust $ map (getValueForKey key) splitted

-- |Return the first Just value of a list of Maybe
firstJust :: (Eq a) => [Maybe a] -> Maybe a
firstJust l = case dropWhile (==Nothing) l of
  [] -> Nothing
  (j:_) -> j

-- |Given a line of words ("word1":"word2":rest) getValue will return
-- rest if word1 == key 'elem =' or Nothing otherwise
getValueForKey :: String          -- key
                  -> [TL.Text]     -- line of words
                  -> Maybe String -- the value if found
getValueForKey el (n:e:xs)
  | n == (TL.pack el) && e == (TL.pack "=") = Just $ TL.unpack $ TL.unwords xs
  | otherwise = Nothing
getValueForKey _ _ = Nothing

-- |Make an HTTP request, making sure the User-Agent is set
simpleHTTPWithUserAgent :: String -> IO BL.ByteString
simpleHTTPWithUserAgent url = do
  r <- parseUrl url
  let request = r { requestHeaders = [ ("User-Agent", "HTTP-Conduite") ] }
  withManager $ (return.responseBody) <=< httpLbs request

-- | Get the Github username for a specific email
getGHUser :: Maybe String -> IO (Maybe String)
getGHUser Nothing = return Nothing
getGHUser (Just email) = do
  let url = "https://api.github.com/search/users?q=" ++ email
  body <- simpleHTTPWithUserAgent url -- JSON representation
  let login = body ^? key "items" . nth 0 . key "login"
  return $ fmap jsonValueToString login
  where
    jsonValueToString = TL.unpack . TLB.toLazyText . encodeToTextBuilder

intro :: IO ()
intro = do
  sword
  colorPutStr Yellow "\nCreating project structure...\n"

sword :: IO ()
sword = do
  putStrLn "\n"
  colorPutStr Yellow "          /*\n"
  colorPutStr Yellow "         />\n"
  colorPutStr Yellow "       @/<    --==:: PALADIN ::==--\n"
  colorPutStr Yellow "[\\\\\\\\\\\\(O):::<oooooooooooooooooooooooooooooooooooooooo-\n"
  colorPutStr Yellow "       @\\<\n"
  colorPutStr Yellow "         \\>\n"
  colorPutStr Yellow "          \\*\n"

-- |Called at the end of Paladin
end :: IO ()
end = do
  colorPutStr Yellow "--==:: Done! ::==--\n"

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
  ghuser <- getGHUser email
  in_author <- ask "name" name
  in_email <- ask "email" email
  in_ghaccount <- ask "github account" ghuser
  in_synopsis <- ask "project in less than a dozen words" Nothing
  current_year <- getCurrentYear
  createProject $ Project projectName moduleName in_author in_email in_ghaccount in_synopsis current_year

  -- Initialize git and cabal, and run tests
  _ <- system "git init ."
  _ <- system "cabal sandbox init"
  _ <- system "cabal install"
  --_ <- system "cabal test"
  --_ <- system $ "./.cabal-sandbox/bin/test-" ++ projectName
  end

  
