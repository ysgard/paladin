{-
Module: PaladinTemplates
Description: This module contains template strings for use with hastache
Copyright: (c) Jan Van Uytven, 2015
License: MIT
Maintainer : ysgard@gmail.com
Stability: experimental
Portability: POSIX

Probably could just implement this as functions that take a Project
data structure and returns a rendered string, but meh... already
using hastache from Holy-Haskell-Starter.
-}
module Paladin.Raw where

import Data.List

licenseRaw :: String
licenseRaw = intercalate "\n" [
  "Copyright (c) {{year}} {{author}}"
  , ""
  , "Permission is hereby granted, free of charge, to any person obtaining"
  , "a copy of this software and associated documentation files (the"
  , "\"Software\"), to deal in the Software without restriction, including"
  , "without limitation the rights to use, copy, modify, merge, publish,"
  , "distribute, sublicense, and/or sell copies of the Software, and to"
  , "permit persons to whom the Software is furnished to do so, subject to"
  , "the following conditions:"
  , ""
  , "The above copyright notice and this permission notice shall be included"
  , "in all copies or substantial portions of the Software."
  , ""
  , "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,"
  , "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF"
  , "MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT."
  , "IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY"
  , "CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,"
  , "TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE"
  , "SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."
  ]

gitignoreRaw :: String
gitignoreRaw = intercalate "\n" [
  ".cabal-sandbox/"
  , "cabal.sandbox.config"
  , "dist/"
  ]

projectcabalRaw :: String
projectcabalRaw = intercalate "\n" [
  "name: {{projectName}}"
  , "version: 0.1.0.0"
  , "synopsis: {{synopsis}}"
  , "-- Description"
  , "license: MIT"
  , "license-file: LICENSE"
  , "author: {{author}}"
  , "maintainer: {{mail}}"
  , "-- Copyright"
  , "category: Development"
  , "build-type: Simple"
  , "-- extra-source-files"
  , "-- data-files"
  , "cabal-version: >=1.10"
  , "executable {{projectName}}"
  , "  main-is: Main.hs"
  , "  --other-modules"
  , "  --other-extensions"
  , "  build-depends: base >=4.6"
  , "  hs-source-dirs: src"
  , "  ghc-options: -Wall"
  , "  default-language: Haskell2010"
  ]

mainRaw :: String
mainRaw = intercalate "\n" [
  "{-|"
  , "Module : {{moduleName}}"
  , "Description: {{synopsis}}"
  , "Copyright: (c) {{author}}, {{year}}"
  , "License: MIT"
  , "Maintainer : {{mail}}"
  , "Stability: experimental"
  , "Portability: POSIX"
  , ""
  , "-}"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  putStrLn \"You fight with the strength of many men, sir Knight...\"--" 
  , ""
  ]

setupRaw :: String
setupRaw = intercalate "\n" [
  "import Distribution.Simple"
  , "main = defaultMain"
  ]

