module Main (main) where

import System.Environment.Guard (ExpectEnv (..), guardOrElse')
import Test.DocTest qualified as DocTest
import Prelude

main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DocTest.doctest args)
    (putStrLn "*** Doctests Disabled ***")
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Data/Time/Relative.hs"
  ]

exts :: [String]
exts =
  [ "-XApplicativeDo",
    "-XDataKinds",
    "-XDeriveAnyClass",
    "-XDeriveDataTypeable",
    "-XDeriveGeneric",
    "-XDerivingStrategies",
    "-XDerivingVia",
    "-XFlexibleInstances",
    "-XGADTs",
    "-XImportQualifiedPost",
    "-XLambdaCase",
    "-XMultiParamTypeClasses",
    "-XNumericUnderscores",
    "-XOverloadedStrings",
    "-XStandaloneKindSignatures",
    "-XTypeApplications"
  ]
