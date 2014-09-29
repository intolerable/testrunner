-- Copyright (C) 2009 Reinier Lamers
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

-- | Test.Runner.Frontend contains the code for the prefabricated unit test
--   executable, like command-line argument parsing and handling.
module Test.Runner.Frontend ( testRunnerMain ) where

import System.IO ( hPutStrLn, stderr, hSetBuffering, stdout,
                   BufferMode ( NoBuffering ) )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), getOpt, usageInfo,
                               ArgOrder(Permute) )
import System.Environment ( getArgs )
import System.Random ( StdGen )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.Maybe ( isJust )
import Text.Regex ( mkRegex, matchRegex )
import Test.QuickCheck ( Args(..), stdArgs )
import Test.QuickCheck.Random (QCGen)

import Test.Runner.Backends ( TestRunnerTest(..) )
import Test.Runner.Driver ( runTestsParallelWithArgs, Result(..) )

-- | testRunnerMain is intended to be used as the main function of a unit test
--   program. It takes as an argument the complete list of unit tests for a
--   package.
testRunnerMain :: [(String, TestRunnerTest)] -> IO ()
testRunnerMain tests = do
  hSetBuffering stdout NoBuffering
  maybeFlags <- parse_args `fmap` getArgs
  case maybeFlags of
    Nothing -> do hPutStrLn stderr "testrunner: Unrecognized arguments on command line"
                  printUsageAndDie
    Just flags ->
        let showHelp = not (null [x | x <- flags,
                                      case x of
                                        ShowHelp -> True
                                        _        -> False])
        in if showHelp
             then printUsageAndDie
             else runWithFlags flags tests

printUsageAndDie :: IO ()
printUsageAndDie = do
    putStr (usageInfo "unit - run darcs unit tests" opts)
    exitWith ExitSuccess

-- | Runs a set of tests according to the given command-line flags. The
--   @ShowHelp@ flag is assumed to have been handled already and is ignored by
--   this function.
runWithFlags :: [UnitFlag] -> [(String, TestRunnerTest)] -> IO ()
runWithFlags flags tests = runAndShowTests numThreads qcArgs matchingTests
  where matchingTests = filter (isJust . (matchRegex matchEx) . fst) tests
        qcArgs = case replayValues of
                   Nothing           -> stdArgs
                   Just (seed, size) -> stdArgs { replay = Just (seed, size) }
        numThreads | null jobsArgs = 1
                   | otherwise     = last jobsArgs
        jobsArgs = [j | NumJobs j <- flags]
        matchEx | null matchArgs = mkRegex ".*" -- matches any string
                | otherwise      = mkRegex (last matchArgs)
        matchArgs = [ex | Matching ex <- flags]
        replayValues | null replayArgs = Nothing
                     | otherwise = Just (last replayArgs)
        replayArgs = [(seed, size) | QuickCheckReplay seed size <- flags]

-- | Run tests in a number of threads, and give a summary after all tests have
--   been run
runAndShowTests :: Int -> Args -> [(String, TestRunnerTest)] -> IO ()
runAndShowTests numThreads qcArgs tests = do
    results <- runTestsParallelWithArgs numThreads qcArgs tests
    putStr (show (numPassed results) ++ " tests passed.")
    if not (null (failures results))
      then do putStrLn " Failing tests:"
              mapM_ (putStr . formatFailure) (failures results)
      else putChar '\n'
  where formatFailure (name, output) =
            "    " ++ name ++ ":\n" ++
            ((unlines . map ("        "++) . lines) output)

-- | Data type describing command line flag
data UnitFlag = ShowHelp
              | NumJobs Int
              | Matching String
              | QuickCheckReplay QCGen Int

-- | Parse a string to UnitFlag that describes the number of jobs to run. Exits
--   in case of malformed input.
parse_numjobs :: String -> UnitFlag
parse_numjobs s = case reads s of
    [(x,"")] -> NumJobs x
    _        -> error "Invalid number of Haskell threads given"

parse_qc_replay :: String -> UnitFlag
parse_qc_replay s = QuickCheckReplay seed size
  where (seedString, sizeString) = break (==',') s
        seed = case reads seedString :: [(QCGen, String)] of
                 [(seed', "")] -> seed'
                 _             -> error "Invalid QuickCheck seed given"
        size = case sizeString of
                 []          -> error "Empty QuickCheck size given"
                 sizeString' -> case reads (tail sizeString') of
                                  [(size', "")] -> size'
                                  _             -> error "Invalid QuickCheck size given"

-- | List of possible command line options
opts :: [OptDescr UnitFlag]
opts = [Option ['j'] ["jobs"] (ReqArg parse_numjobs "NUM") "Number of Haskell threads to run unit tests (you need +RTS -N<NUM> too)"
       ,Option ['m'] ["matching"] (ReqArg Matching "REGEX") "Run only tests matching the given POSIX regular expression"
       ,Option ['r'] ["quickcheck-replay"] (ReqArg parse_qc_replay "SEED,SIZE") "Run QuickCheck tests once with given seed and size"
       ,Option ['h'] ["help"] (NoArg ShowHelp)         "Show usage information and exit"
       ]

-- | Parses the command line options with @getOpt@ and returns @Nothing@ in the
--   case of an invalid command line, or the parsed options otherwise.
parse_args :: [String]         -- ^ The list of command line args as from
                               --   @getArgs@
           -> Maybe [UnitFlag] -- ^ List of options represented by @UnitFlag@
                               --   values
parse_args args | not (null nonopts)
                  || not (null errs) = Nothing
                | otherwise          = Just vals
  where         (vals, nonopts, errs) = getOpt Permute opts args

