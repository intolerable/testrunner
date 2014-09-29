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

-- | Test.Runner.Driver contains the functions that determine which tests are
--   run, with which parameters and by how many threads.
module Test.Runner.Driver ( runTests, runTestsParallel,
                            runTestsWithArgs,
                            runTestsParallelWithArgs,
                            Result(..),
                          ) where

import Data.Maybe ( isJust, isNothing, fromJust )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( newTVar, readTVar, writeTVar, atomically,
                                retry, TVar )
import Test.QuickCheck ( Args(..), stdArgs )

import Test.Runner.Backends

-- | Shows a name, runs the test, and then shows whether it failed or not by
--   printing either "OK" or "FAIL!" to standard output.
run_showing_name :: Args -> (String, TestRunnerTest) -> IO (Maybe String)
run_showing_name qcArgs (name, TestRunnerTest t) = do
    putStr (name ++ ": ")
    r <- runWithArgs qcArgs t
    putStrLn (if isNothing r then "OK" else "FAIL!")
    return r


-- | The result of the test runner mentions how many tests passed, and the names
--   and failure messages of the tests that failed.
data Result = Result { numPassed   :: Int
                     , failures    :: [(String, String)]
                     } deriving (Show, Eq, Ord)

-- | Run a list of named tests.
runTests :: [(String, TestRunnerTest)] -> IO Result
runTests = runTestsWithArgs stdArgs

-- | Run a list of named tests, using the given QuickCheck @Args@ for the
--   QuickCHeck tests.
runTestsWithArgs :: Args -> [(String, TestRunnerTest)] -> IO Result
runTestsWithArgs qcArgs namedTests = do
    results <- mapM (run_showing_name qcArgs) namedTests
    let namedResults = zip names results
        passed = length (filter isNothing results)
        failed = map sndFromJust $ filter (isJust . snd) namedResults
    return (Result passed failed)
  where (names, _) = unzip namedTests
        sndFromJust (name, result) = (name, fromJust result)

-- * Running tests in parallel

data RunnerState = RunnerState
    { testsToDo :: [(String, TestRunnerTest)]
    , passedTests :: [String]
    , failedTests :: [(String, String)] -- ^ Names and failure messages
    , numDone     :: Int
    }

-- | Creates an initial test runner state given the list of named tests
initial_runner_state :: [(String, TestRunnerTest)] -> RunnerState
initial_runner_state ts = RunnerState ts [] [] 0

-- | Uses multiple threads to run a set of unit tests.
runTestsParallel :: Int -- ^ Number of worker threads to use
                 -> [(String, TestRunnerTest)] -- ^ The tests with their names
                 -> IO Result
runTestsParallel n namedTests = runTestsParallelWithArgs n stdArgs namedTests

-- | Use multiple threads to run a set of unit tests, and run the QuickCheck
--   tests with the given QuickCheck @Args@.
runTestsParallelWithArgs :: Int  -- ^ Number of worker threads to use
                         -> Args -- ^ Arguments to QuickCheck
                         -> [(String, TestRunnerTest)] -- ^ Tests with names
                         -> IO Result
runTestsParallelWithArgs n qcArgs namedTests = do
    let numToDo = length namedTests
    stateRef <- atomically (newTVar (initial_runner_state namedTests))
    sequence_ (replicate n (forkIO (test_runner_thread qcArgs stateRef)))
    -- now wait until the threads have completed
    atomically $ do
        state <- readTVar stateRef
        if numDone state == numToDo
          then return $ Result (length (passedTests state)) (failedTests state)
          else retry

-- | The main loop of a worker thread when doing a parallel run
test_runner_thread :: Args -> TVar RunnerState -> IO ()
test_runner_thread qcArgs stateRef = do
    nextTest <- getNextTest
    case nextTest of
      Just t  -> run_one_test qcArgs stateRef t >> test_runner_thread qcArgs stateRef
      Nothing -> return () -- ready
  where getNextTest = atomically $ do
           state <- readTVar stateRef
           let tests_to_do = testsToDo state
           case tests_to_do of
             doNow:doLater -> do writeTVar stateRef (state { testsToDo = doLater })
                                 return (Just doNow)
             []            -> return Nothing

-- | Runs one test as a part of a parallel test run, and updates the global
--   state after it's done.
run_one_test :: Args -> TVar RunnerState -> (String, TestRunnerTest) -> IO ()
run_one_test qcArgs stateRef (name, TestRunnerTest t) = do
    result <- runWithArgs qcArgs t
    putStrLn (name ++ ": " ++ (if isNothing result then "OK" else "FAIL!"))
    atomically $ do
        state <- readTVar stateRef
        let ps = passedTests state
            fs = failedTests state
            state'  = state { numDone = numDone state + 1 }
            state'' = case result of
                        Nothing   -> state' { passedTests = name : ps }
                        Just msg  -> state' { failedTests = (name, msg) : fs }
        writeTVar stateRef state''

