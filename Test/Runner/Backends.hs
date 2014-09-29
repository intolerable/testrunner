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
{-# LANGUAGE GADTs, FlexibleInstances #-}

-- | Test.Runner.Backends contains the types and functions that make it possible
--   to run tests constructed with different test packages with the same driver
--   framework from Test.Runner.Driver.
module Test.Runner.Backends ( TestRunnerTest(..), RunnableTest(..),
                              RunWithQuickCheck(..), runWithQuickCheck
                            ) where

import Data.List ( intersperse )
import qualified Test.QuickCheck as QC ( Testable(..), quickCheckWithResult,
                                         Result(..),
                                         Args, stdArgs )
import Test.HUnit ( Test, PutText(..), runTestText, errors, failures )

-- | The class of all types that testrunner can treat as a test. The method
--   'run' should return @Nothing@ if the test succeeds, or @Just s@ if the test
--   fails, where @s@ is a human-readable description of the failure.
class RunnableTest a where
    run :: a -> IO (Maybe String)
    -- | 'runWithArgs' runs the test with specified QuickCheck arguments. For
    --   all non-QuickCheck tests, this defaults to just @run@.
    runWithArgs :: QC.Args -> a -> IO (Maybe String)
    runWithArgs = const run

-- | Any expression that returns @True@ upon success and @False@ upon failure
--   can be treated as a test by testrunner.
instance RunnableTest Bool where
    run e = return $ if e then Nothing else Just "Boolean test failed"

-- | Any @IO@ action that returns @True@ upon success and @False@ upon failure
--   can be treated as a test by testrunner.
instance RunnableTest (IO Bool) where
    run a = a >>= run

-- | 'RunWithQuickCheck' turns a QuickCheck test into something that can be run
--   with testrunner. This type-level indirection is necessary to please the
--   type checker.
data RunWithQuickCheck where
    RunWithQuickCheck :: QC.Testable a => a -> RunWithQuickCheck

-- | QuickCheck properties can be run by testrunner.
--   You do lose a lot of information on the result though; only whether the
--   test succeeded or not is returned.
instance RunnableTest RunWithQuickCheck where
    run t = runWithArgs QC.stdArgs t
    runWithArgs args (RunWithQuickCheck t) = do
      r <- QC.quickCheckWithResult args t
      return $ case r of
                 QC.Failure{} -> Just (QC.reason r ++ " (seed: " ++
                                       show (QC.usedSeed r) ++ ", size: "
                                       ++ show (QC.usedSize r) ++ ")")
                 _           -> Nothing

-- | HUnit @Test@s can be run by testrunner.
instance RunnableTest Test where
    run t = do
        (counts, messages) <- runTestText recordMessage t
        return $ if errors counts == 0 && failures counts == 0
                   then Nothing
                   else Just (concat $ reverse $ intersperse "\n" messages)
      where recordMessage = PutText (\msg _ msgs -> return (msg : msgs)) []

-- | A TestRunnerTest is a data type that hides the actual type of the test -
--   QuickCheck, plain IO action, or any other RunnableTest. This is required to
--   be able to put tests of different types in a single list.
data TestRunnerTest where
    TestRunnerTest :: (RunnableTest a) => a -> TestRunnerTest

-- | Convenience function to go from something testable by QuickCheck to a
--   @TestRunnerTest@ in one step.
runWithQuickCheck :: (QC.Testable a) => a -> TestRunnerTest
runWithQuickCheck = TestRunnerTest . RunWithQuickCheck

