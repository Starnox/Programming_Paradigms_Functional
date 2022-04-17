module Main where

import System.Environment (getArgs)
import Proof.Core
import Tasks
import Text.Printf
import qualified Data.Map as M

import qualified Refs as R
import qualified Dataset as D


task1_1 = ("Task 1", [
        expect (compute_average_steps D.eight_hours) toBeSorted R.task1_1
   ])

task1_2 = ("Task 2", [
        expect (get_passed_people_num D.eight_hours) toBe 97,
        expect (get_passed_people_percentage D.eight_hours) toBeF 0.73,
        expect (get_steps_avg D.eight_hours) toBeF 2328.19
   ])

task1_3 = ("Task 3", [
        expect (get_avg_steps_per_h D.eight_hours) toBeSorted R.task1_3
   ])

task1_4 = ("Task 4", [
        expect (get_activ_summary D.physical_activity) toBeSorted R.task1_4
   ])

task1_5 = ("Task 5", [
        expect (get_ranking D.physical_activity) toBeSorted R.task1_5
    ])

task1_6 = ("Task 6", [
        expect (get_steps_diff_table D.eight_hours) toBeSorted R.task1_6
    ])

task1_7 = ("Task 7", [
        expect (vmap (show . length) D.emails) toBeSorted R.task1_7
    ])

task1_8 = ("Task 8", [
        expect (get_sleep_total $ head $ tail $ D.sleep_min) toBeSorted R.task1_8
    ])

task2_1 = ("Task 2.1", [
        expect (tsort "TotalSteps" D.physical_activity) toBe R.task2_1
    ])

task2_2 = ("Task 2.2", [
        expect (vunion (take 100 D.physical_activity) ((head D.physical_activity):(drop 100 D.physical_activity))) toBe D.physical_activity
    ])

task2_3 = ("Task 2.3", [
        expect (hunion D.physical_activity D.eight_hours) toBe R.task2_3
    ])

task2_4 = ("Task 2.4", [
        expect (tjoin "Name" D.physical_activity D.eight_hours) toBeSorted R.task2_4
    ])

task2_5 = ("Task 2.5", [
        expect (cartesian (++) (head D.physical_activity ++ head D.eight_hours) (take 15 D.physical_activity) (take 20 D.eight_hours)) toBeSorted R.task2_5
    ])

task2_6 = ("Task 2.6", [
        expect (projection (head D.physical_activity) D.physical_activity) toBe D.physical_activity,
        expect (projection ["Name"] D.physical_activity) toBe R.task2_6
    ])

task2_7 = ("Task 2.7", [
        expect (filterTable ((>10000).read) "TotalSteps" D.physical_activity) toBe R.task2_7
    ])

taskSets2 = M.fromList [
        ("1.1", task1_1),
        ("1.2", task1_2),
        ("1.3", task1_3),
        ("1.4", task1_4),
        ("1.5", task1_5),
        ("1.6", task1_6),
        ("1.7", task1_7),
        ("1.8", task1_8),

        ("2.1", task2_1),
        ("2.2", task2_2),
        ("2.3", task2_3),
        ("2.4", task2_4),
        ("2.5", task2_5),
        ("2.6", task2_6),
        ("2.7", task2_7)
    ]

taskSets = [
        task1_1, task1_2, task1_3, task1_4, task1_5, task1_6, task1_7, task1_8,
        task2_1, task2_2, task2_3, task2_4, task2_5, task2_6, task2_7
    ]

main :: IO ()
main = do
    args <- getArgs
    let test_suites = if null args then map snd $ M.toList taskSets2
                                   else map (\x -> M.findWithDefault task1_1 x taskSets2) args

    let y = sum $ fmap (snd . fmap length) test_suites
    x <- runSuites test_suites

    putStrLn "Finished running all unit tests."

    putStrLn $ printf "%d/%d" x y
