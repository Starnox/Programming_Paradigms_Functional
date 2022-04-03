module Main where

import System.Environment (getArgs)
import Proof.Core
import Tasks
import Text.Printf

import qualified Refs as R
import qualified Dataset as D


task1 = ("Task 1", [
        expect (compute_average_steps D.eight_hours) toBeSorted R.task1
   ])

task2 = ("Task 2", [
        expect (get_passed_people_num D.eight_hours) toBe 97,
        expect (get_passed_people_percentage D.eight_hours) toBeF 0.73,
        expect (get_steps_avg D.eight_hours) toBeF 2328.19
   ])

task3 = ("Task 3", [
        expect (get_avg_steps_per_h D.eight_hours) toBeSorted R.task3
   ])

task4 = ("Task 4", [
        expect (get_activ_summary D.physical_activity) toBeSorted R.task4
   ])

task5 = ("Task 5", [
        expect (get_ranking D.physical_activity) toBeSorted R.task5
    ])

task6 = ("Task 6", [
        expect (get_steps_diff_table D.eight_hours) toBeSorted R.task6
    ])

task7 = ("Task 7", [
        expect (vmap (show . length) D.emails) toBeSorted R.task7
    ])

task8 = ("Task 8", [
        expect (get_sleep_total $ head $ tail $ D.sleep_min) toBeSorted R.task8
    ])

taskSets = [task1, task2, task3, task4, task5, task6, task7, task8]

main :: IO ()
main = do
    args <- getArgs
    let test_suites = if null args then taskSets
                                   else map ((taskSets !!) . (subtract 1) . (read :: String -> Int)) args

    let y = sum $ fmap (snd . fmap length) test_suites
    x <- runSuites test_suites

    putStrLn "Finished running all unit tests."

    putStrLn $ printf "%d/%d" x y
