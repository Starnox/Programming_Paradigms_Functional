-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array ()
import Text.Read (Lexeme(String))

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

get_number_of_steps_row :: Row -> Float
get_number_of_steps_row row = sum (map read (tail row))

transform_row_task1 :: Row -> Row
transform_row_task1 current_row = [head current_row,  get_steps current_row] where
    get_steps current_row = printf "%.2f" ( get_number_of_steps_row current_row / 8)

compute_average_steps :: Table -> Table
compute_average_steps m =  ["Name", "Average Number of Steps"] : map transform_row_task1 (tail m)


-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = foldl (\acc el -> if get_number_of_steps_row el >= 1000
    then acc + 1 else acc) 0 (tail m)



-- Get the total number of people
get_num_people :: Table -> Int
get_num_people m = length m - 1

-- Percentage of people who have achieved their:
-- The number of people that achived their goal / number of people
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = fromIntegral (get_passed_people_num m) / fromIntegral (get_num_people m)


-- Average number of daily steps

-- The average of the number of daily steps from all people
get_steps_avg :: Table -> Float
get_steps_avg m = sum (map get_number_of_steps_row (tail m)) / fromIntegral (get_num_people m)



-- Task 3


transpose_matrix :: [[Value]] -> [[Value]]
transpose_matrix ([]:_) = []
transpose_matrix m = map head m:transpose_matrix (map tail m)

--get_column :: Table -> Row
get_average :: Float -> Float -> Float 
get_average x y = x / y

--get_average_steps :: Float -> Float 

create_row_of_avg :: Table -> Row
create_row_of_avg x = ["dsa"]

{-
foldr (\row acc->
    print (get_number_of_steps_row row): acc) [] (tail (transpose_matrix m))
-}

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"]
    : map (show . get_number_of_steps_row) tail(transpose_matrix (tail m))


-- Task 4

get_activ_summary :: Table -> Table
get_activ_summary m = undefined


-- Task 5

get_ranking :: Table -> Table
get_ranking m = undefined


-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = undefined


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = undefined


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = undefined


get_sleep_total :: Row -> Row
get_sleep_total r = undefined
