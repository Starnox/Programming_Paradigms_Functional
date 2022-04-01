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
get_num_people :: Table -> Float
get_num_people m = fromIntegral (length m - 1)

-- Percentage of people who have achieved their:
-- The number of people that achived their goal / number of people
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = fromIntegral (get_passed_people_num m) / get_num_people m


-- Average number of daily steps

-- The average of the number of daily steps from all people
get_steps_avg :: Table -> Float
get_steps_avg m = sum (map get_number_of_steps_row (tail m)) / get_num_people m



-- Task 3



transpose_matrix :: Table -> Table
transpose_matrix ([]:_) = []
transpose_matrix m = map head m:transpose_matrix (map tail m)

get_average :: Float -> Float -> Float
get_average x y = x / y

{-
foldr (\row acc->
    print (get_number_of_steps_row row): acc) [] (tail (transpose_matrix m))
-}

-- transforms a vector of floats to a vector of strings that contain the averages
get_avg_steps_aux :: [Float] -> Float  -> Row
get_avg_steps_aux matrix x = map (printf "%.2f" . (`get_average` x)) matrix

-- adds the header row then uses th aux func to add the averages for each hour
-- we transpose the matrix in order to get each column
get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = [["H10","H11","H12","H13","H14","H15","H16","H17"],
     get_avg_steps_aux (map get_number_of_steps_row (tail (transpose_matrix m))) (get_num_people m)]


-- Task 4

{-
Steps:
-extract the columns for very/fairly/lightly active minutes and transoform them into rows
-for each category calculate the number of people and then add them to the table
-}

-- Extracts the columns relevant to the exercise
extract_relevant_columns :: Table -> Table
extract_relevant_columns = tail . tail . tail

get_num_of_people_for_range :: Row -> Int -> Int -> Int
get_num_of_people_for_range row left right = foldr (\time acc -> if read time >= left && read time < right then acc + 1 else acc) 0 (tail row)

get_row_of_ranges :: Row -> Row
get_row_of_ranges row = [show (get_num_of_people_for_range row 0 50), show (get_num_of_people_for_range row 50 100), show (get_num_of_people_for_range row 100 500)]

-- gets the column for each type of minute, counts the number of people and 
-- returns a table 
get_activ_summary_aux :: Table -> Table
get_activ_summary_aux = map (\row -> head row : get_row_of_ranges row)

get_activ_summary :: Table -> Table
get_activ_summary m = ["column", "range1", "range2", "range3"] : get_activ_summary_aux (extract_relevant_columns . transpose_matrix $ m )


-- Task 5

{-
Idea: create the header row then use a use a function that takes the table
and sorts it by getting the total number of steps

-}

-- get the number of steps for a particular person
get_total_num_of_steps :: Row -> Float
get_total_num_of_steps = sum . map read . tail


-- sort the rows by number of total steps
get_sorted_rankings :: Table -> Table
get_sorted_rankings = sortBy myCmp where
    myCmp row1 row2
        |   get_total_num_of_steps row1 > get_total_num_of_steps row2 = GT
        |   get_total_num_of_steps row1 == get_total_num_of_steps row2 = EQ
        |   otherwise = LT

get_ranking :: Table -> Table
get_ranking m = ["Name", "Total Steps"] : get_sorted_rankings (tail m)


-- Task 6
{-
Idea:
- Create the header
- Have a function that sort the table by using the diff between the averages
- Have a function that computes the average for the first part
- Have a function that computes the average for the second part
- Have a function that computes the diff for a row
-}

{-
Could have used indexed but wanted to write something that scales better
-}
get_sum_first_part :: Int -> Row -> Float
get_sum_first_part 4 _ = 0.0
get_sum_first_part counter [] = 0.0
get_sum_first_part counter (x:xs) = read x + get_sum_first_part (counter + 1) xs

get_sum_second_part :: Row -> Float
get_sum_second_part row = get_total_num_of_steps row - get_sum_first_part 0 (tail row)

get_average_first_part :: Row -> Float
get_average_first_part row = get_average (get_sum_first_part 0 $ tail row) 4.0

get_average_second_part :: Row -> Float
get_average_second_part row = get_average (get_sum_second_part row) 4.0

get_diff :: Row -> Float
get_diff row = abs (get_average_first_part row - get_average_second_part row)

-- sorts the table
get_sorted_diff_table :: Table -> Table
get_sorted_diff_table = sortBy myCmp where
    myCmp row1 row2
        |   get_diff row1 > get_diff row2 = GT
        |   get_diff row1 == get_diff row2 = EQ
        |   otherwise = LT

-- applies the transformation to each row
get_steps_diff_table_aux :: Table -> Table
get_steps_diff_table_aux = map (\row -> [head row, printf "%.2f" (get_average_first_part row),printf "%.2f" (get_average_second_part row), printf "%.2f" (get_diff row)])


-- put the header then append the rest of the transformed table
-- before we sort by difference we sort by name (alphabetical) to get the correct order
get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : get_steps_diff_table_aux (get_sorted_diff_table (sort (tail m)))


-- Task 7

{-
mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr ((:) . f) []
-}

testFunc :: Value -> Value
testFunc v = v ++ "test"

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f = map (map f)

-- Task 8
testFunc2 :: Row -> Row
testFunc2 = map (++ "test")

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m =  s : tail (map f m)

-- add the name then calculate the sum with a fold and transform the output to a string with specified format
get_sleep_total :: Row -> Row
get_sleep_total r = [head r, printf "%.02f" (foldr (\el acc -> (read el :: Float) + acc) 0.0 (tail r))]
