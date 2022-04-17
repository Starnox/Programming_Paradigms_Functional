-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


-- ==================================================

module Tasks where

import Dataset
import Data.List
import Data.Maybe (fromJust)
import Text.Printf
import Data.Array (Ix (index))
import Text.Read (Lexeme(String), readMaybe)

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String


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

-- get the total sum of steps from the eight hours table
get_number_of_steps_row :: Row -> Float
get_number_of_steps_row row = sum (map read (tail row))

-- aux function that takes a row and returns another row with the name and the
-- average steps
transform_row_task1 :: Row -> Row
transform_row_task1 current_row = [head current_row,  get_steps current_row] where
    get_steps current_row = printf "%.2f" ( get_number_of_steps_row current_row / 8)

-- add the header then use the aux function to get the average for each row
compute_average_steps :: Table -> Table
compute_average_steps m =  ["Name", "Average Number of Steps"] : map transform_row_task1 (tail m)


-- Task 2

-- Number of people who have achieved their goal:
-- made using a fold the goes through each row and does the verification
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

-- swaping rows with columns
transpose_matrix :: Table -> Table
transpose_matrix ([]:_) = []
transpose_matrix m = map head m:transpose_matrix (map tail m)

-- aux function
get_average :: Float -> Float -> Float
get_average x y = x / y

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

-- calculate the number of people for a specific range
get_num_of_people_for_range :: Row -> Int -> Int -> Int
get_num_of_people_for_range row left right = foldr (\time acc -> if read time >= left && read time < right then acc + 1 else acc) 0 (tail row)

-- create a row that contains an entry for each time interval
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
get_tot_steps :: Row -> Int
get_tot_steps = read . head . tail

-- sort the rows by number of total steps
get_sorted_rankings :: Table -> Table
get_sorted_rankings = sortBy myCmp where
    myCmp row1 row2
        |   get_tot_steps row1 > get_tot_steps row2 = GT
        |   get_tot_steps row1 == get_tot_steps row2 = EQ
        |   otherwise = LT

-- get only the relevant info from the table
transform_row_total_steps :: Table -> Table
transform_row_total_steps = map (\row -> [head row , show (get_tot_steps row)])

-- add the header and then add the calculated rows
get_ranking :: Table -> Table
get_ranking m = ["Name", "Total Steps"] :  transform_row_total_steps (get_sorted_rankings (sort (tail m)))


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
get_sum_second_part row = get_number_of_steps_row row - get_sum_first_part 0 (tail row)

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
        |   get_diff row1 == get_diff row2 = compare (head row1) (head row2) -- compare alphabetically
        |   otherwise = LT

-- applies the transformation to each row
get_steps_diff_table_aux :: Table -> Table
get_steps_diff_table_aux = map (\row -> [head row, printf "%.2f" (get_average_first_part row),printf "%.2f" (get_average_second_part row), printf "%.2f" (get_diff row)])


-- put the header then append the rest of the transformed table
get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : get_steps_diff_table_aux (get_sorted_diff_table (tail m))


-- Task 7

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

{-
    TASK SET 2
-}

-- Task 1

-- get the index of the column with the given name
get_column :: ColumnName -> Row -> Maybe Int
get_column = elemIndex

-- get the element in the correct row to be compared (it returns a string)
extract_el :: Row -> Maybe Int -> Value
extract_el row Nothing = ""
extract_el row (Just x) = row !! x

-- when we know we can extract a float from the cell specified
get_float_value :: Row -> Maybe Int -> Float
get_float_value row relevantCol = read (extract_el row relevantCol) :: Float

-- first of all tries to convert a value from the column to a float
-- if it succedes then compare as floats, otherwise compare as string with compare
tsort :: ColumnName -> Table -> Table
tsort column table = head table : mySort (readMaybe (extract_el (table !! 1) (get_column column (head table))) :: Maybe Float) where
    mySort Nothing = sortBy my_comp1 (tail table) where
        my_comp1 row1 row2 = compare (extract_el row1 relevantCol) (extract_el row2 relevantCol) where
                relevantCol = get_column column (head table)
    mySort (Just x) = sortBy my_comp2 (tail table) where
         my_comp2 row1 row2
            | get_float_value row1 relevantCol > get_float_value row2 relevantCol = GT
            | get_float_value row1 relevantCol == get_float_value row2 relevantCol = compare (head row1) (head row2)
            | otherwise = LT where
                relevantCol = get_column column (head table)

-- Task 2

-- verify if the headers are the same
check :: Row -> Row ->Bool
check r1 r2 = length r1 == length r2 && checkEQ where
    checkEQ = and (zipWith (==) r1 r2)

-- if the headers are the same then concat, otherwise just return the first table
vunion :: Table -> Table -> Table
vunion t1 t2 = if check (head t1) (head t2) then t1 ++ tail t2 else t1

-- Task 3
-- gets a row with as many empty strings as the number of columns
num_empty :: Row -> Row
num_empty = map (const "")

-- zip together the rows of the first table with the rows of the second one
-- haskell is "clever" enough to pad only when needed when using the infinite
-- function repeat
hunion :: Table -> Table -> Table
hunion t1 t2 = zipWith (++) t1 (t2 ++ repeat (num_empty (head t2)))

-- Task 4

unify :: Row -> Row -> Row
unify r1 r2 = nub (r1 ++ r2)


{-
- unite the two headers into one with no duplicates using unify
- use a map to go through every row of table 1 and apply the function -> map_function
- map_function looks at the row and tries to find using the key (from key_column) the
- correspondent row in table 2 with the function find_by_key.
- if it doesn't find a correspondent then it puts an empty list [] in place of that row
- otherwise it appends the two rows together and delete the duplicate columns using delete_duplicate
- Last of all, we apply a filter to the final table in order to remove the empty lists that we left there
- first index is the index of the key column from the first table
- last index is the index of the key column from the second table
-}
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = unify (head t1) (head t2) :
    filter (not . null) (map map_function (tail t1)) where
        first_index = head (extract_list_of_indices [key_column] (head t1 ++ head t2) 0)
        last_index = last (extract_list_of_indices [key_column] (head t1 ++ head t2) 0)
        index_in_t2 c_key = findIndex (\row -> head row == c_key) t2
        find_by_key Nothing = []
        find_by_key (Just index) = t2 !! index
        map_function row = if null (find_by_key (index_in_t2 (row !! first_index)))
             then [] else delete_duplicate (row ++ find_by_key (index_in_t2 (row !! first_index)))
        delete_duplicate row = take last_index row ++ drop (last_index+1) row

-- Task 5

-- add the new header column afterwards use list comprehension to get
-- every row from t1 with every row from 2 and apply the function to the two rows
cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 =
    new_column_names : [new_row_function row1 row2 | row1 <- tail t1, row2 <- tail t2 ]

-- Task 6

-- get a list of indexes for the columns that are specified
extract_list_of_indices :: [ColumnName] -> Row -> Int -> [Int]
extract_list_of_indices columns_to_extract [] _ = []
extract_list_of_indices columns_to_extract (x:xs) cnt = if x `elem` columns_to_extract then
    cnt : extract_list_of_indices columns_to_extract xs (cnt+1) else extract_list_of_indices columns_to_extract xs (cnt+1)

-- for each row in the table extract the info only at the indexes obtained
-- with the function from above
-- written also with list comprehension -> for every row create a new list
-- with the elements in the columns specified by indexes
projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = map (\row -> [row !! index | index <- indexes]) t where
    indexes = extract_list_of_indices columns_to_extract (head t ) 0

-- Task 7

{-
- Put the header
- Use the filter function which uses the condition on the extracted value to compare from each row
- get_relevant_value gets the correct value from the row like this row[index]
- where index is obtained by calling the function made previously `extract_list_of_indexe`
- with the key_column used as a list -> and we use head on the result as we know it will only return
- a single index
-}
filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = head t : filter (condition . get_relevant_value key_column) (tail t) where
    get_relevant_value key_column row = row !! head (extract_list_of_indices [key_column] (head t) 0)


