-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Data.Maybe (fromJust, isNothing)
import Text.Printf
import Data.Array (Ix (index), array, (!), listArray)
import Text.Read (Lexeme(String), readMaybe)

import Common
import qualified Dataset as D

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


{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5


instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
    eval :: a -> QResult

evalFromTable :: Table -> QResult
evalFromTable = Table

-- transpose the matrix, get the corresponding row using a combination of filter and head
-- then apply tail on the result
evalAsList :: String -> Table -> QResult
evalAsList str table = List (tail $ head ( filter (\row -> head row == str) (transpose_matrix table)))

-- use the sort function defined in part 2
evalSort :: String -> Table -> QResult
evalSort col table = Table (tsort col table)

-- use the vmap function defined in part 1
evalValueMap :: (Value -> Value) -> Table -> QResult
evalValueMap func table = Table (vmap func table)

-- use the rmap function defined in part 1
evalRowMap :: (Row -> Row) -> [String] -> Table -> QResult
evalRowMap func hdr table = Table (rmap func hdr table)

-- use the vunion function defined in part 2
evalVUnion :: Table -> Table -> QResult
evalVUnion t1 t2 = Table (vunion t1 t2)

-- use the hunion function defined in part 2
evalHUnion :: Table -> Table -> QResult
evalHUnion t1 t2 = Table (hunion t1 t2)

-- use the tjoin function defined in part 2
evalTableJoin :: String -> Table -> Table -> QResult
evalTableJoin col t1 t2 = Table (tjoin col t1 t2)

-- use the cartesian function defined in part 2
evalCartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> QResult
evalCartesian func hdr t1 t2 = Table (cartesian func hdr t1 t2)

-- use the projection function defined in part 2
evalProjection :: [String] -> Table -> QResult
evalProjection cols table = Table (projection cols table)

-- apply the filter obtained from feval to the table
evalFilter :: FEval a => FilterCondition a -> Table -> QResult
evalFilter cond table = Table (head table : filter filter_function (tail table)) where
    filter_function = feval (head table) cond

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

-- 3.4 implement eval for graph queries

getRowForGraph :: EdgeOp -> Row -> Row -> Row
getRowForGraph edgeop row1 row2 = if isNothing value then [] else finalRow where
    value = edgeop row1 row2
    finalRow = if head row1 < head row2 then [head row1, head row2, fromJust value] else [head row2, head row1, fromJust value]

createGroups :: EdgeOp -> Table -> Table
createGroups _ [] = []
createGroups edgeop (row1:xs) = map (getRowForGraph edgeop row1) xs ++ createGroups edgeop xs

evalGraph :: EdgeOp -> Table -> QResult
evalGraph edgeop table = Table (["From", "To", "Value"] : filter (not . null) (createGroups edgeop (tail table)))

-- enroll Query in Eval class
-- Each query except FromTable expects to recieve another table 
-- and if it doesn't it returns an empty list
transform :: QResult  -> Query
transform (Table t) = FromTable t
transform _ = FromTable []

{-
If it is a query on a table then execute that query
Otherwise execute the rightmost query first then the current query
-}
instance Eval Query where
    eval (FromTable table) = evalFromTable table

    eval (AsList str (FromTable table)) = evalAsList str table
    eval (AsList str (AsList str2 q) ) = List []
    eval (AsList str query) = eval (AsList str (transform (eval query)))

    eval (Sort str (FromTable table)) = evalSort str table
    eval (Sort str (AsList str2 q) ) = List []
    eval (Sort str query) = eval (Sort str (transform (eval query)))

    eval (ValueMap func (FromTable table)) = evalValueMap func table
    eval (ValueMap func (AsList str2 q) ) = List []
    eval (ValueMap func query) = eval (ValueMap func (transform (eval query)))

    eval (RowMap func cols (FromTable table)) = evalRowMap func cols table
    eval (RowMap func cols (AsList str2 q) ) = List []
    eval (RowMap func cols query) = eval (RowMap func cols (transform (eval query)))

    eval (VUnion (FromTable table1) (FromTable table2)) = evalVUnion table1 table2
    eval (VUnion q1 q2) = eval (VUnion (transform (eval q1))  (transform (eval q2)))

    eval (HUnion (FromTable table1) (FromTable table2)) = evalHUnion table1 table2
    eval (HUnion q1 q2) = eval (HUnion (transform (eval q1))  (transform (eval q2)))

    eval (TableJoin str (FromTable table1) (FromTable table2)) = evalTableJoin str table1 table2
    eval (TableJoin str q1 q2) = eval (TableJoin str (transform (eval q1))  (transform (eval q2)))

    eval (Cartesian op cols (FromTable table1) (FromTable table2)) = evalCartesian op cols table1 table2
    eval (Cartesian op cols q1 q2) = eval (Cartesian op cols (transform (eval q1))  (transform (eval q2)))

    eval (Projection cols (FromTable table)) = evalProjection cols table
    eval (Projection cols query) = eval (Projection cols (transform (eval query)))

    eval (Filter cond (FromTable table)) = evalFilter cond table
    eval (Filter cond query) = eval (Filter cond (transform (eval query)))

    eval (Graph edgeop (FromTable table)) = evalGraph edgeop table
    eval (Graph edgeop query) = eval(Graph edge_op (transform (eval query)))

-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

-- extract the element in the correct position from the row 
-- and create a delta function that verify if it is equal to val
fevalEqFloat :: [String] -> String -> Float -> FilterOp
fevalEqFloat header str val = \row -> read (extract_el row index) == val where
        index = get_column str header

fevalEqString :: [String] -> String -> String -> FilterOp
fevalEqString header str val = \row -> extract_el row index == val where
        index = get_column str header

fevalGtFloat :: [String] -> String -> Float -> FilterOp
fevalGtFloat header str val = \row -> read (extract_el row index) > val where
        index = get_column str header

fevalGtString :: [String] -> String -> String -> FilterOp
fevalGtString header str val = \row -> extract_el row index > val where
        index = get_column str header

fevalLtFloat :: [String] -> String -> Float -> FilterOp
fevalLtFloat header str val = \row -> read (extract_el row index) < val where
        index = get_column str header

fevalLtString :: [String] -> String -> String -> FilterOp
fevalLtString header str val = \row -> extract_el row index < val where
        index = get_column str header

fevalInFloat :: [String] -> String -> [Float] -> FilterOp
fevalInFloat header str lst = \row -> read (extract_el row index) `elem` lst where
        index = get_column str header

fevalInString :: [String] -> String -> [String] -> FilterOp
fevalInString header str lst = \row -> extract_el row index `elem` lst where
        index = get_column str header

fevalFNot :: FEval a => [String] -> FilterCondition a -> FilterOp
fevalFNot header condition = not . initial where
    initial = feval header condition

fevalFieldEqFloat :: [String] -> String -> String -> FilterOp
fevalFieldEqFloat header s1 s2 = \row -> (read (extract_el row index1) :: Float) == read (extract_el row index2) where
        index1 = get_column s1 header
        index2 = get_column s2 header

fevalFieldEqString :: [String] -> String -> String -> FilterOp
fevalFieldEqString header s1 s2 = \row -> extract_el row index1 == extract_el row index2 where
        index1 = get_column s1 header
        index2 = get_column s2 header

instance FEval Float where
    feval header (Eq str val) = fevalEqFloat header str val
    feval header (Lt str val) = fevalLtFloat header str val
    feval header (Gt str val) = fevalGtFloat header str val
    feval header (In str xs) = fevalInFloat header str xs
    feval header (FNot condition) = fevalFNot header condition
    feval header (FieldEq s1 s2) = fevalFieldEqFloat header s1 s2

instance FEval String where
    feval header (Eq str val) = fevalEqString header str val
    feval header (Lt str val) = fevalLtString header str val
    feval header (Gt str val) = fevalGtString header str val
    feval header (In str xs) = fevalInString header str xs
    feval header (FNot condition) = fevalFNot header condition
    feval header (FieldEq s1 s2) = fevalFieldEqString header s1 s2

-- edge_op3 [_,_,z] [_,_,c]
--    | z == c = Just c
--    | otherwise = Nothing

-- TODO modify
-- my_edge_op :: EdgeOp
-- my_edge_op (e1:a1:a2:a3:a4:a5:a6:a7:_) (e2:b1:b2:b3:b4:b5:b6:b7:_)
--     | e1 /= e2 = Just (read (fromEnum (a1 == b1) + fromEnum  (a2 == b2) + fromEnum (a3 == b3)
--     + fromEnum (a4 == b4) + fromEnum (a5 == b5) + fromEnum (a6 == b6) + fromEnum (a7 == b7)))
--     | otherwise = Nothing 


edge_op :: EdgeOp
edge_op (e1:a1:a2:a3:a4:a5:a6:a7:a8) (e2:b1:b2:b3:b4:b5:b6:b7:b8)
    | e1 /= e2 = Just (show (fromEnum  (a1 == b1) + fromEnum  (a2 == b2) + fromEnum (a3 == b3)
        + fromEnum (a4 == b4) + fromEnum (a5 == b5) + fromEnum (a6 == b6) + fromEnum (a7 == b7)
        + fromEnum (a8 == b8)) )
    | otherwise = Nothing


myFilter = Filter (Gt "Value" (read "4" :: Float))


-- Sort "Value" $ myFilter $
-- 3.5
similarities_query :: Query
similarities_query = Sort "Value" $ myFilter (Graph edge_op (FromTable D.eight_hours ))

-- 3.6 (Typos)

-- I will calculate the distance beetween strings as levenshtein distance
-- The code was inspired from this website: https://swizec.com/blog/levenshtein-distance-in-haskell/

-- levenshtein with memoisation using arrays
levenshtein :: (Eq a) => [a] -> [a] -> Int
levenshtein xs ys = levMemo ! (n, m)
  where levMemo = array ((0,0),(n,m)) [((i,j),lev i j) | i <- [0..n], j <- [0..m]] -- create the matrix
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        lev 0 v = v
        lev u 0 = u
        lev u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),
                                            levMemo ! (u-1, v-1)]

{-
-   extract the necessary column from the table with typos and the reference table (let's call these T and Ref);
-   filter out only the values from T and Ref which don't have a perfect match in the other table - these are the problematic entries (this will help improve time performance);
-   calculate the distance between each value from T and each value from Ref (distance = how similar the 2 strings are - you decide how to formally define this distance);
-   for every value from T, its correct form is the value from Ref with the shortest distance to it;
-   lastly, restore the original table, replacing the incorrect values from T with the correct values from Ref.
-}
fromQResultToTable :: QResult -> Table
fromQResultToTable (Table t) = t
fromQResultToTable _ = []

fromQResultToList :: QResult -> [String]
fromQResultToList (List l) = l
fromQResultToList _ = []


-- Filter the words that have typos for efficency use Filter queries
filterOutMatchesAux :: Table -> String -> [String] -> Table
filterOutMatchesAux table col listOfElements = fromQResultToTable (eval $ Filter (FNot $ In col listOfElements) (FromTable table) )

filterOutMatches :: String -> Table -> Table -> Table
filterOutMatches col t1 t2 = filterOutMatchesAux t1 col (fromQResultToList listOfElements) where
    listOfElements = eval $ AsList col (FromTable t2)


-- calculate the distance from word to every word in the table in the specified column
distanceToEachWord :: String -> String -> Table -> [(String, Int)]
distanceToEachWord word col table = map
  (\ row
     -> (extract_el row index, levenshtein word (extract_el row index))) (tail table) where
    index = get_column col (head table)

-- from the list of tuples with names and distances extract the word with the minimum distance
extractNameWithMinimumDistance :: [(String, Int)] -> String
extractNameWithMinimumDistance [] = ""
extractNameWithMinimumDistance list = extractMin "" 999999 list where
    extractMin soFar res [] = soFar
    extractMin soFar res (x:xs) = if snd x < res then extractMin (fst x ) (snd x) xs else extractMin soFar res xs



-- if a typo has been found, get the correct form by calculating the levensthein distance to all the words
-- marked in the second table and replacing "word" with the that
getRowIfTypo :: String -> String -> Row -> Table -> Row
getRowIfTypo word col row table = map (\str -> if str == word then
    extractNameWithMinimumDistance (distanceToEachWord word col table) else str) row


-- goes through each row of the typo table with map
-- filter the elements that have typos
-- if the element has a typo then replace the value at index with the corrected form
-- otherwise do nothing
{-
firstTableIncorrect = the first table that has been filtered
secondTableIncorrect = the second table that has been filtered
listOfElements = a list of Values from the given row (the words that contain typos)\
indexTypo = the index of the column that needs to be corrected
indexReference = the index of the column that we have as referance

-}
correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = map (\row -> if extract_el row indexTypo `elem` listOfElements then
     getRowIfTypo (extract_el row indexTypo) col row secondTableIncorrect else row ) csv1 where
        firstTableIncorrect = filterOutMatches col csv1 csv2
        secondTableIncorrect = filterOutMatches col csv2 csv1
        listOfElements = fromQResultToList (eval $ AsList col (FromTable firstTableIncorrect))
        indexTypo = get_column col (head csv1)
        indexReference = get_column col (head csv2)

