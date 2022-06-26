
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
import Data.Array



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

compute_average_steps :: Table -> Table
-- suma unei liste
sum_of_list list = foldl (+) 0 list
--medie
average l = printf "%.2f" (sum_of_list l / 8)

to_Float li = map (\a -> read a :: Float) li 
-- apica media pe o lista
comp m = average (to_Float m)

first  = ("Name" : "Average Number of Steps" : []) 
-- calcul medie de pasi pe tot tabelul
fun m  = ((head m) : (comp(tail m)) : []) 
compute_average_steps  m = first : (map fun (tail m))

-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int

-- verificare conditie
pass elem = if head elem >= 1000 then True else False
suma row = sum_of_list(to_Float row)

-- creeaza tabel cu total de pasi
create m = if (suma (tail m)) >= 1000 then True  else False
transf ls =  map create (tail ls)
get_passed_people_num m = foldr (\value acc -> if value == True then acc + 1 else acc) 0 (transf m)

-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
-- valoare numarator
numarator m = (get_passed_people_num m) 
-- valoare numitor
numitor m = length (tail m)
printasF m = printf "%.2f" m
percentage m = printasF ((fromIntegral (numarator m) :: Float) / (fromIntegral (numitor m) :: Float ))
get_passed_people_percentage m =  read (percentage m) :: Float 


-- Average number of daily steps
get_steps_avg :: Table -> Float
-- afiseaza suma in format Float
s l = printf "%.2f" (sum_of_list l)
-- aplica s pe o linie
c m = s (to_Float m)
-- creeaza tabel
f m  = ((head m) : (c(tail m)) : []) 
compute_sum  m = (map f (tail m))

createAvgTable m = map tail  (compute_sum m)
listofAvg m = to_Float (map head (createAvgTable m))
sum_avg m = sum_of_list (listofAvg m) 

avg m = sum_avg m / (fromIntegral (length (listofAvg m)) :: Float) 
printAvg :: PrintfType t => [[String]] -> t
printAvg m = printf "%.2f" (avg m)

get_steps_avg m = read (printAvg m) :: Float

-- Task 3

get_avg_steps_per_h :: Table -> Table

list_of_hours l = tail l
table_of_hours t = map list_of_hours (tail t)
-- transformare tabel 
trans_table t = map to_Float (transpose (table_of_hours t))

-- calcul medie pe linie
avg_line l =  (sum_of_list l / fromIntegral (length l) :: Float)

printFloat :: Float -> String 
printFloat l = printf "%.2f" l

avg_table::[[String]] -> [Float]
avg_table t = map avg_line (trans_table t)

avg_tableStr t = map printFloat (avg_table t)
-- prima linie
hours_list = "H10" : "H11" :"H12" :"H13" :"H14" :"H15" :"H16" :"H17" : []

get_avg_steps_per_h m = hours_list : (avg_tableStr m) : []

-- Task 4

get_activ_summary :: Table -> Table

three_activities l = tail (tail (tail l))
-- creare tabel
three_activ_table t = map three_activities (tail t)
int_table t = map to_Float (three_activ_table t)

sec_el l = head (tail l)
first_list t = map head (int_table t)
third_el l = head (tail (tail l))
second_list t = map sec_el (int_table t)
third_list t = map third_el (int_table t)
-- verificari pentru fiecare categorie
verif_r1 el = if el < 50 then True else False 
verif_r2 el = if el >= 50 && el < 100 then True else False 
verif_r3 el = if el >= 100 && el < 500 then True else False 

-- prima linie
line1 = "column" : "range1" : "range2" : "range3" : []

-- aplicare verificari pentru ficare linie
va_r1 t = printf "%.d" (length (filter verif_r1 (first_list t)))
va_r2 t = printf "%d" (length (filter verif_r2 (first_list t)))
va_r3 t = printf "%d" (length (filter verif_r3 (first_list t)))

-- a doua linie
line2 t = "VeryActiveMinutes" : (va_r1 t) : (va_r2 t) : (va_r3 t) : []

fa_r1 t = printf "%d" (length (filter verif_r1 (second_list t)))
fa_r2 t = printf "%d" (length (filter verif_r2 (second_list t)))
fa_r3 t = printf "%d" (length (filter verif_r3 (second_list t)))

-- a treia linie
line3 t = "FairlyActiveMinutes": (fa_r1 t) : (fa_r2 t) : (fa_r3 t) : []

la_r1 t = printf "%d" (length (filter verif_r1 (third_list t)))
la_r2 t = printf "%d" (length (filter verif_r2 (third_list t)))
la_r3 t = printf "%d" (length (filter verif_r3 (third_list t)))

line4 t = "LightlyActiveMinutes": (la_r1 t) : (la_r2 t) : (la_r3 t) : []

get_activ_summary m = (line1 ): (line2 m) : (line3 m) : (line4 m) : []

-- Task 5

get_ranking :: Table -> Table

p l = tail l
-- calcul primele doua coloane
first_twoCol l = head l : head (tail l) : []
-- creare tabel
table m = map first_twoCol (tail m)
int_format n = read n :: Int
-- functie de comparare
co [n1, nr1] [n2, nr2] 
                        |int_format nr1 == int_format nr2 &&  n1 > n2 = GT 
                        |int_format nr1 == int_format nr2 && n1 < n2 = LT 
                        |int_format nr1 < int_format nr2 = LT 
                        |int_format nr1 > int_format nr2 = GT 
                        |otherwise = EQ
line_one = "Name":"Total Steps":[]

new l = head l : (head (tail l)) : []
get_ranking m = line_one : (sortBy co (map new (tail m))) 

-- Task 6
get_steps_diff_table :: Table -> Table

hours l = map int_format (tail l)
-- calcul medie pe o linie
averageOne l = (fromIntegral (sum (take 4 (hours l))) :: Float) / 4
-- calcul pentru primele 4 valori
avg_first4 l = printf "%.2f" (averageOne l)
averageTwo l = (fromIntegral (sum (take 4 (reverse (hours l)))) :: Float) / 4
-- calcul pentru ultimele 4 valori
avg_last4 l = printf "%.2f" (averageTwo l)
-- calcul diferenta
dif_avg l = printf "%.2f" (abs(averageOne l - averageTwo l))

new_line l = (head l) : (avg_first4 l) : (avg_last4 l) : (dif_avg l) : []

-- creare tabel
dif_table m = map new_line (tail m)

float_format el = read el :: Float
-- functie de comparare
compar [n1,firstAvg1,secAvg1,dif1] [n2,firstAvg2,secAvg2,dif2] 
                        |float_format dif1 == float_format dif2 &&  n1 > n2 = GT 
                        |float_format dif1 == float_format dif2 && n1 < n2 = LT 
                        |float_format dif1 < float_format dif2 = LT 
                        |float_format dif1 > float_format dif2 = GT 
                        |otherwise = EQ
first_row  = "Name":"Average first 4h":"Average last 4h":"Difference" : []
get_steps_diff_table m = first_row : (sortBy compar (dif_table m))
-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
-- implementare functie map
myMap :: (a -> b) -> [a] -> [b]
myMap fun [] = []
myMap fun (x:xs) = (fun x) : (myMap fun xs)
vmap f m = myMap(myMap (f)) m
correct_table table = vmap (\x -> if x == "" then "0" else x) table


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table

get_sleep_total :: Row -> Row
-- suma pentru dormit
sum_sleep :: [String] -> Float
sum_sleep l =  sum(map float_format (tail l)) 
-- printare in format Float
sleep l = printf "%.2f" (sum_sleep l)

-- aplicare sleep 
get_sleep_total r = head r : [sleep r] 

rmap f s m = s : (map f m)

{-
    TASK SET 2
-}

-- Task 1
tsort :: ColumnName -> Table -> Table

-- find the number of the column that must be sorted
find_nr_of_column list string = function list string 0
                                        where function ls str n
                                                    | ls == [] = n
                                                    | ((head ls) == str) = function [] str n
                                                    | otherwise = function (tail ls) str (n + 1)                         

                 
cmp_el l1 l2 ind
                |int_format(l1 !! ind) == int_format (l2 !! ind) && (l1 !! 0) > (l2 !! 0) = GT
                |int_format(l1 !! ind) == int_format(l2 !! ind) && (l1 !! 0) < (l2 !! 0) = LT
                |int_format(l1 !! ind) > int_format(l2 !! ind) = GT 
                |int_format(l1 !! ind) < int_format(l2 !! ind) = LT 
                |otherwise = EQ


tsort column table = (head table) : (sortBy (\a b -> cmp_el a b (find_nr_of_column (head table) column)) (tail table)) 

-- Task 2
vunion :: Table -> Table -> Table
-- function to verify if two tables have the same columns
verify :: [String] -> [String] -> Int
verify t1 t2 = function t1 t2 0
                    where function t1 t2 logic 
                            |(length t1 /= length t2) == True = 1
                            |t1 == [] = logic
                            |(head t1 /= head t2) == True = 1
                            |otherwise = function (tail t1) (tail t2) logic
vunion t1 t2 = if (verify (head t1) (head t2)) == 0 then (t1 ++ tail t2) else t1

-- Task 3

hunion :: Table -> Table -> Table
-- calculate number of elements 
count_elements t = function t 0
                        where function table count 
                                    |table == [] = count
                                    |otherwise = function (tail table) (count + 1)

-- create empty line
create_empty line = function line 0 (count_elements(line)) []
                        where function t current count new
                                    |current == count = new : []
                                    |otherwise = function (tail t) (current + 1) count ([""] ++ new)
-- case 1 first > second
create_empty_firstCase t1 t2 = function t1 t2 (count_elements t1) (count_elements t2) 
                            where function tab1 tab2 nr1 nr2 
                                        |nr1 == nr2 = tab2
                                        |otherwise = function (tail tab1) (tab2 ++ (create_empty (head tab2))) nr1  (nr2 + 1)
create_new_firstCase t1 t2 = function t1 (create_empty_firstCase t1 t2) []
                            where function tab1 tab2 new 
                                        |length tab1 == 0 = new 
                                        |otherwise = function (tail tab1) (tail tab2) (new ++ (((head tab1) ++ (head tab2)) : []))

-- case 2 second > first
create_empty_secondCase t1 t2 = function t1 t2 (count_elements t1) (count_elements t2) 
                            where function tab1 tab2 nr1 nr2 
                                        |nr1 == nr2 = tab1
                                        |otherwise = function (tab1 ++ (create_empty (head tab1))) (tail tab2)  (nr1 + 1) nr2

create_new_secondCase t1 t2 = function (create_empty_secondCase t1 t2) t2 []
                            where function tab1 tab2 new 
                                        |length tab1 == 0 = new 
                                        |otherwise = function (tail tab1) (tail tab2) (new ++ (((head tab1) ++ (head tab2)) : []))

hunion  t1  t2 = if (count_elements t1) > (count_elements t2) then create_new_firstCase t1 t2 else create_new_secondCase t1 t2


-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table

-- verify if the value of the key_column exists in the second table
-- function returns line if exists, [] otherwise
exists_value :: Int -> String -> [[String]] -> [String]
exists_value key_column value t2 = function key_column value t2 []
                                        where function col val tab2 new_line
                                                |tab2 == [] = new_line
                                                |((head tab2) !! col) == val = function col val [] (new_line ++ (head tab2))
                                                |otherwise = function col val (tail tab2) new_line
                                                
-- create line with the columns' names of the final table
columns_names :: Table -> Table -> Row
columns_names t1 t2 = nub ((head t1) ++ (head t2))

-- verify if a column exists in a table
-- True if exists, False does not exist
exists_col :: String -> Row -> Bool
exists_col col tab = function col tab False 
                        where function c t new
                                |t == [] = new
                                |col == (head t) = function col [] True
                                |otherwise = function col (tail t) False
-- compare values for the columns with the same names
cmp_values :: String -> [String]-> [String] -> [[String]] -> [[String]] -> String
cmp_values col row1 row2 tab1 tab2 = function col row1 row2 tab1 tab2 ""
                                        where function c r1 r2 t1 t2 new
                                                |(exists_col col (head tab1)) == True && (exists_col col (head t2)) == False = (row1 !! (find_nr_of_column (head tab1) col))
                                                |(exists_col col (head tab1)) == False && (exists_col col (head t2)) == True = (row2 !! (find_nr_of_column (head tab2) col))
                                                |(exists_col col (head tab1)) == True && (exists_col col (head t2)) == True && 
                                                (row2 !! (find_nr_of_column (head tab2) col)) /= "" =  (row2 !! (find_nr_of_column (head tab2) col))
                                                |otherwise = (row1 !! (find_nr_of_column (head tab1) col))


-- create one line of the table according to the line 
-- merges one line from the first table with one from the other one
create_one :: String -> Row -> Row -> Table -> Table -> Row -> [String]
create_one key_column l1 l2 t1 t2 line = function l1 l2 t1 t2 line []
                                                where function line1 line2 tab1 tab2 row new 
                                                        |row == [] = new 
                                                        |otherwise = function  line1 line2 tab1 tab2 (tail row) (new ++ ((cmp_values (head row) line1 line2 tab1 tab2)) : []) 
-- merges one line from the first with the searched line from table two
merge_many :: String -> Row -> Table -> Table -> Row -> [String]
merge_many key_column l1 t1 t2 line = if (exists_value (find_nr_of_column (head t2) key_column) (l1 !! (find_nr_of_column (head t2) key_column)) t2) /= [] 
                                      then create_one key_column l1 (exists_value (find_nr_of_column (head t2) key_column) (l1 !! (find_nr_of_column (head t2) key_column)) t2)  t1 t2 line else []

-- merges every line from the first with the searched line from table two
merge_every ::String -> Table -> Table -> Table
merge_every key_column t1 t2 = function key_column (tail t1) t1 t2 []
                                        where function c tab1 first tab2 new
                                                |tab1 == [] = new 
                                                |(merge_many c (head tab1) first tab2 (columns_names first tab2)) /= [] = function c (tail tab1) first tab2 (new ++ ((merge_many c (head tab1) first tab2 (columns_names first tab2)) : []))
                                                |otherwise = function c (tail tab1) first tab2 new

tjoin key_column t1 t2 = (columns_names t1 t2) : (merge_every key_column t1 t2)

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table

-- merge first line from the first table with a line of the second table
cartesian_one_to_one :: (Row -> Row -> Row) -> Table -> Table -> Row
cartesian_one_to_one fun t1 t2 = fun (head t1) (head t2)

-- merge first line from the first table with all the lines from the second table
cartesian_one_to_many :: (Row -> Row -> Row) -> [Row] -> [Row] -> [Row]
cartesian_one_to_many fun t1 t2 = function fun t1 t2 []
                                        where function f tab1 tab2 new
                                                |tab2 == [] = new 
                                                |otherwise = function f tab1 (tail tab2) (new ++ ((cartesian_one_to_one f tab1 tab2) : [] ))

-- merge every line from the first table with all the lines from the second table
cartesian_many_to_many :: (Row -> Row -> Row) -> [Row] -> [Row] -> [Row]
cartesian_many_to_many fun t1 t2 = function fun (tail t1) (tail t2) []
                                        where function f tab1 tab2 new 
                                                |tab1 == [] = new
                                                |otherwise = function f (tail tab1) tab2 (new ++ (cartesian_one_to_many f tab1 tab2))

cartesian new_row_function new_column_names t1 t2 = new_column_names : (cartesian_many_to_many new_row_function t1 t2)
-- Task 6

projection :: [ColumnName] -> Table -> Table
-- list of indexes for the columns that must be extracted
ind listStr listTab = function [] listStr listTab
                            where function new str tab 
                                    |str == [] = new
                                    |otherwise = function (new ++ ((find_nr_of_column tab (head str)) : [])) (tail str) (tab) 

-- takes from a list only elements with indexes from listIndex
new_list listStr listIndex = function listStr listIndex []
                                    where function str ind new
                                            |ind == [] = new
                                            |otherwise = function str (tail ind) (new ++ ((str !! (head ind)) : []))

-- apply new_list for each line in the table
new_table table listIndex = function table listIndex []
                                where function t l n
                                        |t == [] = n
                                        |otherwise = function (tail t) l (n ++ ((new_list (head t) l) : []))

projection columns_to_extract t = new_table t (ind columns_to_extract (head t))
-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table

-- verify if a row respects the condition 
verif_cond_line line column cond = if (cond (line !! column) == True) then True else False 

-- add all rows that respect the condition for the column with the specific index
condition_table :: Int -> [[String ]] -> (String -> Bool) -> [[String ]]
condition_table column table cond = function column table cond []
                                        where function col tab c new
                                                    |tab == [] = new
                                                    |(verif_cond_line (head tab) col cond) == True = function col (tail tab) c (new ++ ((head tab) : []))
                                                    |otherwise = function col (tail tab) c new

filterTable condition key_column t = (head t) : (condition_table (find_nr_of_column (head t) key_column) (tail t) condition) 



