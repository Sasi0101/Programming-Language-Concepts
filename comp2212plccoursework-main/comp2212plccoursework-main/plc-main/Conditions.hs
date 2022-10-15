module Conditions where
import Data.List
import Data.Char
import TtlReader (all_together)
import System.Environment
import Control.Exception
import Text.Read
-- import System.IO
-- import Control.Monad

check_if_add_chevron base xs
    | isPrefixOf base xs = "<" ++ xs ++ ">"
    | otherwise = xs

get_base :: String -> String
get_base xs = do
        let line = filter (isPrefixOf "@base") $ clean_lst xs
        filter (/='>') $ filter (/='<') $ (split ' ' (line !! 0)) !! 1
        where
            clean_lst xs = map trim $ filter (not . null) $ split '\n' (xs)

-- get_bool :: String -> (String -> String -> Bool)
get_bool xs
    | xs == "==" = (==)
    | xs == "<" = (<)
    | xs == ">" = (>)
    | xs == "<=" = (<=)
    | xs == ">=" = (>=)
get_bool _ = error "Error at get_bool"

checkValidInt :: String -> Bool
checkValidInt strInt = Nothing /= (readMaybe $ filter (/= '+') strInt :: Maybe Int)

filter_type file_content_tuple3 value
    | checkValidInt value == True = map (\s -> (s!!0,s!!1,s!!2)) $ filter (\s -> checkValidInt (s !! 2)) $ map tuple3_to_lst file_content_tuple3
    | otherwise = file_content_tuple3

-- Takes as param: handle_absolute_values ["foo.subj","==","http://www.cw.org/#problem2"]
-- test :: [String] -> [(String, String, String)]
handle_absolute_values cond
    | ((check_stuff cond) !! 0 == 2) && ((check_stuff cond) !! 2 == 2) = do -- handling values between 2 files
        let file_and_elem = split '.' $ cond !! 0
        let file_and_elem_2 = split '.' $ cond !! 2
        file_content <- readFile $ (head file_and_elem) ++ ".ttl"
        file_content_tuple3 <- all_together file_content
        file_content_2 <- readFile $ (head file_and_elem_2) ++ ".ttl"
        file_content_tuple3_2 <- all_together file_content_2
        let element = file_and_elem !! 1
        let element_2 = file_and_elem_2 !! 1
        let value = map snd $ map (lst_to_tuple element_2) file_content_tuple3_2
        let bool = get_bool $ cond !! 1
        let a = filter_type file_content_tuple3 $ head value
        return $ apply_condition a bool value element
    | (check_stuff cond) !! 0 == 2 = do -- handling absolute values
        let file_and_elem = split '.' $ cond !! 0
        file_content <- readFile $ (head file_and_elem) ++ ".ttl"
        file_content_tuple3 <- all_together file_content
        let element = file_and_elem !! 1
        let value = [check_if_add_chevron (get_base file_content) (cond !! 2)]
        let bool = get_bool $ cond !! 1
        let a = filter_type file_content_tuple3 $ head value
        return $ apply_condition a bool value element
    | (check_stuff cond) !! 2  == 2 = do -- handling absolute values
        let file_and_elem = split '.' $ cond !! 2
        file_content <- readFile $ (head file_and_elem) ++ ".ttl"
        file_content_tuple3 <- all_together file_content
        let element = file_and_elem !! 1
        let value = [check_if_add_chevron (get_base file_content) (cond !! 0)]
        let bool = get_bool $ cond !! 1
        let a = filter_type file_content_tuple3 $ head value
        return $ apply_condition a bool value element
    where check_stuff cond_bis = map length $ map (split '.') cond_bis
handle_absolute_values _  = error "Error at handle_absolute_values"

-- Takes as param: handle_conditions [["foo.pred","==","http://www.cw.org/problem3/predicate1"], ["||"], ["foo.pred","==","http://www.cw.org/problem3/predicate2"], ["||"], ["foo.pred","==","http://www.cw.org/problem3/predicate3"]] []
-- Takes as param: handle_conditions [["foo.subj","==","http://www.cw.org/#problem2"], ["&&"], ["foo.obj","==","true"]] []
-- problem 4: handle_conditions [["foo.subj","==","bar.obj"], ["||"], ["bar.subj","==","foo.obj"]] []
handle_conditions cond res
    | ((length cond > 1) && (head cond /= ["&&"] && head cond /= ["||"])) = do
        a <- handle_absolute_values $ head cond
        handle_conditions (drop 1 cond) (res ++ a)
    | (length cond /= 0) && (head cond == ["&&"]) = do
        a <- (handle_absolute_values (cond !! 1))
        handle_conditions (drop 2 cond) (intersect' res a)
    | (length cond /= 0) && (head cond == ["||"]) = do
        a <- (handle_absolute_values (cond !! 1))
        handle_conditions (drop 2 cond) (res ++ a)
    | (length cond == 1) = do
        a <- handle_absolute_values $ head cond
        return $ res ++ a
    | otherwise = return $ res

intersect' :: (Eq a) => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' xs ys = filter (\x -> x `elem` xs) ys

-- Takes as input
    -- input_lst: the list of 3tuple from a file
    -- comp_lst: all the values to be compared with
    -- element: which element the values from comp_lst will be compared with input_lst ("subj" || "pred" || "obj")
    -- bool: the compare element that will be used (==, <, >...)
-- Compares a list of 3tuple against list of values, and returns the result

-- apply_condition :: [(String, String, String)] -> (String -> String -> Bool) -> [String] -> String -> Boolean -> [(String, String, String)]
apply_condition input_lst bool comp_lst element = do
        let tpl_subj = map (lst_to_tuple element) input_lst
        let boolean_mask = yo tpl_subj comp_lst bool [] (length comp_lst)
        apply_bool_mask boolean_mask input_lst [] (length boolean_mask)
    where
        test tpl bool_mask = map (split ' ') $ map fst $ head $ apply_bool_mask bool_mask tpl [] (length bool_mask)

-- let lst = [("google.com/subj1", "google.com/pred1", "google.com/obj1"), ("google.com/subj2", "google.com/pred2", "google.com/obj2"), ("google.com/subj3", "google.com/pred3", "google.com/obj3")]

-- let tpl_subj = map (lst_to_tuple "subj") lst

-- let comp_lst = ["google.com/subj1", "google.com/subj2"]

-- let bool_mask = [[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False,False],[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True],[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False],[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]]

-- let tpl_subj = [("<http://www.cw.org/subjectA> <http://www.cw.org/predicateA> <http://www.cw.org/objectA>","<http://www.cw.org/subjectA>"),("<http://www.cw.org/subjectB> <http://www.cw.org/predicateB> <http://www.cw.org/objectB>","<http://www.cw.org/subjectB>"),("<http://www.cw.org/pprefix/subjectC> <http://www.cw.org/pprefix/predicateC> <http://www.cw.org/pprefix/objectC>","<http://www.cw.org/pprefix/subjectC>"),("<http://www.cw.org/pprefix/subjectD> <http://www.cw.org/qprefix/predicateD> <http://www.cw.org/qprefix/objectD>","<http://www.cw.org/pprefix/subjectD>"),("<http://www.cw.org/pprefix/subjectE> <http://www.cw.org/rprefix/predicateE> <http://www.cw.org/qprefix/objectE>","<http://www.cw.org/pprefix/subjectE>"),("<http://www.cw.org/testSubA> <http://www.cw.org/hasNegInt> -50","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/hasPosInt> 50","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/hasBool> true","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/hasBool> false","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/hasStringLit> \"literalstring\"","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/testPredList> -5","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/testPredList> 10","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/testPredList> 20","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/testObjList> -5","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/testObjList> 10","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubA> <http://www.cw.org/testObjList> 20","<http://www.cw.org/testSubA>"),("<http://www.cw.org/#problem2> <http://www.cw.org/testPredA> true","<http://www.cw.org/#problem2>"),("<http://www.cw.org/#problem2> <http://www.cw.org/testPredA> false","<http://www.cw.org/#problem2>"),("<http://www.cw.org/#problem2> <http://www.cw.org/testPredB> true","<http://www.cw.org/#problem2>"),("<http://www.cw.org/#problemNot2> <http://www.cw.org/testPredB> true","<http://www.cw.org/#problemNot2>"),("<http://www.cw.org/#problemNot2> <http://www.cw.org/testPredB> false","<http://www.cw.org/#problemNot2>"),("<http://www.cw.org/testSubA> <http://www.cw.org/problem3/#predicate1> true","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubB> <http://www.cw.org/problem3/#predicate2> true","<http://www.cw.org/testSubB>"),("<http://www.cw.org/testSubC> <http://www.cw.org/problem3/#predicate3> true","<http://www.cw.org/testSubC>"),("<http://www.cw.org/testSubA> <http://www.cw.org/problem3/#predicate4> true","<http://www.cw.org/testSubA>"),("<http://www.cw.org/testSubB> <http://www.cw.org/problem3/#predicate5> true","<http://www.cw.org/testSubB>"),("<http://www.cw.org/testSubC> <http://www.cw.org/problem3/#predicate6> true","<http://www.cw.org/testSubC>"),("<http://www.cw.org/testSubC> <http://www.cw.org/problem3/#predicate2> true","<http://www.cw.org/testSubC>"),("<http://www.cw.org/prob4A> <http://www.cw.org/testPredA> <http://www.cw.org/prob4B>","<http://www.cw.org/prob4A>"),("<http://www.cw.org/prob4C> <http://www.cw.org/testPredB> <http://www.cw.org/prob4D>","<http://www.cw.org/prob4C>"),("<http://www.cw.org/prob4E> <http://www.cw.org/testPredC> <http://www.cw.org/prob4F>","<http://www.cw.org/prob4E>")]

-- let foo_tpl = [("<http://www.cw.org/subjectA>","<http://www.cw.org/predicateA>","<http://www.cw.org/objectA>"),("<http://www.cw.org/subjectB>","<http://www.cw.org/predicateB>","<http://www.cw.org/objectB>"),("<http://www.cw.org/pprefix/subjectC>","<http://www.cw.org/pprefix/predicateC>","<http://www.cw.org/pprefix/objectC>"),("<http://www.cw.org/pprefix/subjectD>","<http://www.cw.org/qprefix/predicateD>","<http://www.cw.org/qprefix/objectD>"),("<http://www.cw.org/pprefix/subjectE>","<http://www.cw.org/rprefix/predicateE>","<http://www.cw.org/qprefix/objectE>"),("<http://www.cw.org/testSubA>","<http://www.cw.org/hasNegInt>","-50"),("<http://www.cw.org/testSubA>","<http://www.cw.org/hasPosInt>","50"),("<http://www.cw.org/testSubA>","<http://www.cw.org/hasBool>","true"),("<http://www.cw.org/testSubA>","<http://www.cw.org/hasBool>","false"),("<http://www.cw.org/testSubA>","<http://www.cw.org/hasStringLit>","\"literalstring\""),("<http://www.cw.org/testSubA>","<http://www.cw.org/testPredList>","-5"),("<http://www.cw.org/testSubA>","<http://www.cw.org/testPredList>","10"),("<http://www.cw.org/testSubA>","<http://www.cw.org/testPredList>","20"),("<http://www.cw.org/testSubA>","<http://www.cw.org/testObjList>","-5"),("<http://www.cw.org/testSubA>","<http://www.cw.org/testObjList>","10"),("<http://www.cw.org/testSubA>","<http://www.cw.org/testObjList>","20"),("<http://www.cw.org/#problem2>","<http://www.cw.org/testPredA>","true"),("<http://www.cw.org/#problem2>","<http://www.cw.org/testPredA>","false"),("<http://www.cw.org/#problem2>","<http://www.cw.org/testPredB>","true"),("<http://www.cw.org/#problemNot2>","<http://www.cw.org/testPredB>","true"),("<http://www.cw.org/#problemNot2>","<http://www.cw.org/testPredB>","false"),("<http://www.cw.org/testSubA>","<http://www.cw.org/problem3/#predicate1>","true"),("<http://www.cw.org/testSubB>","<http://www.cw.org/problem3/#predicate2>","true"),("<http://www.cw.org/testSubC>","<http://www.cw.org/problem3/#predicate3>","true"),("<http://www.cw.org/testSubA>","<http://www.cw.org/problem3/#predicate4>","true"),("<http://www.cw.org/testSubB>","<http://www.cw.org/problem3/#predicate5>","true"),("<http://www.cw.org/testSubC>","<http://www.cw.org/problem3/#predicate6>","true"),("<http://www.cw.org/testSubC>","<http://www.cw.org/problem3/#predicate2>","true"),("<http://www.cw.org/prob4A>","<http://www.cw.org/testPredA>","<http://www.cw.org/prob4B>"),("<http://www.cw.org/prob4C>","<http://www.cw.org/testPredB>","<http://www.cw.org/prob4D>"),("<http://www.cw.org/prob4E>","<http://www.cw.org/testPredC>","<http://www.cw.org/prob4F>")]

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
    (ls, "") -> [ls]
    (ls, x:rs) -> ls : split c rs

-- lst_to_tuple :: String -> (String, String, String) -> (String, String)
lst_to_tuple e tuple3
    | e == "subj" = (intercal tuple3, let (c,_,_) = tuple3 in c)
    | e == "pred" = (intercal tuple3, let (_,c,_) = tuple3 in c)
    | e == "obj" = (intercal tuple3, let (_,_,c) = tuple3 in c)
    | otherwise = ("","")
    where
        intercal tuple3 = intercalate " " $ tuple3_to_lst tuple3

tuple3_to_lst :: (a, a, a) -> [a]
tuple3_to_lst tuple3 = [let (c,_,_) = tuple3 in c, let (_,c,_) = tuple3 in c, let (_,_,c) = tuple3 in c]

apply_bool_mask _ _ res (0) = res
apply_bool_mask boolean_mask lst res i = apply_bool_mask boolean_mask lst (res ++ [ b1 | (b1, a1) <- zip lst (boolean_mask !! (i - 1)), a1]) (i - 1)

-- yo :: [(String, String)] -> [String] -> (String->String->Bool) -> [[Bool]] -> Int -> [[Bool]]
yo :: [(a1, a2)] -> [t] -> (t -> a2 -> Bool) -> [[Bool]] -> Int -> [[Bool]]
yo input_lst [] _ _ _ = [take (length input_lst) $ repeat True]
yo _ _ _ res (0) = res
yo input_lst comp_lst bool res i = yo input_lst comp_lst bool (res ++ [(map ((bool) $ comp_lst !! (i - 1)) $ map snd input_lst)]) (i - 1)

trim :: String -> String
trim = unwords . words
