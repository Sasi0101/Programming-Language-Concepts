module TtlReader where
import System.Environment
import Control.Exception
import System.IO
import Data.List
import Data.Text (strip)
import Data.Char
import GHC.IO (unsafePerformIO)

readTtl' f = do
    source_text <- readFile f
    --let source_text2 = [unsafePerformIO x | x <- source_text]
    let res = all_together source_text
    return res
    --return res
--readTtl f = return (all_together (unsafePerformIO  (readFile f)))

readTtl f = unsafePerformIO $ all_together $ (unsafePerformIO (readFile f))

-- Parameter: list of lines (string) from file content
-- Return: a prefix relation (var_name, value)
get_all_prefixes :: [String] -> [(String, String)]
get_all_prefixes str = map get_prefix $ filter (isPrefixOf "@prefix") str
        where
            get_prefix str = (head $ dbl_split str ':' ' ', head $ dbl_split str '>' '<')
            dbl_split xs c1 c2 = split c1 $ split c2 xs !! 1

-- all_together :: String -> [[(String, String, String)]]
all_together source_text = do
        let new_source_text = filter (/= '+') source_text
        let base = get_base $ clean_lst new_source_text
        let pref_lst = get_all_prefixes $ clean_lst new_source_text
        return $ nub $ concat $ map (filter (/= ("","",""))) $ map (test base pref_lst ) $ filter (not . null) $ clean_lst new_source_text
        -- return $ map that_one t
        where clean_lst xs = map trim $ filter (not . null) $ split '\n' (xs)

toOneList (x:xs) | null xs = [(head x, x !! 1, x !! 2)]
                 | otherwise = (head x, x !! 1, x !! 2) : toOneList xs

test :: String -> [(String, String)] -> String -> [(String, String, String)]
test base pref_lst xs
        | isPrefixOf "@base" xs = [("","","")]
        | isPrefixOf "@prefix" xs = [("","","")]
        | elem ';' xs = extract_predicate_list base xs
        | elem ',' xs = extract_subject_list base xs
        | ((length $ filter (== ':') xs) == 3) && ((length $ filter (== '<') xs) == 0) = [extract_prefix_list base xs pref_lst]
        | otherwise = [string_to_correct base xs pref_lst]

hello pref_lst xs
    | (length pref_lst) > 0 && isPrefixOf (fst(pref_lst !! 0)) xs = snd(pref_lst !! 0) ++ (tail $ tail $ xs)
    | (length pref_lst) > 1 && isPrefixOf (fst(pref_lst !! 1)) xs = snd(pref_lst !! 1) ++ (tail $ tail $ xs)
    | (length pref_lst) > 2 && isPrefixOf (fst(pref_lst !! 2)) xs = snd(pref_lst !! 2) ++ (tail $ tail $ xs)
    | (length pref_lst) > 3 && isPrefixOf (fst(pref_lst !! 3)) xs = snd(pref_lst !! 3) ++ (tail $ tail $ xs)
    | otherwise = xs

that_one :: (a, a, a) -> [a]
that_one tuple3 = [let (c,_,_) = tuple3 in c, let (_,c,_) = tuple3 in c, let (_,_,c) = tuple3 in c]


-- Extract turtle predicate list
-- extract_predicate_list :: String -> String -> [(String, String, String)]
extract_predicate_list base str = do
    let pred_obj_lst = map (map (check_if_add_base base)) $ map (split ' ')$ filter (not . null) $ map (trim) $ map (filter (/= '.')) $ map (filter (/= ';')) $ split '<' $ intercalate " " $ drop 1 $ split '>' str
    let sub = add_base base $ trim $ filter (/= '>') $ split '<' str !! 1
    map list_to_3_tuple $ map (intercalate " ") $ map (map (check_if_add_chevron base)) $ map ([sub] ++) pred_obj_lst

-- Extract turtle subject list
extract_subject_list :: String -> String -> [(String, String, String)]
extract_subject_list base str = do
    let obj_lst = map trim $ split ',' $ trim $ filter (/= '.') $ split '>' str !! 2
    let sub = add_base base $ sub_pred str !! 0
    let pred = add_base base $ sub_pred str !! 1
    let a = map (\s -> (check_if_add_chevron base sub) ++ " " ++ (check_if_add_chevron base pred) ++ " " ++ (check_if_add_chevron base $ check_if_add_base base s)) obj_lst
    map list_to_3_tuple a
        where sub_pred s = map trim $ map (filter (/= '<')) $ split '>' s

check_if_add_base base xs
    | (elem True $ map isAlpha xs) && xs /= "true" && xs /= "false" && (head xs) /= '\"' = add_base base xs
    | otherwise = xs

check_if_add_chevron base xs
    | base == "" && (isPrefixOf "http" xs) = "<" ++ xs ++ ">"
    | base == "" = xs
    | isPrefixOf base xs = "<" ++ xs ++ ">"
    | otherwise = xs

-- Parameter: simple one line (string) from file content + list of prefix relations
-- Return: 3-tuple values of a given prefix line
extract_prefix_list :: String -> String -> [(String, String)] -> (String, String, String)
extract_prefix_list base str pref_lst = do
        let a = map trim $ split ' ' str
        let b = take 3 $ map (split ':') a
        (
            check_if_add_chevron base $ add_base base $ get_pref_base 0 b pref_lst ++ get_pref_value 0 b,
            check_if_add_chevron base $ add_base base $ get_pref_base 1 b pref_lst ++ get_pref_value 1 b,
            check_if_add_chevron base $ add_base base $ get_pref_base 2 b pref_lst ++ get_pref_value 2 b)
        where
            get_pref_base i b xs = snd $ head $ filter (\a -> elem ((b !! i) !! 0) $ [fst a]) xs
            get_pref_value i b = (b !! i) !! 1

-- string_to_correct :: String -> String -> String
string_to_correct base str pref_lst = do
        let a = filter (not . null) $ init $ split ' ' $ intercalate " " $ filter (not . null) $ map (filter (/= '>')) $ split '<' str
        let b = [add_base base $ hello pref_lst $ a !! 0, add_base base $ hello pref_lst $ a !! 1, check_if_add_base base $ hello pref_lst $ a !! 2]
        let c = intercalate " " $ map (check_if_add_chevron base) $ b
        list_to_3_tuple c

-- only gets the relation ships without the starts with 'a'
filter_list :: [String] -> [String]
filter_list xss = do
        let one = filter (not . null) $ filter (notElem '@') xss
        let two = map (filter (/= '+')) one
        nub two

-- Gets the base of the ttl file
-- Parameters: List of strings. Strings only the 3 tuple
get_base :: [String] -> String
get_base xss
    | ((length $ filter (isPrefixOf "@base") xss) > 0) = do
        let line = filter (isPrefixOf "@base") xss !! 0
        (split '>' $ (split '<' line)!!1) !!0
    | otherwise = ""

-- Adds base to string if needed
-- Parameters: base and uri
add_base :: String -> String -> String
add_base base uri
        | isPrefixOf base uri = uri
        | isPrefixOf "http" uri = uri
        | isPrefixOf "<http" uri = uri
        | otherwise = base ++ uri

list_to_3_tuple :: String -> (String, String, String)
list_to_3_tuple file_content = do
        (b file_content !! 0, b file_content !! 1, b file_content !! 2)
        where
            b a = (split ' ' a)

trim :: String -> String
trim = unwords . words

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
    (ls, "") -> [ls]
    (ls, x:rs) -> ls : split c rs
