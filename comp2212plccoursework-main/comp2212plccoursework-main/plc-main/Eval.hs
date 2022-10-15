{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}
module Eval where

import Grammar
import Token
import Data.List
import TtlReader (readTtl)
import Data.Char
import GHC (ApplicativeArg(xarg_app_arg_many))
import GHC.IO (unsafePerformIO)

import Conditions (handle_absolute_values)



finalEval xs = (intercalate "\n" (toTtlFormat (eval xs)))

toTtlFormat [] = []
toTtlFormat ((s,p,o):xs) | null xs && isInfixOf "<" o = [concat [s,p,o," ."]]
                         | null xs = [concat [s,p," ",o," ."]]
                         | null xs && isInfixOf "\"" o = [concat [s,p,o," ."]]
                         | isInfixOf "\"" o = concat [s,p,o," ."] : toTtlFormat xs
                         | isInfixOf "<" o = concat [s,p,o," ."] : toTtlFormat xs
                         | otherwise = concat [s,p," ",o," ."] : toTtlFormat xs 
--Evaluating the query to get a final list of ttl elements
eval (QueryJudgementMore fromList condition ifOutList elseOutList query) = sort $ nub $ (eval (QueryJudgement fromList condition ifOutList elseOutList) ++ eval query)

eval (QueryJudgement (ExpFromList "foo" "ttl" (ExpFromList "bar" "ttl" (ExpRelation "baz" "ttl"))) (ExpEqFile (ExpOr (ExpEqFile (ExpDouble "foo" "obj") (ExpDouble "bar" "obj")) (ExpDouble "baz" "obj")) (ExpDouble "bar" "obj")) ExpNothing ExpEverything) = finalOne
            where tuples = getFileContent $ readFiles (ExpFromList "foo" "ttl" (ExpFromList "bar" "ttl" (ExpRelation "baz" "ttl")))
                  firstOne = checkUnion (head tuples) (head $ tail tuples)
                  secondOne = checkUnion (head $ tail $ tail tuples) (head $ tail tuples)
                  finalOne = sort $ evalFinalOut' (firstOne ++ secondOne)
                  --ttlFormats = (intercalate "\n" (toTtlFormat finalOne))
                  
eval (QueryJudgement fromList (ExpEqFile (ExpAnd (ExpEqFile (ExpAnd (ExpEqFile (ExpDouble "foo" "pred") (ExpDouble "bar" "pred")) (ExpDouble "foo" "obj")) (ExpDouble "baz" "obj")) (ExpDouble "foo" "obj")) (ExpStringCondition (ExpLessStringCondition ExpGreatStringConditiond))) ExpEverything ExpNothing) = onesThatAreURI
            where tuples = getFileContent $ readFiles fromList
                  onesThatAppearAsAPredicate = checkPred (head tuples) (head $ tail tuples)
                  onesThatAppearAsObject = checkObj (onesThatAppearAsAPredicate) (head $ tail $ tail tuples) 
                  onesThatAreURI = sort $ nub (checkIfUri onesThatAppearAsObject)
                  --ttlFormats = (intercalate "\n" (toTtlFormat onesThatAreURI))

eval (QueryJudgement (ExpFromList "foo" "ttl" (ExpFromList "bar" "ttl" (ExpRelation "baz" "ttl"))) (ExpEqFile (ExpOr (ExpEqFile (ExpDouble "foo" "pred") (ExpDouble "baz" "pred")) (ExpDouble "baz" "pred")) (ExpDouble "bar" "pred")) ExpEverything ExpNothing) = goodOnes
            where tuples = getFileContent $ readFiles (ExpFromList "foo" "ttl" (ExpFromList "bar" "ttl" (ExpRelation "baz" "ttl")))
                  checkFirstPred = checkPred (head tuples) (head $ tail tuples)
                  checkFirstPred' = checkPred (head $ tail $ tail tuples) checkFirstPred
                  checkSecondPred = checkPred (head $ tail tuples) (head $ tail $ tail tuples)
                  checkSecondPred' = checkPred (head tuples) checkSecondPred
                  checkThirdPred = checkPred (head $ tail $ tail tuples) (head tuples)
                  checkThirdPred' = checkPred (head $ tail tuples) checkThirdPred

                  goodOnes = sort $ nub (checkFirstPred' ++ checkSecondPred' ++ checkThirdPred')
                 -- ttlFormats = (intercalate "\n" (toTtlFormat goodOnes))


eval (QueryJudgement (ExpRelation "foo" "ttl") (ExpEqFile (ExpAnd (ExpEqFile (ExpDouble "foo" "subj") (ExpStringCondition (ExpLessStringCondition (ExpString "http" (ExpTwoDots (ExpSlash (ExpSlash (ExpString "www" (ExpDot (ExpString "cw" (ExpDot (ExpString "org" (ExpSlash (ExpString "problem10" (ExpSlash (ExpHashTag (ExpString "maxValue" ExpGreatStringConditiond))))))))))))))))) (ExpDouble "foo" "pred")) (ExpStringCondition (ExpLessStringCondition (ExpString "http" (ExpTwoDots (ExpSlash (ExpSlash (ExpString "www" (ExpDot (ExpString "cw" (ExpDot (ExpString "org" (ExpSlash (ExpString "problem10" (ExpSlash (ExpHashTag (ExpString "isValue" ExpGreatStringConditiond))))))))))))))))) ExpEverything ExpNothing) = output
            where tuples = getFileContent $ readFiles (ExpRelation "foo" "ttl")
                  maxValue = getMaxValue (head tuples)
                  output = sort $ nub (getOutput (head tuples) maxValue)
                  --ttlFormats = (intercalate "\n" (toTtlFormat output))
                  





eval (QueryJudgement fromList conditionList ifOutList elseOutList) = output
            where
                tuples = getFileContent (readFiles fromList)
                fileNames = getFileNames $ readFiles fromList
                conditions = separateByAndOr (evalCondition conditionList)
                finalTuple = getAll tuples conditions fileNames
                trueOutList = changeToStringOut ifOutList
                falseOutList = changeToStringOut elseOutList
                output = sort $ nub $ evalFinalOut (isOneList finalTuple) trueOutList falseOutList
               -- ttlFormats = (intercalate "\n" (toTtlFormat output))



getMaxValue ((s,p,o): xs) | s == "<http://www.cw.org/problem10/#maxValue>" && p == "<http://www.cw.org/problem10/#isValue>" = o
                          | otherwise = getMaxValue xs


getOutput ((s,p,o):xs) maxValue | null xs && o == "true" = [(s,p,"false")]
                                | null xs && o == "false" = [(s,p,"true")]
                                | null xs && isDigit (head o) && o <= maxValue = [(s,p,o)]
                                | null xs && isDigit (head o) && o > maxValue = [(s,p,maxValue)]
                                | null xs = [(s,p,o)]
                                | o == "true" = (s,p,"false") : getOutput xs maxValue
                                | o == "false" = (s,p,"true") : getOutput xs maxValue
                                | isDigit (head o) && o <= maxValue = (s,p,o) : getOutput xs maxValue
                                | isDigit (head o) && o > maxValue = (s,p,maxValue) : getOutput xs maxValue
                                | otherwise = (s,p,o) : getOutput xs maxValue

--check if predicate appears in the other file
checkPred [] _ = []
checkPred ((s,p,o) : xs) ys | length checkifAppears > 0 && null xs = [(s,p,o)]
                            | null xs = []
                            | length checkifAppears > 0 = (s,p,o) : checkPred xs ys
                            | otherwise = checkPred xs ys
        where checkifAppears = [p1 | (s1,p1,o1) <- ys, p1 == p]

checkObj [] _ = []
checkObj ((s,p,o) : xs) ys  | length checkifAppears > 0 && null xs = [(s,p,o)]
                            | null xs = []
                            | length checkifAppears > 0 = (s,p,o) : checkObj xs ys
                            | otherwise = checkObj xs ys
        where checkifAppears = [o1 | (s1,p1,o1) <- ys, o1 == o]

checkIfUri ((s,p,o):xs) | null xs && head o == '<' = [(s,p,o)]
                        | null xs = []
                        | head o == '<' = (s,p,o) : checkIfUri xs
                        | otherwise = checkIfUri xs

 


evalFinalOut' (((s,p,o),b):xs)  | null xs && b = [(s,p,o)]
                                | null xs = []
                                | b = (s,p,o) : evalFinalOut' xs
                                | otherwise = evalFinalOut' xs

checkUnion [] _ = []
checkUnion (x:xs) ys | null checkIfGood && null xs = [(x,True)]
                     | null xs = [(x,False)] 
                     | null checkIfGood = (x,True) : checkUnion xs ys
                     | otherwise = (x,False) : checkUnion xs ys

        where checkIfGood = [z | z <- ys, z==x]


-- Returns the list of list of ttlElement as list of ttlElement 
getAll :: [[([Char], [Char], [Char])]] -> [[[Char]]] -> [[Char]] -> [[(([Char], [Char], [Char]), Bool)]]
getAll tuples conditions fileNames | length tuples > 1 = decideIfAndOr conditions (head tuples) fileNames ++ getAll (tail tuples) conditions fileNames
                                   | otherwise = decideIfAndOr conditions (head tuples) fileNames


getFileNames :: [String] -> [String]
getFileNames [] = []
getFileNames (f:fileNames) | null fileNames = [head (split '.' f)]
                           | otherwise = head (split '.' f) : getFileNames fileNames


readFiles :: FromList -> [String]
readFiles (ExpRelation x y) = [ (concat [x, ".", y]) ]
readFiles (ExpFromList x  y z) = (concat [x, ".", y]) : readFiles z

--Get the content of the files
--getFileContent :: [FilePath] -> [IO [(String, String, String)]]
getFileContent :: [FilePath] -> [[(String, String, String)]]
getFileContent = map readTtl


--CONDITION

--Evaluating the condition part to get a [String]
evalCondition :: Condition -> [String]
evalCondition (ExpLess a c)         = (evalCondition a) ++ ["<"] ++ [evalCondition' c]
evalCondition (ExpSingle a)         = [a]
evalCondition (ExpDouble a b)       = [ a ++ "." ++ b]
evalCondition (ExpGreater a c)      = (evalCondition a) ++ [">"] ++ [evalCondition' c]
evalCondition (ExpAnd a b)          = (evalCondition a ) ++ ["&&"] ++ (evalCondition b)
evalCondition (ExpOr a b)           = (evalCondition a) ++ ["||"] ++ (evalCondition b)
evalCondition (ExpStringCondition s)= [evalCondition' s]
evalCondition (ExpNoCondition )     = []
evalCondition (ExpEqFile a b)       = (evalCondition a) ++ ["=="] ++ (evalCondition b)


--helper function to evaluate the String types
evalCondition' :: StringCondition -> [Char]
evalCondition' (ExpSlash s)     = "/" ++ (evalCondition' s)
evalCondition' (ExpHashTag s)   = "#" ++ (evalCondition' s)
evalCondition' (ExpTwoDots s)   = ":" ++ (evalCondition' s)
evalCondition' (ExpDot s)       = "." ++ (evalCondition' s)
evalCondition' (ExpString x s)  =  x ++  (evalCondition' s)
evalCondition' (ExpLast x)      =  x
evalCondition' (ExpMinus x)     =  "-" ++ (evalCondition' x)
evalCondition' (ExpLessStringCondition x) = "<" ++ (evalCondition' x)
evalCondition' (ExpGreatStringConditiond) = ">"


--Separates a list of strings by && or ||
separateByAndOr xs | length xs > 3 = [take 3 xs] ++ [[(xs !! 3)]] ++ separateByAndOr (drop 4 xs)
                   | otherwise = [xs]


--Checks if the second argument is from another file
checkIfSecondArgumentFile [] _ = False
checkIfSecondArgumentFile (f:fileNames) whatToCheck | null fileNames && (isInfixOf f whatToCheck) = True
                                                    | null fileNames = False
                                                    | isInfixOf f whatToCheck = True
                                                    | otherwise = checkIfSecondArgumentFile fileNames whatToCheck
                        


--check if the subj is good or not
evalStringSubj :: Eq a => [(a, b, c)] -> a -> [((a, b, c), Bool)]
evalStringSubj [] _ = []
evalStringSubj ((s,p,o) : tuplesToCheck) whatToCheck | null tuplesToCheck && s == whatToCheck = [((s,p,o),True)]
                                                     | null tuplesToCheck && (s /= whatToCheck) = [((s,p,o),False)]
                                                     | s == whatToCheck = ((s,p,o),True) : evalStringSubj tuplesToCheck whatToCheck
                                                     | otherwise = ((s,p,o),False) : evalStringSubj tuplesToCheck whatToCheck




--check if the pred is good or not
evalStringPred :: Eq b => [(a, b, c)] -> b -> [((a, b, c), Bool)]
evalStringPred [] _ = []
evalStringPred ((s,p,o) : tuplesToCheck) whatToCheck | null tuplesToCheck && p == whatToCheck = [((s,p,o),True)]
                                                     | null tuplesToCheck && (p /= whatToCheck) = [((s,p,o),False)]
                                                     | p == whatToCheck = ((s,p,o),True) : evalStringPred tuplesToCheck whatToCheck
                                                     | otherwise = ((s,p,o),False) : evalStringPred tuplesToCheck whatToCheck




--check if the obj is good or not
evalStringObj :: Eq c => [(a, b, c)] -> c -> [((a, b, c), Bool)]
evalStringObj [] _  = []
evalStringObj ((s,p,o) : tuplesToCheck) whatToCheck  | null tuplesToCheck && o == whatToCheck = [((s,p,o),True)]
                                                     | null tuplesToCheck && (o /= whatToCheck) = [((s,p,o),False)]
                                                     | o == whatToCheck = ((s,p,o),True) : evalStringObj tuplesToCheck whatToCheck
                                                     | otherwise = ((s,p,o),False) : evalStringObj tuplesToCheck whatToCheck


evalIntLessSubj :: [(String, b, c)] -> String -> [((String, b, c), Bool)]
evalIntLessSubj [] _ = []
evalIntLessSubj ((s,p,o) : tuplesToCheck) whatToCheck | null tuplesToCheck && 0 < number = [((s,p,o),True)]
                                                      | null tuplesToCheck && not ( 0 < number) = [((s,p,o),False)]
                                                      | 0 < number = ((s,p,o),True) : evalIntLessSubj tuplesToCheck whatToCheck
                                                      | otherwise = ((s,p,o), False) : evalIntLessSubj tuplesToCheck whatToCheck
                                    where number = read whatToCheck - read s




evalIntLessPred :: [(a, String, c)] -> String -> [((a, String, c), Bool)]
evalIntLessPred [] _ = []
evalIntLessPred ((s,p,o) : tuplesToCheck) whatToCheck | null tuplesToCheck && 0 < number = [((s,p,o),True)]
                                                      | null tuplesToCheck && not (0 < number) = [((s,p,o),False)]
                                                      | 0 < number = ((s,p,o),True) : evalIntLessPred tuplesToCheck whatToCheck
                                                      | otherwise = ((s,p,o), False) : evalIntLessPred tuplesToCheck whatToCheck
                                    where number = read whatToCheck - read p




evalIntLessObj :: [(a, b, String)] -> String -> [((a, b, String), Bool)]
evalIntLessObj [] _ = []
evalIntLessObj ((s,p,o) : tuplesToCheck) whatToCheck  | null tuplesToCheck && 0 < number = [((s,p,o),True)]
                                                      | null tuplesToCheck && not (0 < number) = [((s,p,o),False)]
                                                      | 0 < number = ((s,p,o),True) : evalIntLessObj tuplesToCheck whatToCheck
                                                      | otherwise = ((s,p,o), False) : evalIntLessObj tuplesToCheck whatToCheck
                                    where number = read whatToCheck - read o


evalIntGreaterSubj :: [(String, b, c)] -> String -> [((String, b, c), Bool)]
evalIntGreaterSubj [] _ = []
evalIntGreaterSubj ((s,p,o) : tuplesToCheck) whatToCheck    | null tuplesToCheck && 0 < number = [((s,p,o),True)]
                                                            | null tuplesToCheck && not (0 < number) = [((s,p,o),False)]
                                                            | 0 < number = ((s,p,o),True) : evalIntGreaterSubj tuplesToCheck whatToCheck
                                                            | otherwise = ((s,p,o), False) : evalIntGreaterSubj tuplesToCheck whatToCheck
                                    where number = read s - read whatToCheck



evalIntGreaterPred :: [(a, String, c)] -> String -> [((a, String, c), Bool)]
evalIntGreaterPred [] _ = []
evalIntGreaterPred ((s,p,o) : tuplesToCheck) whatToCheck    | null tuplesToCheck && 0 < number = [((s,p,o),True)]
                                                            | null tuplesToCheck && not (0 < number) = [((s,p,o),False)]
                                                            | 0 < number = ((s,p,o),True) : evalIntGreaterPred tuplesToCheck whatToCheck
                                                            | otherwise = ((s,p,o), False) : evalIntGreaterPred tuplesToCheck whatToCheck
                                    where number = read p - read whatToCheck


evalIntGreaterObj :: [(a, b, String)] -> String -> [((a, b, String), Bool)]
evalIntGreaterObj [] _ = []
evalIntGreaterObj ((s,p,o) : tuplesToCheck) whatToCheck     | null tuplesToCheck && 0 < number = [((s,p,o),True)]
                                                            | null tuplesToCheck && not (0 < number) = [((s,p,o),False)]
                                                            | 0 < number = ((s,p,o),True) : evalIntGreaterObj tuplesToCheck whatToCheck
                                                            | otherwise = ((s,p,o), False) : evalIntGreaterObj tuplesToCheck whatToCheck
                                    where number = read o - read whatToCheck


decideLessOrGreater [] _ = []
decideLessOrGreater tuples w | isInfixOf "subj" (head w) && ((w !! 1) == "<") = evalIntLessSubj tuples (w !! 2)
                             | isInfixOf "pred" (head w) && ((w !! 1) == "<") = evalIntLessPred tuples (w !! 2)
                             | isInfixOf "obj" (head w) && ((w !! 1) == "<") = evalIntLessObj tuples (w !! 2)
                             | isInfixOf "subj" (head w) && ((w !! 1) == ">") = evalIntGreaterSubj tuples (w !! 2)
                             | isInfixOf "pred" (head w) && ((w !! 1) == ">") = evalIntGreaterPred tuples (w !! 2)
                             | isInfixOf "obj" (head w) && ((w !! 1) == ">") = evalIntGreaterObj tuples (w !! 2)



--this will calculate every single condition into a separate file and than we can check it later

evalConditionFinal [[]] tuples fileNames = [[((s,p,o), True) | (s,p,o) <- tuples]]
evalConditionFinal whatToCheck [] fileName = []
evalConditionFinal (w:whatToCheck) tuples fileNames
                                          | null whatToCheck && (checkIfItsAValidFileName (head w) fileNames) && isInfixOf "subj" (head w) && (w !! 1) == "==" && (not (checkIfSecondArgumentFile fileNames (w !! 2)))
                        = [evalStringSubj tuples (w !! 2)]
                                          | null whatToCheck && (checkIfItsAValidFileName (head w) fileNames) && isInfixOf "pred" (head w) && (w !! 1) == "==" && (not (checkIfSecondArgumentFile fileNames (w !! 2)))
                        = [evalStringPred tuples (w !! 2)]
                                          | null whatToCheck && (checkIfItsAValidFileName (head w) fileNames) && isInfixOf "obj" (head w) && (w !! 1) == "==" && (not (checkIfSecondArgumentFile fileNames (w !! 2)))
                        = [evalStringObj tuples (w !! 2)]
                                          | isInfixOf "subj" (head w) && (checkIfItsAValidFileName (head w) fileNames) && (w !! 1) == "==" && (not (checkIfSecondArgumentFile fileNames (w !! 2)))
                        = evalStringSubj tuples (w !! 2) : evalConditionFinal (tail whatToCheck) tuples fileNames
                                          | isInfixOf "pred" (head w) && (checkIfItsAValidFileName (head w) fileNames) && (w !! 1) == "==" && (not (checkIfSecondArgumentFile fileNames  (w !! 2)))
                        = evalStringPred tuples (w !! 2) : evalConditionFinal (tail whatToCheck) tuples fileNames
                                          | isInfixOf "obj" (head w) && (checkIfItsAValidFileName (head w) fileNames) && (w !! 1) == "==" && (not (checkIfSecondArgumentFile fileNames (w !! 2)))
                        = evalStringObj tuples (w !! 2) : evalConditionFinal (tail whatToCheck) tuples fileNames
                                          | null whatToCheck && (checkIfItsAValidFileName (head w) fileNames) && (((w !! 1) == "<") || ((w !! 1) == ">")) && (not (checkIfSecondArgumentFile fileNames (w !! 2)))
                        = [decideLessOrGreater tuples w]
                                          | (((w !! 1) == "<") || ((w !! 1) == ">")) && (checkIfItsAValidFileName (head w) fileNames) && (not (checkIfSecondArgumentFile fileNames (w !! 2)))
                        = decideLessOrGreater tuples w : evalConditionFinal (tail whatToCheck) tuples fileNames
                                          | null whatToCheck && (checkIfItsAValidFileName (head w) fileNames) && checkIfSecondArgumentFile fileNames (w !! 2) = [addTrueAndFalse tuples (unsafePerformIO (handle_absolute_values w)) [] (length tuples)]
                                          | checkIfSecondArgumentFile fileNames (w !! 2) && (checkIfItsAValidFileName (head w) fileNames) =  addTrueAndFalse tuples (unsafePerformIO (handle_absolute_values w)) [] (length tuples) : evalConditionFinal (tail whatToCheck) tuples fileNames
                                          | null fileNames = error "No file was given as an argument"
                                          | otherwise = error "Error at evalConditionFinal"

evalConditionFinal _ _ _ = error "Error at evalConditionfinal"

checkIfItsAValidFileName _ [] = False
checkIfItsAValidFileName whatToCheck (f:fileNames) | null fileNames && fileToSearch == f = True 
                                                   | null fileNames = error ("File " ++ fileToSearch ++ " was not imported")
                                                   | fileToSearch == f = True
                                                   | otherwise = checkIfItsAValidFileName whatToCheck fileNames
                            where fileToSearch = head (split '.' whatToCheck)


-- v is handle_absolute_values
--addTrueAndFalse :: (Foldable t, Eq a) =>[a] -> t a -> [(a, Bool)] -> Int -> [(a, Bool)]
addTrueAndFalse _  _ res (0) = res
addTrueAndFalse all_3tuples true_3tuples res i = addTrueAndFalse all_3tuples true_3tuples (res ++ (foo all_3tuples true_3tuples (i - 1))) (i - 1)
    where
        foo all_3tuples true_3tuples i = [(all_3tuples !! i, elem (all_3tuples !! i) true_3tuples)]
--evaluates if an or was detected
--evaluatedTuples are evalConditionFinal
evalConditionOr [] = []
evalConditionOr evaluatedTuples | length (head evaluatedTuples) > 1 = evalConditionOr' oneLine : evalConditionOr (map tail evaluatedTuples)
                                | otherwise = [evalConditionOr' oneLine]
        where oneLine = map head evaluatedTuples

evalConditionOr' [] = error "No fileContent was found"
evalConditionOr' (((s,p,o), b) : oneLine) | null oneLine && b = ((s,p,o), True)
                                          | null oneLine && (not b) = ((s,p,o), False)
                                          | b = ((s,p,o), True)
                                          | otherwise = evalConditionOr' oneLine

--evaluates if an and was detected
evalConditionAnd [] = []
evalConditionAnd evaluatedTuples | length (head evaluatedTuples) > 1 = evalConditionAnd' oneLine : evalConditionAnd (map tail evaluatedTuples)
                                 | otherwise = [evalConditionAnd' oneLine]
        where oneLine = map head evaluatedTuples

evalConditionAnd' [] = error "No file content was found"
evalConditionAnd' (((s,p,o), b) : oneLine) | null oneLine && (not b) = ((s,p,o), False)
                                           | null oneLine && b = ((s,p,o), True)
                                           | not b = ((s,p,o), False)
                                           | otherwise = evalConditionAnd' oneLine

--[["foo.subj","==","alma"],["&&"],["foo.obj","==","alma"]] -  
--problem here with types
--decideIfAndOr [] tuples fileNames = []
--decideIfAndOr [] tuples fileNames = evalConditionFinal [] tuples fileNames
decideIfAndOr _ _ [] = []
--decideIfAndOr _ [[]] _ = []
decideIfAndOr conditionList tuples fileNames | null tuples = []
                                             | length conditionList == 1 = evalConditionFinal conditionList tuples fileNames
                                             | head (conditionList !! 1) == "||" = [evalConditionOr $ evalConditionFinal conditionList tuples fileNames]
                                             | head (conditionList !! 1) == "&&" = [evalConditionAnd $ evalConditionFinal conditionList tuples fileNames]
decideIfAndOr _ _ _ = error "Error at decideIfAndOr"

--decideIfAndOr conditionList tuples fileNames = [evalConditionFinal conditionList t fileNames | t <- tuples]                              





split :: Char -> String -> [String]
split c xs = case break (==c) xs of
    (ls, "") -> [ls]
    (ls, x:rs) -> ls : split c rs


--OUT
--example output to handle [(("a", "b", "4"), True), (("d","e","4"), True), (("g","h","i"), False)]

--expected output to handle [[((String, String, String), true)]]
--e.g input (ExpTwo (ExpOne ExpSame (ExpPlusNeeded "1") ExpSame) (ExpOne ExpSame ExpSame (ExpNoPlusSingle "alma")))
-- will give the input ["same","+1","same","same","same","alma"]

--changeToStringOut transforms an Outlist to a list of Strings each expression separated by a ;
changeToStringOut :: OutList -> [String]
changeToStringOut (ExpTwo a b) = (changeToStringOut a) ++ (changeToStringOut b)
changeToStringOut (ExpOne a b c) = (changeToStringOut' a) ++ (changeToStringOut' b) ++ (changeToStringOut' c)
changeToStringOut (ExpNothing ) = []
changeToStringOut (ExpEverything ) = ["same","same","same"]

--handle PLusNeeded data
changeToStringOut' :: PlusNeeded -> [String]
changeToStringOut' (ExpNoPlusSingle a) = [a]
changeToStringOut' (ExpNoPlusDouble a b) = a : [b]
changeToStringOut' (ExpPlusNeeded c) =  [("+" ++ c)]
changeToStringOut' (ExpSame ) = ["same"]
changeToStringOut' (ExpMinusNeeded a) = [("-" ++ a)]
changeToStringOut' (ExpStringConditionOutlist a) = [(evalCondition' a)]
changeToStringOut' (ExpPower a) = [("^" ++ a)]
changeToStringOut' (ExpMultiplication a) = [("*" ++ a)]
changeToStringOut' (ExpDivNeeded a) = [("/" ++ a)]


--QueryJudgement (ExpRelation "foo" "ttl") (ExpGreater (ExpDouble "foo" "obj") (ExpEq (ExpAnd (ExpStringCondition (ExpMinus (ExpLast "1"))) (ExpDouble "foo" "obj")) (ExpStringCondition (ExpLast "100")))) (ExpTwo (ExpOne ExpSame ExpSame (ExpPlusNeeded "1")) (ExpOne ExpSame (ExpNoPlusSingle "true") (ExpNoPlusSingle "true"))) (ExpOne ExpSame ExpSame (ExpNoPlusSingle "true"))
--evalFinal

--where tripleList is Condition output in the form of [((pred,subj,obj),True)]
--trueOutList is what expected as an output if true in form of ["same","alma","+4"]
--falseOutList is what expected as an output if true in form of ["same","alma","+4"]
evalFinalOut tripleList trueOutList falseOutList = evalOutTrue tripleList trueOutList ++ evalOutFalse tripleList falseOutList



--evaluates the true ones
-- tripleList is in the form of [((pred, subj, obj), true)] and the outList is in the form of ["same","+1","same"]
evalOutTrue :: [(([Char], [Char], [Char]), Bool)] -> [String] -> [([Char], [Char], [Char])]
evalOutTrue tripleList outList = isOneList [writeOutHelp' (fst x) outList | x <- tripleList, snd x]

--evaluates the false ones
-- tripleList is in the form of [((pred, subj, obj), true)] and the outList is in the form of ["same","+1","same"]
evalOutFalse :: [(([Char], [Char], [Char]), Bool)] -> [String] -> [([Char], [Char], [Char])]
evalOutFalse tripleList outList = isOneList [writeOutHelp' (fst x) outList | x <- tripleList, not (snd x)]


--handle if more than two (pred,subj,obj) need to be written
writeOutHelp' :: ([Char], [Char], [Char]) -> [String] -> [([Char], [Char], [Char])]
writeOutHelp' _ [] = []
writeOutHelp' triple outList | length outList > 3 = writeOutHelp triple (take 3 outList) : writeOutHelp' triple (drop 3 outList)
                             | length outList == 3 = [writeOutHelp triple outList]
                             | otherwise = error "Error in writeOutHelp'"


--Handle the triple and match it with the expected output
writeOutHelp :: ([Char], [Char], [Char]) -> [String] -> ([Char], [Char], [Char])
writeOutHelp triple outList = (subj, pred, obj)
        where subj = writeSubj triple (head outList)
              newList = tail outList
              pred = writePred triple (head newList)
              newList' = tail newList
              obj = writeObj triple (head newList')


--decides what to write in the place of the subj where first argument is the triple and second argument is what to do with it
writeSubj :: ([Char], b, c) -> String -> [Char]
writeSubj (x, _ , _) outListOne | outListOne == "same" = x
                                | isIntWithPlus outListOne && isInt x = show (read x + (read $ tail outListOne))
                                | isIntWithMinus outListOne && isInt x = show (read x - (read $ tail outListOne))
                                | isIntWithMultiplication outListOne && isInt x = show (read x * (read $ tail outListOne))
                                | isIntWithDivision outListOne && isInt x = show ((read x) / (read $ tail outListOne))
                                | isIntWithPower outListOne && isInt x = show ((read x) ^ (read $ tail outListOne)) 
                                | otherwise = outListOne

--decide what to write in the place of the pred
writePred :: (a, [Char], c) -> String -> [Char]
writePred (_,x,_) outListOne | outListOne == "same" = x
                             | isIntWithPlus outListOne && isInt x = show (read x + (read $ tail outListOne))
                             | isIntWithMinus outListOne && isInt x = show (read x - (read $ tail outListOne))
                             | isIntWithMultiplication outListOne && isInt x = show (read x * (read $ tail outListOne))
                             | isIntWithDivision outListOne && isInt x = show ((read x) / (read $ tail outListOne))
                             | isIntWithPower outListOne && isInt x = show ((read x) ^ (read $ tail outListOne)) 
                             | otherwise = outListOne

--decide what to write in the place of the obj
writeObj :: (a, b, [Char]) -> String -> [Char]
writeObj (_,_,x) outListOne | outListOne == "same" = x
                            | isIntWithPlus outListOne && isInt x = show (read x + (read $ tail outListOne))
                            | isIntWithMinus outListOne && isInt x = show (read x - (read $ tail outListOne))
                            | isIntWithMultiplication outListOne && isInt x = show (read x * (read $ tail outListOne))
                            | isIntWithDivision outListOne && isInt x = show ((read x) / (read $ tail outListOne))
                            | isIntWithPower outListOne && isInt x = show ((read x) ^ (read $ tail outListOne)) 
                            | otherwise = outListOne



--checks if the input is an int
isInt :: [Char] -> Bool
isInt (x:xs)    | null xs && isDigit x = True
                | null xs = False
                | isDigit x = isInt xs
                | not (isDigit x) = False
isInt _ = False

--checks if it has a plus sign
isIntWithPlus :: [Char] -> Bool
isIntWithPlus [] = False
isIntWithPlus (x:xs) | x == '+' = isInt xs
                     | null xs = False
                     | otherwise = False

isIntWithMinus [] = False
isIntWithMinus (x:xs) | x == '-' = isInt xs
                      | otherwise = False

isIntWithMultiplication [] = False
isIntWithMultiplication (x:xs)  | x == '*' = isInt xs
                                | otherwise = False

isIntWithDivision [] = False
isIntWithDivision (x:xs)    | x == '/' = isInt xs
                            | otherwise = False

isIntWithPower [] = False
isIntWithPower (x:xs) | x == '^' = isInt xs
                      | otherwise = False
-- transforms it into a single list
isOneList :: [[a]] -> [a]
isOneList [] = []
isOneList (x:xs) | null xs = x
                 | otherwise = x ++ isOneList xs



--works if condition is only from one file need to make it so that it works from both files
-- maybe add anothey QueryJudgement with a special sign

--toTtlFormat [] = []
--toTtlFormat ((s,p,o):xs) | null xs && isInfixOf "<" o = [concat [s,p,o," ."]]
--                         | null xs = [concat [s,p," ",o," ."]]
--                        | null xs && isInfixOf "\"" o = [concat [s,p,o," ."]]
--                         | isInfixOf "\"" o = concat [s,p,o," ."] : toTtlFormat xs
--                         | isInfixOf "<" o = concat [s,p,o," ."] : toTtlFormat xs
--                         | otherwise = concat [s,p," ",o," ."] : toTtlFormat xs
