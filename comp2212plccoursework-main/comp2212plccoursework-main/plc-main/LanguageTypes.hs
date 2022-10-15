module LanguageTypes where
import Grammar
import Eval


--QueryJudgement (ExpRelation "foo" "ttl") (ExpEqFile (ExpAnd (ExpEqFile (ExpDouble "foo" "ubj") (ExpStringCondition (ExpLessStringCondition (ExpString "http" (ExpTwoDots (ExpSlash (ExpSlash (ExpString "www" (ExpDot (ExpString "cw" (ExpDot (ExpString "org" (ExpSlash (ExpHashTag (ExpString "problem2" ExpGreatStringConditiond))))))))))))))) (ExpDouble "foo" "obj")) (ExpStringCondition (ExpLast "true"))) ExpEverything ExpNothing

typeCheck (QueryJudgementMore fromList condition ifOutList elseOutList query) | (typeCheck (QueryJudgement fromList condition ifOutList elseOutList) == "The query passed the type checking") = typeCheck (query)
                                                                              | otherwise = "Not good"


typeCheck (QueryJudgement fromList condition ifOutList elseOutList) | checkCondition conditionList fileNames  
                        && checkIfOutList trueOutList && checkIfOutList falseOutList 
                        && checkFromList files= "The query passed the type checking"
                                                                    | otherwise = "Not Good"
                        where conditionList = evalCondition condition
                              fileNames = getFileNames $ readFiles fromList
                              trueOutList = changeToStringOut ifOutList
                              falseOutList = changeToStringOut elseOutList
                              files = readFiles fromList

--Checking the input
checkFromList (f:files) | length splitted > 1 && not(splitted !! 1 == "ttl") = error ("Wrong file extension it should be .ttl not " ++ splitted !! 1)
                        | not (null files) = checkFromList files
                        | otherwise = True
          where splitted = split '.' f
--Checks the condition part
checkCondition [] _ = True
checkCondition conditionList fileNames  | length conditionList == 3 = checkCondition' conditionList fileNames
                                        | length conditionList > 3 && checkCondition' (take 3 conditionList) fileNames  && checkIfItsAndOr (conditionList !! 3) = checkCondition (drop 4 conditionList) fileNames
                                        | otherwise = checkCondition' conditionList fileNames


checkCondition' conditionList fileNames | checkIfItsAValidFileName' first fileNames && checkIfSubjPredObj first
                                        && checkIfSecondArgumentIsOperation second = True
                                        | otherwise = False                                     
          where first = head conditionList
                second = conditionList !! 1
                third = conditionList !! 2
                                                              


--Checks it the expression has subj pred or obj in it
checkIfSubjPredObj whatToCheck | checkThis == "subj" || checkThis == "pred" || checkThis == "obj" = True
                               | otherwise = error ("No subj/pred/obj was found at " ++ whatToCheck ++ ", perhaps you ment to write " ++ fileName ++".subj or " ++ fileName ++ ".pred or " ++ fileName ++ ".obj")
                where fileName  = head (split '.' whatToCheck)
                      checkThis = (split '.' whatToCheck) !! 1


checkIfItsAValidFileName' whatToCheck [] = error ("File " ++ fileToSearch ++ " was not imported")
                  where fileToSearch = head (split '.' whatToCheck)
checkIfItsAValidFileName' whatToCheck (f:fileNames) | null fileNames && fileToSearch == f = True
                                                   | null fileNames = error ("File " ++ fileToSearch ++ " was not imported")
                                                   | fileToSearch == f = True
                                                   | otherwise = checkIfItsAValidFileName' whatToCheck fileNames
                            where fileToSearch = head (split '.' whatToCheck)

checkIfSecondArgumentIsOperation whatToCheck | whatToCheck == "==" || whatToCheck == "<" || whatToCheck == ">" = True
                                             | otherwise = error ("The " ++ whatToCheck ++ " operator is not supported please use one of the following: =, <, >")


checkIfItsAndOr whatToCheck | whatToCheck == "&&" || whatToCheck == "||" = True
                            | otherwise = error (whatToCheck ++ " is not supported please use AND or OR")

--["same", "same", "+1"]
--checkIfOutList :: [String] -> Bool
checkIfOutList [] = True
checkIfOutList trueOutList  | length trueOutList == 3 && length subj > 1 && isInt (tail subj) = error ("Subject does not support " ++ subj ++ " format")
                            | length trueOutList == 3 && length pred > 1 && isInt (tail pred) = error ("Predicate does not support " ++ pred ++ " format")
                            | length trueOutList == 3 && (operator == '+' || operator == '-' || operator == '/' || operator == '*' || operator == '^') = checkIfRestIsInt (tail obj)
                            | checkIfOutList' (take 3 trueOutList) = checkIfOutList (drop 3 trueOutList)
                            | otherwise = True

            where subj = head trueOutList
                  pred = trueOutList !! 1
                  obj = trueOutList !! 2
                  operator = head obj
                  

checkIfRestIsInt whatToCheck | isInt whatToCheck = True
                             | otherwise = error ("In the output after an operator you must have an operator followed by an integer")


checkIfOutList' trueOutList | length trueOutList == 3 && length subj > 1 && isInt (tail subj) = error ("Subject does not support " ++ subj ++ " format")
                            | length trueOutList == 3 && length pred > 1 && isInt (tail pred) = error ("Predicate does not support " ++ pred ++ " format")
                            | length trueOutList == 3 && (operator == '+' || operator == '-' || operator == '/' || operator == '*' || operator == '^') = checkIfRestIsInt (tail obj)
                            | otherwise = True

                where subj = head trueOutList
                      pred = trueOutList !! 1
                      obj = trueOutList !! 2
                      operator = head obj


