{
module Grammar where
import Token
}

%name parse
%tokentype { Token }
%error { parseError }
%token
  where       { TokenWhere _ }
  out         { TokenOut _ }
  else        { TokenElse _ }
  '*'         { TokenStar _ }
  '.'         { TokenDot _ }
  from        { TokenFrom _ }
  other       { TokenOther _ }
  AND         { TokenAnd _ }
  '='         { TokenEq _ }
  '('         { TokenLParen _ }
  ','         { TokenComma _ }
  ')'         { TokenRParen _ }
  '<'         { TokenLessThan _ }
  '>'         { TokenGreaterThan _ }
  '+'         { TokenPlus _ }
  '_'         { TokenUnderScore _ }
  '-'         { TokenMinus _ }
  '#'         { TokenHashtag _ }
  '/'         { TokenSlash _ }
  '^'         { TokenPower _ }
  '"'         { TokenString _ }
  ':'         { TokenTwoDots _ }
  OR          { TokenOr _ }
  varRelation { TokenVarRelation _ $$ }

%nonassoc '(' ')'  ',' '.' '<' '>' '+'
%left OR AND '='

%%

Query : from '(' FromList ')' where '(' Condition ')' out '(' OutList ')' else '(' OutList ')' other Query { QueryJudgementMore $3 $7 $11 $15 $18 }
        | from '(' FromList ')' where '(' Condition ')' out '(' OutList ')' else '(' OutList ')' {QueryJudgement $3 $7 $11 $15}

FromList : varRelation '.' varRelation           { ExpRelation $1 $3 }
         | varRelation '.' varRelation ',' FromList                 { ExpFromList $1 $3 $5 }

Condition :
           Condition '<' '"' StringCondition '"'          { ExpLess $1 $4}
          | Condition '>' '"' StringCondition '"'         { ExpGreater $1 $4}
          | Condition '=' Condition                       { ExpEqFile $1 $3} 
          | Condition AND Condition                       { ExpAnd $1 $3 }
          | Condition OR Condition                        { ExpOr $1 $3 }
          | varRelation '.' varRelation                   { ExpDouble $1 $3}          
          | varRelation                                   { ExpSingle $1}
          |'.'                                            { ExpNoCondition }
          | '"' StringCondition '"'                       { ExpStringCondition $2 }
          


StringCondition: '/' StringCondition        { ExpSlash $2}
          | '#' StringCondition             { ExpHashTag $2}
          | ':' StringCondition             { ExpTwoDots $2}
          | '.' StringCondition             { ExpDot $2}
          | '-' StringCondition             { ExpMinus $2}
          | '<' StringCondition             { ExpLessStringCondition $2}
          | '>'                             { ExpGreatStringConditiond} 
          | varRelation StringCondition     { ExpString $1 $2}
          | varRelation                     { ExpLast $1}
          
OutList :   '*'                                                                                     { ExpEverything } 
          | '.'                                                                                     { ExpNothing }
          | PlusNeeded PlusNeeded PlusNeeded                                                        { ExpOne $1 $2 $3}
          | OutList ',' OutList                                                                     { ExpTwo $1 $3}
          
          
PlusNeeded : '"' StringCondition '"'                        { ExpStringConditionOutlist $2}
            | '+' varRelation                               { ExpPlusNeeded $2}
            | '-' varRelation                               { ExpMinusNeeded $2}
            | '/' varRelation                               { ExpDivNeeded $2}
            | '*' varRelation                               { ExpMultiplication $2}
            | '^' varRelation                               { ExpPower $2}
            | varRelation '.' varRelation                   { ExpNoPlusDouble $1 $3}
            | varRelation                                   { ExpNoPlusSingle $1}
            | '_'                                           { ExpSame }
            
{

data Query = QueryJudgement FromList Condition OutList OutList
            | QueryJudgementMore FromList Condition OutList OutList Query
             deriving Show

data StringCondition = ExpSlash StringCondition
              | ExpHashTag StringCondition
              | ExpTwoDots StringCondition
              | ExpDot StringCondition
              | ExpString String StringCondition
              | ExpLast String
              | ExpMinus StringCondition
              | ExpLessStringCondition StringCondition
              | ExpGreatStringConditiond 
            deriving Show

data PlusNeeded = ExpPlusNeeded String
              | ExpNoPlusDouble String String
              | ExpNoPlusSingle String
              | ExpSame
              | ExpStringConditionOutlist StringCondition
              | ExpDivNeeded String
              | ExpMultiplication String
              | ExpPower String
              | ExpMinusNeeded String
             deriving Show

data FromList = ExpRelation String String
              | ExpFromList String String FromList
             deriving Show

data Condition = ExpAnd Condition Condition
              | ExpOr Condition Condition
              | ExpLess Condition StringCondition
              | ExpGreater Condition StringCondition
              | ExpDouble String String
              | ExpSingle String
              | ExpNoCondition
              | ExpStringCondition StringCondition
              | ExpEqFile Condition Condition
            deriving Show

data OutList = ExpOne PlusNeeded PlusNeeded PlusNeeded
              | ExpTwo OutList OutList
              | ExpEverything
              | ExpNothing
            deriving Show


parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:ts) = error ("Parsing error at line:column " ++ (tokenPosn t) ++ " with token " ++ (show t))

}
