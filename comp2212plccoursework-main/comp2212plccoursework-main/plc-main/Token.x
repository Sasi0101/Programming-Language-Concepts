{
module Token where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+ ;
  "--".*  ;
  from                                      { tok (\p s -> TokenFrom p) }
  out                                       { tok (\p s -> TokenOut p) }
  else                                      { tok (\p s -> TokenElse p) }
  \.                                        { tok (\p s -> TokenDot p) }
  \*                                        { tok (\p s -> TokenStar p) }
  where                                     { tok (\p s -> TokenWhere p) }
  AND                                       { tok (\p s -> TokenAnd p) }
  other                                     { tok (\p s -> TokenOther p) }
  OR                                        { tok (\p s -> TokenOr p) }
  \=                                        { tok (\p s -> TokenEq p) }
  \,                                        { tok (\p s -> TokenComma p)}
  \(                                        { tok (\p s -> TokenLParen p) }
  \)                                        { tok (\p s -> TokenRParen p) }
  \<                                        { tok (\p s -> TokenLessThan p) }
  \>                                        { tok (\p s -> TokenGreaterThan p) }
  \+                                        { tok (\p s -> TokenPlus p) }
  \"                                        { tok (\p s -> TokenString p) }
  \/                                        { tok (\p s -> TokenSlash p)}
  \:                                        { tok (\p s -> TokenTwoDots p) }
  \#                                        { tok (\p s -> TokenHashtag p) }
  \_                                        { tok (\p s -> TokenUnderScore p) }
  \-                                        { tok (\p s -> TokenMinus p) }
  \^                                        { tok (\p s -> TokenPower p)}
  exists                                    { tok (\p s -> TokenExists p) }
  \_ [$digit] [$digit]*                     { tok (\p s -> TokenVarSkip p (read (tail s) :: Int)) }
  [$alpha $digit] [$alpha $digit \_ \’]*    { tok (\p s -> TokenVarRelation p s) }

{
-- Each action has type :: AlexPosn -> String -> Token 
tok f p s = f p s

data Token =
  TokenDot AlexPosn                   |
  TokenString AlexPosn                |
  TokenSlash AlexPosn                 |
  TokenTwoDots AlexPosn               |
  TokenStar AlexPosn                  |
  TokenHashtag AlexPosn               |
  TokenElse AlexPosn                  |
  TokenOut AlexPosn                   |
  TokenOther AlexPosn                 |
  TokenWhere AlexPosn                 |
  TokenPower AlexPosn                 |
  TokenAnd AlexPosn                   |
  TokenEq AlexPosn                    |
  TokenLParen AlexPosn                |
  TokenRParen AlexPosn                |
  TokenMinus AlexPosn                 |
  TokenExists AlexPosn                |
  TokenOr AlexPosn                    |
  TokenFrom AlexPosn                  |
  TokenLessThan AlexPosn              |
  TokenGreaterThan AlexPosn           |
  TokenVarSkip AlexPosn Int           |
  TokenPlus AlexPosn                  |
  TokenUnderScore AlexPosn            |
  TokenComma AlexPosn                 |
  TokenVarRelation AlexPosn String
  deriving (Eq,Show)

tokenPosn :: Token -> String
tokenPosn (TokenWhere (AlexPn a l c))             = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c))               = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c))                = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c))            = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c))            = show(l) ++ ":" ++ show(c)
tokenPosn (TokenExists (AlexPn a l c))            = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFrom (AlexPn a l c))              = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVarSkip (AlexPn a l c) _)         = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVarRelation (AlexPn a l c) _)     = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c))             = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOut (AlexPn a l c))               = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c))              = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDot (AlexPn a l c))               = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStar (AlexPn a l c))              = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c))                = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn a l c))          = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThan (AlexPn a l c))       = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c))              = show(l) ++ ":" ++ show(c)
tokenPosn (TokenUnderScore (AlexPn a l c))        = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c))            = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSlash (AlexPn a l c))             = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTwoDots (AlexPn a l c))           = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHashtag (AlexPn a l c))           = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c))             = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOther (AlexPn a l c))             = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPower (AlexPn a l c))             = show(l) ++ ":" ++ show(c)

}
