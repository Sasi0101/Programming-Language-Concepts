{-# OPTIONS_GHC -w #-}
module Grammar where
import Token
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,152) ([16384,0,0,1,0,64,0,0,0,1024,0,0,1024,0,4096,0,4,8192,0,0,64,8192,0,0,0,64,2,0,4096,0,32768,0,0,1,8192,8192,1,0,4,2,18,7360,32,0,0,32896,1337,512,0,0,0,0,32883,32768,32768,4,2,18,1,0,0,32,0,128,512,4608,0,16384,0,8192,32768,14720,5,58882,20,0,0,24608,334,32896,1337,512,5350,2048,21400,8192,20064,4097,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32896,1337,512,5350,0,1,0,0,0,0,0,47107,19,0,8,0,32,192,1262,32768,1,1024,20192,0,0,1,0,0,0,16,0,0,0,256,0,1024,0,4096,2048,21400,8192,0,0,0,0,0,0,1536,0,32816,315,32,0,0,4096,0,2048,0,0,0,0,0,0,0,0,0,0,0,64,1262,0,4096,512,0,0,4,0,0,0,0,0,0,0,1024,0,0,0,768,5048,3072,20192,0,24,0,96,0,8,0,32,0,64,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Query","FromList","Condition","StringCondition","OutList","PlusNeeded","where","out","else","'*'","'.'","from","other","AND","'='","'('","','","')'","'<'","'>'","'+'","'_'","'-'","'#'","'/'","'^'","'\"'","':'","OR","varRelation","%eof"]
        bit_start = st Prelude.* 34
        bit_end = (st Prelude.+ 1) Prelude.* 34
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..33]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (15) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (15) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (19) = happyShift action_6
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (34) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (19) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (33) = happyShift action_8
action_5 (5) = happyGoto action_9
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (33) = happyShift action_8
action_6 (5) = happyGoto action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (21) = happyShift action_12
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (14) = happyShift action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (21) = happyShift action_10
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (10) = happyShift action_15
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (33) = happyShift action_14
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (10) = happyShift action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (19) = happyShift action_18
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (20) = happyShift action_17
action_14 _ = happyReduce_3

action_15 (19) = happyShift action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (14) = happyShift action_20
action_16 (30) = happyShift action_21
action_16 (33) = happyShift action_22
action_16 (6) = happyGoto action_24
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (33) = happyShift action_8
action_17 (5) = happyGoto action_23
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (14) = happyShift action_20
action_18 (30) = happyShift action_21
action_18 (33) = happyShift action_22
action_18 (6) = happyGoto action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (17) = happyShift action_25
action_19 (18) = happyShift action_26
action_19 (21) = happyShift action_41
action_19 (22) = happyShift action_28
action_19 (23) = happyShift action_29
action_19 (32) = happyShift action_30
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_12

action_21 (14) = happyShift action_33
action_21 (22) = happyShift action_34
action_21 (23) = happyShift action_35
action_21 (26) = happyShift action_36
action_21 (27) = happyShift action_37
action_21 (28) = happyShift action_38
action_21 (31) = happyShift action_39
action_21 (33) = happyShift action_40
action_21 (7) = happyGoto action_32
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (14) = happyShift action_31
action_22 _ = happyReduce_11

action_23 _ = happyReduce_4

action_24 (17) = happyShift action_25
action_24 (18) = happyShift action_26
action_24 (21) = happyShift action_27
action_24 (22) = happyShift action_28
action_24 (23) = happyShift action_29
action_24 (32) = happyShift action_30
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (14) = happyShift action_20
action_25 (30) = happyShift action_21
action_25 (33) = happyShift action_22
action_25 (6) = happyGoto action_57
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (14) = happyShift action_20
action_26 (30) = happyShift action_21
action_26 (33) = happyShift action_22
action_26 (6) = happyGoto action_56
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (11) = happyShift action_55
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (30) = happyShift action_54
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (30) = happyShift action_53
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (14) = happyShift action_20
action_30 (30) = happyShift action_21
action_30 (33) = happyShift action_22
action_30 (6) = happyGoto action_52
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (33) = happyShift action_51
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (30) = happyShift action_50
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (14) = happyShift action_33
action_33 (22) = happyShift action_34
action_33 (23) = happyShift action_35
action_33 (26) = happyShift action_36
action_33 (27) = happyShift action_37
action_33 (28) = happyShift action_38
action_33 (31) = happyShift action_39
action_33 (33) = happyShift action_40
action_33 (7) = happyGoto action_49
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (14) = happyShift action_33
action_34 (22) = happyShift action_34
action_34 (23) = happyShift action_35
action_34 (26) = happyShift action_36
action_34 (27) = happyShift action_37
action_34 (28) = happyShift action_38
action_34 (31) = happyShift action_39
action_34 (33) = happyShift action_40
action_34 (7) = happyGoto action_48
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_20

action_36 (14) = happyShift action_33
action_36 (22) = happyShift action_34
action_36 (23) = happyShift action_35
action_36 (26) = happyShift action_36
action_36 (27) = happyShift action_37
action_36 (28) = happyShift action_38
action_36 (31) = happyShift action_39
action_36 (33) = happyShift action_40
action_36 (7) = happyGoto action_47
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (14) = happyShift action_33
action_37 (22) = happyShift action_34
action_37 (23) = happyShift action_35
action_37 (26) = happyShift action_36
action_37 (27) = happyShift action_37
action_37 (28) = happyShift action_38
action_37 (31) = happyShift action_39
action_37 (33) = happyShift action_40
action_37 (7) = happyGoto action_46
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (14) = happyShift action_33
action_38 (22) = happyShift action_34
action_38 (23) = happyShift action_35
action_38 (26) = happyShift action_36
action_38 (27) = happyShift action_37
action_38 (28) = happyShift action_38
action_38 (31) = happyShift action_39
action_38 (33) = happyShift action_40
action_38 (7) = happyGoto action_45
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (14) = happyShift action_33
action_39 (22) = happyShift action_34
action_39 (23) = happyShift action_35
action_39 (26) = happyShift action_36
action_39 (27) = happyShift action_37
action_39 (28) = happyShift action_38
action_39 (31) = happyShift action_39
action_39 (33) = happyShift action_40
action_39 (7) = happyGoto action_44
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (14) = happyShift action_33
action_40 (22) = happyShift action_34
action_40 (23) = happyShift action_35
action_40 (26) = happyShift action_36
action_40 (27) = happyShift action_37
action_40 (28) = happyShift action_38
action_40 (31) = happyShift action_39
action_40 (33) = happyShift action_40
action_40 (7) = happyGoto action_43
action_40 _ = happyReduce_22

action_41 (11) = happyShift action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (19) = happyShift action_61
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_21

action_44 _ = happyReduce_16

action_45 _ = happyReduce_14

action_46 _ = happyReduce_15

action_47 _ = happyReduce_18

action_48 _ = happyReduce_19

action_49 _ = happyReduce_17

action_50 _ = happyReduce_13

action_51 _ = happyReduce_10

action_52 _ = happyReduce_9

action_53 (14) = happyShift action_33
action_53 (22) = happyShift action_34
action_53 (23) = happyShift action_35
action_53 (26) = happyShift action_36
action_53 (27) = happyShift action_37
action_53 (28) = happyShift action_38
action_53 (31) = happyShift action_39
action_53 (33) = happyShift action_40
action_53 (7) = happyGoto action_60
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (14) = happyShift action_33
action_54 (22) = happyShift action_34
action_54 (23) = happyShift action_35
action_54 (26) = happyShift action_36
action_54 (27) = happyShift action_37
action_54 (28) = happyShift action_38
action_54 (31) = happyShift action_39
action_54 (33) = happyShift action_40
action_54 (7) = happyGoto action_59
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (19) = happyShift action_58
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_7

action_57 _ = happyReduce_8

action_58 (13) = happyShift action_64
action_58 (14) = happyShift action_65
action_58 (24) = happyShift action_66
action_58 (25) = happyShift action_67
action_58 (26) = happyShift action_68
action_58 (28) = happyShift action_69
action_58 (29) = happyShift action_70
action_58 (30) = happyShift action_71
action_58 (33) = happyShift action_72
action_58 (8) = happyGoto action_75
action_58 (9) = happyGoto action_63
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (30) = happyShift action_74
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (30) = happyShift action_73
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (13) = happyShift action_64
action_61 (14) = happyShift action_65
action_61 (24) = happyShift action_66
action_61 (25) = happyShift action_67
action_61 (26) = happyShift action_68
action_61 (28) = happyShift action_69
action_61 (29) = happyShift action_70
action_61 (30) = happyShift action_71
action_61 (33) = happyShift action_72
action_61 (8) = happyGoto action_62
action_61 (9) = happyGoto action_63
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (20) = happyShift action_76
action_62 (21) = happyShift action_87
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (13) = happyShift action_86
action_63 (24) = happyShift action_66
action_63 (25) = happyShift action_67
action_63 (26) = happyShift action_68
action_63 (28) = happyShift action_69
action_63 (29) = happyShift action_70
action_63 (30) = happyShift action_71
action_63 (33) = happyShift action_72
action_63 (9) = happyGoto action_85
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (33) = happyShift action_84
action_64 _ = happyReduce_23

action_65 _ = happyReduce_24

action_66 (33) = happyShift action_83
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_35

action_68 (33) = happyShift action_82
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (33) = happyShift action_81
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (33) = happyShift action_80
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (14) = happyShift action_33
action_71 (22) = happyShift action_34
action_71 (23) = happyShift action_35
action_71 (26) = happyShift action_36
action_71 (27) = happyShift action_37
action_71 (28) = happyShift action_38
action_71 (31) = happyShift action_39
action_71 (33) = happyShift action_40
action_71 (7) = happyGoto action_79
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (14) = happyShift action_78
action_72 _ = happyReduce_34

action_73 _ = happyReduce_6

action_74 _ = happyReduce_5

action_75 (20) = happyShift action_76
action_75 (21) = happyShift action_77
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (13) = happyShift action_64
action_76 (14) = happyShift action_65
action_76 (24) = happyShift action_66
action_76 (25) = happyShift action_67
action_76 (26) = happyShift action_68
action_76 (28) = happyShift action_69
action_76 (29) = happyShift action_70
action_76 (30) = happyShift action_71
action_76 (33) = happyShift action_72
action_76 (8) = happyGoto action_93
action_76 (9) = happyGoto action_63
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (12) = happyShift action_92
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (33) = happyShift action_91
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (30) = happyShift action_90
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_32

action_81 _ = happyReduce_30

action_82 _ = happyReduce_29

action_83 _ = happyReduce_28

action_84 _ = happyReduce_31

action_85 (13) = happyShift action_86
action_85 (24) = happyShift action_66
action_85 (25) = happyShift action_67
action_85 (26) = happyShift action_68
action_85 (28) = happyShift action_69
action_85 (29) = happyShift action_70
action_85 (30) = happyShift action_71
action_85 (33) = happyShift action_72
action_85 (9) = happyGoto action_89
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (33) = happyShift action_84
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (12) = happyShift action_88
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (19) = happyShift action_95
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_25

action_90 _ = happyReduce_27

action_91 _ = happyReduce_33

action_92 (19) = happyShift action_94
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (20) = happyFail []
action_93 _ = happyReduce_26

action_94 (13) = happyShift action_64
action_94 (14) = happyShift action_65
action_94 (24) = happyShift action_66
action_94 (25) = happyShift action_67
action_94 (26) = happyShift action_68
action_94 (28) = happyShift action_69
action_94 (29) = happyShift action_70
action_94 (30) = happyShift action_71
action_94 (33) = happyShift action_72
action_94 (8) = happyGoto action_97
action_94 (9) = happyGoto action_63
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (13) = happyShift action_64
action_95 (14) = happyShift action_65
action_95 (24) = happyShift action_66
action_95 (25) = happyShift action_67
action_95 (26) = happyShift action_68
action_95 (28) = happyShift action_69
action_95 (29) = happyShift action_70
action_95 (30) = happyShift action_71
action_95 (33) = happyShift action_72
action_95 (8) = happyGoto action_96
action_95 (9) = happyGoto action_63
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (20) = happyShift action_76
action_96 (21) = happyShift action_99
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (20) = happyShift action_76
action_97 (21) = happyShift action_98
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (16) = happyShift action_100
action_98 _ = happyReduce_2

action_99 (16) = happyShift action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (15) = happyShift action_4
action_100 (4) = happyGoto action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_1

happyReduce_1 = happyReduce 18 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_18) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_15) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (QueryJudgementMore happy_var_3 happy_var_7 happy_var_11 happy_var_15 happy_var_18
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 16 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_15) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (QueryJudgement happy_var_3 happy_var_7 happy_var_11 happy_var_15
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyTerminal (TokenVarRelation _ happy_var_3))
	_
	(HappyTerminal (TokenVarRelation _ happy_var_1))
	 =  HappyAbsSyn5
		 (ExpRelation happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 5 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVarRelation _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVarRelation _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExpFromList happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 5 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ExpLess happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ExpGreater happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (ExpEqFile happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (ExpAnd happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (ExpOr happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyTerminal (TokenVarRelation _ happy_var_3))
	_
	(HappyTerminal (TokenVarRelation _ happy_var_1))
	 =  HappyAbsSyn6
		 (ExpDouble happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 (HappyTerminal (TokenVarRelation _ happy_var_1))
	 =  HappyAbsSyn6
		 (ExpSingle happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn6
		 (ExpNoCondition
	)

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (ExpStringCondition happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ExpSlash happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  7 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ExpHashTag happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  7 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ExpTwoDots happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  7 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ExpDot happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  7 happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ExpMinus happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  7 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ExpLessStringCondition happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  7 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn7
		 (ExpGreatStringConditiond
	)

happyReduce_21 = happySpecReduce_2  7 happyReduction_21
happyReduction_21 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenVarRelation _ happy_var_1))
	 =  HappyAbsSyn7
		 (ExpString happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  7 happyReduction_22
happyReduction_22 (HappyTerminal (TokenVarRelation _ happy_var_1))
	 =  HappyAbsSyn7
		 (ExpLast happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  8 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn8
		 (ExpEverything
	)

happyReduce_24 = happySpecReduce_1  8 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn8
		 (ExpNothing
	)

happyReduce_25 = happySpecReduce_3  8 happyReduction_25
happyReduction_25 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (ExpOne happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  8 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (ExpTwo happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  9 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (ExpStringConditionOutlist happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  9 happyReduction_28
happyReduction_28 (HappyTerminal (TokenVarRelation _ happy_var_2))
	_
	 =  HappyAbsSyn9
		 (ExpPlusNeeded happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  9 happyReduction_29
happyReduction_29 (HappyTerminal (TokenVarRelation _ happy_var_2))
	_
	 =  HappyAbsSyn9
		 (ExpMinusNeeded happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  9 happyReduction_30
happyReduction_30 (HappyTerminal (TokenVarRelation _ happy_var_2))
	_
	 =  HappyAbsSyn9
		 (ExpDivNeeded happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  9 happyReduction_31
happyReduction_31 (HappyTerminal (TokenVarRelation _ happy_var_2))
	_
	 =  HappyAbsSyn9
		 (ExpMultiplication happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  9 happyReduction_32
happyReduction_32 (HappyTerminal (TokenVarRelation _ happy_var_2))
	_
	 =  HappyAbsSyn9
		 (ExpPower happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  9 happyReduction_33
happyReduction_33 (HappyTerminal (TokenVarRelation _ happy_var_3))
	_
	(HappyTerminal (TokenVarRelation _ happy_var_1))
	 =  HappyAbsSyn9
		 (ExpNoPlusDouble happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  9 happyReduction_34
happyReduction_34 (HappyTerminal (TokenVarRelation _ happy_var_1))
	 =  HappyAbsSyn9
		 (ExpNoPlusSingle happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  9 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn9
		 (ExpSame
	)

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenWhere _ -> cont 10;
	TokenOut _ -> cont 11;
	TokenElse _ -> cont 12;
	TokenStar _ -> cont 13;
	TokenDot _ -> cont 14;
	TokenFrom _ -> cont 15;
	TokenOther _ -> cont 16;
	TokenAnd _ -> cont 17;
	TokenEq _ -> cont 18;
	TokenLParen _ -> cont 19;
	TokenComma _ -> cont 20;
	TokenRParen _ -> cont 21;
	TokenLessThan _ -> cont 22;
	TokenGreaterThan _ -> cont 23;
	TokenPlus _ -> cont 24;
	TokenUnderScore _ -> cont 25;
	TokenMinus _ -> cont 26;
	TokenHashtag _ -> cont 27;
	TokenSlash _ -> cont 28;
	TokenPower _ -> cont 29;
	TokenString _ -> cont 30;
	TokenTwoDots _ -> cont 31;
	TokenOr _ -> cont 32;
	TokenVarRelation _ happy_dollar_dollar -> cont 33;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 34 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
