-- parser produced by Happy Version 1.2-alpha


module Parser (parse) where
import GenUtils
import Lexer
import AbsSyn
import Types
import ParseMonad

data HappyAbsSyn 
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn1(ProgOrInterface)
	| HappyAbsSyn2([Form])
	| HappyAbsSyn3(BinOp)
	| HappyAbsSyn4(CompOp)
	| HappyAbsSyn6(UnOp)
	| HappyAbsSyn7(Expr)
	| HappyAbsSyn10([ Expr ])
	| HappyAbsSyn18(CaseClause)
	| HappyAbsSyn19([ GuardTest ])
	| HappyAbsSyn20([ CaseClause ])
	| HappyAbsSyn21(IfClause)
	| HappyAbsSyn22([ IfClause ])
	| HappyAbsSyn25(GuardTest)
	| HappyAbsSyn28(FunctionClause)
	| HappyAbsSyn30(Function)
	| HappyAbsSyn31(Attribute)
	| HappyAbsSyn32([ Fun ])
	| HappyAbsSyn34(Fun)
	| HappyAbsSyn35(Form)
	| HappyAbsSyn39((String,Int,[UType]))
	| HappyAbsSyn40([ TyVar ])
	| HappyAbsSyn41((Module, [ Form ]))
	| HappyAbsSyn42([ Form ])
	| HappyAbsSyn44(([Constraint], [VarConstraint]))
	| HappyAbsSyn45([ VarOrTypeCon ])
	| HappyAbsSyn47([ UType ])
	| HappyAbsSyn51(UType)
	| HappyAbsSyn52(TaggedTyVar)
	| HappyAbsSyn53([ PType ])
	| HappyAbsSyn54(PType)
	| HappyAbsSyn55([ Tag ])
	| HappyAbsSyn57(Tag)
	| HappyAbsSyn58(String)

type HappyReduction = 
	   Int 
	-> (Token)
	-> HappyState (Token) ([HappyAbsSyn] -> P(ProgOrInterface))
	-> [HappyState (Token) ([HappyAbsSyn] -> P(ProgOrInterface))] 
	-> [HappyAbsSyn] 
	-> P(ProgOrInterface)

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316 :: Int -> HappyReduction

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164 :: HappyReduction

action_0 (112) = happyShift action_2
action_0 (113) = happyShift action_3
action_0 (1) = happyGoto action_1
action_0 _ = happyFail

action_1 (114) = happyAccept
action_1 _ = happyFail

action_2 (59) = happyShift action_11
action_2 (82) = happyShift action_12
action_2 (83) = happyShift action_13
action_2 (84) = happyShift action_14
action_2 (86) = happyShift action_15
action_2 (2) = happyGoto action_6
action_2 (28) = happyGoto action_7
action_2 (30) = happyGoto action_8
action_2 (35) = happyGoto action_9
action_2 (58) = happyGoto action_10
action_2 _ = happyReduce_3

action_3 (86) = happyShift action_5
action_3 (41) = happyGoto action_4
action_3 _ = happyFail

action_4 _ = happyReduce_2

action_5 (59) = happyShift action_11
action_5 (82) = happyShift action_12
action_5 (83) = happyShift action_13
action_5 (84) = happyShift action_14
action_5 (58) = happyGoto action_23
action_5 _ = happyFail

action_6 _ = happyReduce_1

action_7 (106) = happyShift action_22
action_7 _ = happyReduce_95

action_8 (110) = happyShift action_21
action_8 _ = happyFail

action_9 (59) = happyShift action_11
action_9 (82) = happyShift action_12
action_9 (83) = happyShift action_13
action_9 (84) = happyShift action_14
action_9 (86) = happyShift action_15
action_9 (2) = happyGoto action_20
action_9 (28) = happyGoto action_7
action_9 (30) = happyGoto action_8
action_9 (35) = happyGoto action_9
action_9 (58) = happyGoto action_10
action_9 _ = happyReduce_3

action_10 (101) = happyShift action_19
action_10 _ = happyFail

action_11 _ = happyReduce_161

action_12 _ = happyReduce_162

action_13 _ = happyReduce_163

action_14 _ = happyReduce_164

action_15 (59) = happyShift action_11
action_15 (82) = happyShift action_12
action_15 (83) = happyShift action_17
action_15 (84) = happyShift action_18
action_15 (58) = happyGoto action_16
action_15 _ = happyFail

action_16 (101) = happyShift action_41
action_16 _ = happyFail

action_17 (59) = happyShift action_11
action_17 (82) = happyShift action_12
action_17 (83) = happyShift action_13
action_17 (84) = happyShift action_14
action_17 (37) = happyGoto action_39
action_17 (58) = happyGoto action_40
action_17 _ = happyReduce_163

action_18 (59) = happyShift action_11
action_18 (82) = happyShift action_12
action_18 (83) = happyShift action_13
action_18 (84) = happyShift action_14
action_18 (38) = happyGoto action_37
action_18 (58) = happyGoto action_38
action_18 _ = happyReduce_164

action_19 (59) = happyShift action_11
action_19 (60) = happyShift action_31
action_19 (61) = happyShift action_32
action_19 (62) = happyShift action_33
action_19 (63) = happyShift action_34
action_19 (82) = happyShift action_12
action_19 (83) = happyShift action_13
action_19 (84) = happyShift action_14
action_19 (99) = happyShift action_35
action_19 (103) = happyShift action_36
action_19 (7) = happyGoto action_26
action_19 (8) = happyGoto action_27
action_19 (10) = happyGoto action_28
action_19 (29) = happyGoto action_29
action_19 (58) = happyGoto action_30
action_19 _ = happyReduce_93

action_20 _ = happyReduce_4

action_21 _ = happyReduce_108

action_22 (59) = happyShift action_11
action_22 (82) = happyShift action_12
action_22 (83) = happyShift action_13
action_22 (84) = happyShift action_14
action_22 (28) = happyGoto action_7
action_22 (30) = happyGoto action_25
action_22 (58) = happyGoto action_10
action_22 _ = happyFail

action_23 (101) = happyShift action_24
action_23 _ = happyFail

action_24 (59) = happyShift action_11
action_24 (82) = happyShift action_12
action_24 (83) = happyShift action_13
action_24 (84) = happyShift action_14
action_24 (58) = happyGoto action_57
action_24 _ = happyFail

action_25 _ = happyReduce_96

action_26 _ = happyReduce_32

action_27 (105) = happyShift action_56
action_27 _ = happyReduce_41

action_28 _ = happyReduce_94

action_29 (102) = happyShift action_55
action_29 _ = happyFail

action_30 (103) = happyShift action_54
action_30 _ = happyReduce_27

action_31 _ = happyReduce_31

action_32 _ = happyReduce_28

action_33 _ = happyReduce_29

action_34 _ = happyReduce_30

action_35 (59) = happyShift action_11
action_35 (60) = happyShift action_31
action_35 (61) = happyShift action_32
action_35 (62) = happyShift action_33
action_35 (63) = happyShift action_34
action_35 (82) = happyShift action_12
action_35 (83) = happyShift action_13
action_35 (84) = happyShift action_14
action_35 (99) = happyShift action_35
action_35 (100) = happyShift action_53
action_35 (103) = happyShift action_36
action_35 (7) = happyGoto action_26
action_35 (8) = happyGoto action_52
action_35 (58) = happyGoto action_30
action_35 _ = happyFail

action_36 (59) = happyShift action_11
action_36 (60) = happyShift action_31
action_36 (61) = happyShift action_32
action_36 (62) = happyShift action_33
action_36 (63) = happyShift action_34
action_36 (82) = happyShift action_12
action_36 (83) = happyShift action_13
action_36 (84) = happyShift action_14
action_36 (99) = happyShift action_35
action_36 (103) = happyShift action_36
action_36 (104) = happyShift action_51
action_36 (7) = happyGoto action_26
action_36 (8) = happyGoto action_27
action_36 (10) = happyGoto action_50
action_36 (58) = happyGoto action_30
action_36 _ = happyFail

action_37 (110) = happyShift action_49
action_37 _ = happyFail

action_38 (101) = happyShift action_48
action_38 _ = happyFail

action_39 (110) = happyShift action_47
action_39 _ = happyFail

action_40 (101) = happyShift action_46
action_40 _ = happyFail

action_41 (59) = happyShift action_11
action_41 (60) = happyShift action_31
action_41 (61) = happyShift action_32
action_41 (62) = happyShift action_33
action_41 (63) = happyShift action_34
action_41 (82) = happyShift action_12
action_41 (83) = happyShift action_13
action_41 (84) = happyShift action_14
action_41 (99) = happyShift action_45
action_41 (103) = happyShift action_36
action_41 (7) = happyGoto action_26
action_41 (8) = happyGoto action_42
action_41 (31) = happyGoto action_43
action_41 (58) = happyGoto action_44
action_41 _ = happyFail

action_42 _ = happyReduce_97

action_43 (102) = happyShift action_86
action_43 _ = happyFail

action_44 (103) = happyShift action_54
action_44 (105) = happyShift action_85
action_44 _ = happyReduce_27

action_45 (59) = happyShift action_11
action_45 (60) = happyShift action_31
action_45 (61) = happyShift action_32
action_45 (62) = happyShift action_33
action_45 (63) = happyShift action_34
action_45 (82) = happyShift action_12
action_45 (83) = happyShift action_13
action_45 (84) = happyShift action_14
action_45 (99) = happyShift action_35
action_45 (100) = happyShift action_53
action_45 (103) = happyShift action_36
action_45 (7) = happyGoto action_26
action_45 (8) = happyGoto action_52
action_45 (33) = happyGoto action_82
action_45 (34) = happyGoto action_83
action_45 (58) = happyGoto action_84
action_45 _ = happyFail

action_46 (60) = happyShift action_81
action_46 (49) = happyGoto action_79
action_46 (50) = happyGoto action_80
action_46 _ = happyReduce_133

action_47 _ = happyReduce_107

action_48 (59) = happyShift action_11
action_48 (60) = happyShift action_74
action_48 (61) = happyShift action_75
action_48 (82) = happyShift action_12
action_48 (83) = happyShift action_13
action_48 (84) = happyShift action_14
action_48 (99) = happyShift action_76
action_48 (101) = happyShift action_77
action_48 (103) = happyShift action_78
action_48 (47) = happyGoto action_67
action_48 (48) = happyGoto action_68
action_48 (51) = happyGoto action_69
action_48 (52) = happyGoto action_70
action_48 (53) = happyGoto action_71
action_48 (54) = happyGoto action_72
action_48 (58) = happyGoto action_73
action_48 _ = happyReduce_129

action_49 _ = happyReduce_106

action_50 (104) = happyShift action_66
action_50 _ = happyFail

action_51 _ = happyReduce_35

action_52 (105) = happyShift action_64
action_52 (107) = happyShift action_65
action_52 (9) = happyGoto action_63
action_52 _ = happyReduce_40

action_53 _ = happyReduce_33

action_54 (59) = happyShift action_11
action_54 (60) = happyShift action_31
action_54 (61) = happyShift action_32
action_54 (62) = happyShift action_33
action_54 (63) = happyShift action_34
action_54 (82) = happyShift action_12
action_54 (83) = happyShift action_13
action_54 (84) = happyShift action_14
action_54 (99) = happyShift action_35
action_54 (103) = happyShift action_36
action_54 (7) = happyGoto action_26
action_54 (8) = happyGoto action_27
action_54 (10) = happyGoto action_62
action_54 (58) = happyGoto action_30
action_54 _ = happyFail

action_55 (80) = happyShift action_61
action_55 (19) = happyGoto action_60
action_55 _ = happyReduce_76

action_56 (59) = happyShift action_11
action_56 (60) = happyShift action_31
action_56 (61) = happyShift action_32
action_56 (62) = happyShift action_33
action_56 (63) = happyShift action_34
action_56 (82) = happyShift action_12
action_56 (83) = happyShift action_13
action_56 (84) = happyShift action_14
action_56 (99) = happyShift action_35
action_56 (103) = happyShift action_36
action_56 (7) = happyGoto action_26
action_56 (8) = happyGoto action_27
action_56 (10) = happyGoto action_59
action_56 (58) = happyGoto action_30
action_56 _ = happyFail

action_57 (102) = happyShift action_58
action_57 _ = happyFail

action_58 (110) = happyShift action_130
action_58 _ = happyFail

action_59 _ = happyReduce_42

action_60 (109) = happyShift action_129
action_60 _ = happyFail

action_61 (59) = happyShift action_11
action_61 (60) = happyShift action_31
action_61 (61) = happyShift action_32
action_61 (62) = happyShift action_33
action_61 (63) = happyShift action_34
action_61 (71) = happyShift action_118
action_61 (72) = happyShift action_119
action_61 (75) = happyShift action_120
action_61 (77) = happyShift action_121
action_61 (78) = happyShift action_122
action_61 (82) = happyShift action_123
action_61 (83) = happyShift action_13
action_61 (84) = happyShift action_14
action_61 (85) = happyShift action_124
action_61 (86) = happyShift action_125
action_61 (99) = happyShift action_126
action_61 (101) = happyShift action_127
action_61 (103) = happyShift action_128
action_61 (6) = happyGoto action_108
action_61 (7) = happyGoto action_109
action_61 (13) = happyGoto action_110
action_61 (14) = happyGoto action_111
action_61 (15) = happyGoto action_112
action_61 (16) = happyGoto action_113
action_61 (25) = happyGoto action_114
action_61 (26) = happyGoto action_115
action_61 (27) = happyGoto action_116
action_61 (58) = happyGoto action_117
action_61 _ = happyFail

action_62 (104) = happyShift action_107
action_62 _ = happyFail

action_63 (100) = happyShift action_106
action_63 _ = happyFail

action_64 (59) = happyShift action_11
action_64 (60) = happyShift action_31
action_64 (61) = happyShift action_32
action_64 (62) = happyShift action_33
action_64 (63) = happyShift action_34
action_64 (82) = happyShift action_12
action_64 (83) = happyShift action_13
action_64 (84) = happyShift action_14
action_64 (99) = happyShift action_35
action_64 (103) = happyShift action_36
action_64 (7) = happyGoto action_26
action_64 (8) = happyGoto action_105
action_64 (58) = happyGoto action_30
action_64 _ = happyFail

action_65 (59) = happyShift action_11
action_65 (60) = happyShift action_31
action_65 (61) = happyShift action_32
action_65 (62) = happyShift action_33
action_65 (63) = happyShift action_34
action_65 (82) = happyShift action_12
action_65 (83) = happyShift action_13
action_65 (84) = happyShift action_14
action_65 (99) = happyShift action_35
action_65 (103) = happyShift action_36
action_65 (7) = happyGoto action_26
action_65 (8) = happyGoto action_104
action_65 (58) = happyGoto action_30
action_65 _ = happyFail

action_66 _ = happyReduce_36

action_67 (102) = happyShift action_103
action_67 _ = happyFail

action_68 (105) = happyShift action_102
action_68 _ = happyReduce_128

action_69 _ = happyReduce_131

action_70 _ = happyReduce_138

action_71 (107) = happyShift action_101
action_71 _ = happyReduce_136

action_72 _ = happyReduce_145

action_73 (101) = happyShift action_99
action_73 (103) = happyShift action_100
action_73 _ = happyReduce_148

action_74 _ = happyReduce_141

action_75 (111) = happyShift action_98
action_75 _ = happyReduce_142

action_76 (59) = happyShift action_11
action_76 (60) = happyShift action_74
action_76 (61) = happyShift action_75
action_76 (82) = happyShift action_12
action_76 (83) = happyShift action_13
action_76 (84) = happyShift action_14
action_76 (99) = happyShift action_76
action_76 (101) = happyShift action_77
action_76 (103) = happyShift action_78
action_76 (51) = happyGoto action_97
action_76 (52) = happyGoto action_70
action_76 (53) = happyGoto action_71
action_76 (54) = happyGoto action_72
action_76 (58) = happyGoto action_73
action_76 _ = happyFail

action_77 (59) = happyShift action_11
action_77 (60) = happyShift action_74
action_77 (61) = happyShift action_75
action_77 (82) = happyShift action_12
action_77 (83) = happyShift action_13
action_77 (84) = happyShift action_14
action_77 (99) = happyShift action_76
action_77 (101) = happyShift action_77
action_77 (102) = happyShift action_96
action_77 (103) = happyShift action_78
action_77 (51) = happyGoto action_95
action_77 (52) = happyGoto action_70
action_77 (53) = happyGoto action_71
action_77 (54) = happyGoto action_72
action_77 (58) = happyGoto action_73
action_77 _ = happyFail

action_78 (59) = happyShift action_11
action_78 (60) = happyShift action_74
action_78 (61) = happyShift action_75
action_78 (82) = happyShift action_12
action_78 (83) = happyShift action_13
action_78 (84) = happyShift action_14
action_78 (99) = happyShift action_76
action_78 (101) = happyShift action_77
action_78 (103) = happyShift action_78
action_78 (48) = happyGoto action_94
action_78 (51) = happyGoto action_69
action_78 (52) = happyGoto action_70
action_78 (53) = happyGoto action_71
action_78 (54) = happyGoto action_72
action_78 (58) = happyGoto action_73
action_78 _ = happyFail

action_79 (102) = happyShift action_93
action_79 _ = happyFail

action_80 (105) = happyShift action_92
action_80 _ = happyReduce_132

action_81 _ = happyReduce_135

action_82 (100) = happyShift action_91
action_82 _ = happyFail

action_83 (105) = happyShift action_90
action_83 _ = happyReduce_102

action_84 (88) = happyShift action_89
action_84 (103) = happyShift action_54
action_84 _ = happyReduce_27

action_85 (99) = happyShift action_88
action_85 _ = happyFail

action_86 (110) = happyShift action_87
action_86 _ = happyFail

action_87 _ = happyReduce_105

action_88 (59) = happyShift action_11
action_88 (82) = happyShift action_12
action_88 (83) = happyShift action_13
action_88 (84) = happyShift action_14
action_88 (32) = happyGoto action_202
action_88 (33) = happyGoto action_203
action_88 (34) = happyGoto action_83
action_88 (58) = happyGoto action_200
action_88 _ = happyReduce_101

action_89 (61) = happyShift action_201
action_89 _ = happyFail

action_90 (59) = happyShift action_11
action_90 (82) = happyShift action_12
action_90 (83) = happyShift action_13
action_90 (84) = happyShift action_14
action_90 (33) = happyGoto action_199
action_90 (34) = happyGoto action_83
action_90 (58) = happyGoto action_200
action_90 _ = happyFail

action_91 _ = happyReduce_98

action_92 (60) = happyShift action_198
action_92 _ = happyFail

action_93 (98) = happyShift action_197
action_93 _ = happyFail

action_94 (104) = happyShift action_196
action_94 (105) = happyShift action_102
action_94 _ = happyFail

action_95 (102) = happyShift action_195
action_95 _ = happyFail

action_96 _ = happyReduce_140

action_97 (100) = happyShift action_194
action_97 _ = happyFail

action_98 (59) = happyShift action_11
action_98 (82) = happyShift action_12
action_98 (83) = happyShift action_13
action_98 (84) = happyShift action_14
action_98 (99) = happyShift action_191
action_98 (101) = happyShift action_192
action_98 (103) = happyShift action_193
action_98 (56) = happyGoto action_188
action_98 (57) = happyGoto action_189
action_98 (58) = happyGoto action_190
action_98 _ = happyFail

action_99 (59) = happyShift action_11
action_99 (60) = happyShift action_74
action_99 (61) = happyShift action_75
action_99 (82) = happyShift action_12
action_99 (83) = happyShift action_13
action_99 (84) = happyShift action_14
action_99 (99) = happyShift action_76
action_99 (101) = happyShift action_77
action_99 (102) = happyShift action_187
action_99 (103) = happyShift action_78
action_99 (48) = happyGoto action_186
action_99 (51) = happyGoto action_69
action_99 (52) = happyGoto action_70
action_99 (53) = happyGoto action_71
action_99 (54) = happyGoto action_72
action_99 (58) = happyGoto action_73
action_99 _ = happyFail

action_100 (59) = happyShift action_11
action_100 (60) = happyShift action_74
action_100 (61) = happyShift action_75
action_100 (82) = happyShift action_12
action_100 (83) = happyShift action_13
action_100 (84) = happyShift action_14
action_100 (99) = happyShift action_76
action_100 (101) = happyShift action_77
action_100 (103) = happyShift action_78
action_100 (47) = happyGoto action_185
action_100 (48) = happyGoto action_68
action_100 (51) = happyGoto action_69
action_100 (52) = happyGoto action_70
action_100 (53) = happyGoto action_71
action_100 (54) = happyGoto action_72
action_100 (58) = happyGoto action_73
action_100 _ = happyReduce_129

action_101 (59) = happyShift action_11
action_101 (60) = happyShift action_74
action_101 (61) = happyShift action_75
action_101 (82) = happyShift action_12
action_101 (83) = happyShift action_13
action_101 (84) = happyShift action_14
action_101 (99) = happyShift action_76
action_101 (103) = happyShift action_78
action_101 (52) = happyGoto action_183
action_101 (54) = happyGoto action_184
action_101 (58) = happyGoto action_73
action_101 _ = happyFail

action_102 (59) = happyShift action_11
action_102 (60) = happyShift action_74
action_102 (61) = happyShift action_75
action_102 (82) = happyShift action_12
action_102 (83) = happyShift action_13
action_102 (84) = happyShift action_14
action_102 (99) = happyShift action_76
action_102 (101) = happyShift action_77
action_102 (103) = happyShift action_78
action_102 (51) = happyGoto action_182
action_102 (52) = happyGoto action_70
action_102 (53) = happyGoto action_71
action_102 (54) = happyGoto action_72
action_102 (58) = happyGoto action_73
action_102 _ = happyFail

action_103 (109) = happyShift action_181
action_103 _ = happyFail

action_104 _ = happyReduce_38

action_105 (105) = happyShift action_64
action_105 (107) = happyShift action_65
action_105 (9) = happyGoto action_180
action_105 _ = happyReduce_40

action_106 _ = happyReduce_34

action_107 _ = happyReduce_37

action_108 (59) = happyShift action_11
action_108 (60) = happyShift action_31
action_108 (61) = happyShift action_32
action_108 (62) = happyShift action_33
action_108 (63) = happyShift action_34
action_108 (72) = happyShift action_119
action_108 (75) = happyShift action_120
action_108 (77) = happyShift action_121
action_108 (78) = happyShift action_122
action_108 (82) = happyShift action_12
action_108 (83) = happyShift action_13
action_108 (84) = happyShift action_14
action_108 (99) = happyShift action_126
action_108 (101) = happyShift action_127
action_108 (103) = happyShift action_128
action_108 (7) = happyGoto action_109
action_108 (16) = happyGoto action_179
action_108 (58) = happyGoto action_136
action_108 _ = happyFail

action_109 _ = happyReduce_57

action_110 (64) = happyShift action_165
action_110 (65) = happyShift action_166
action_110 (66) = happyShift action_167
action_110 (67) = happyShift action_168
action_110 (85) = happyShift action_169
action_110 (86) = happyShift action_170
action_110 (89) = happyShift action_171
action_110 (90) = happyShift action_172
action_110 (91) = happyShift action_173
action_110 (92) = happyShift action_174
action_110 (93) = happyShift action_175
action_110 (94) = happyShift action_176
action_110 (95) = happyShift action_177
action_110 (96) = happyShift action_178
action_110 (3) = happyGoto action_163
action_110 (4) = happyGoto action_164
action_110 _ = happyFail

action_111 (68) = happyShift action_158
action_111 (69) = happyShift action_159
action_111 (70) = happyShift action_160
action_111 (87) = happyShift action_161
action_111 (88) = happyShift action_162
action_111 (5) = happyGoto action_157
action_111 _ = happyReduce_52

action_112 _ = happyReduce_54

action_113 _ = happyReduce_56

action_114 (105) = happyShift action_156
action_114 _ = happyReduce_88

action_115 _ = happyReduce_91

action_116 _ = happyReduce_75

action_117 (101) = happyShift action_153
action_117 (103) = happyShift action_154
action_117 (108) = happyShift action_155
action_117 _ = happyReduce_27

action_118 _ = happyReduce_26

action_119 (59) = happyShift action_11
action_119 (60) = happyShift action_31
action_119 (61) = happyShift action_32
action_119 (62) = happyShift action_33
action_119 (63) = happyShift action_34
action_119 (71) = happyShift action_118
action_119 (72) = happyShift action_119
action_119 (74) = happyShift action_137
action_119 (75) = happyShift action_120
action_119 (77) = happyShift action_121
action_119 (78) = happyShift action_122
action_119 (81) = happyShift action_138
action_119 (82) = happyShift action_12
action_119 (83) = happyShift action_13
action_119 (84) = happyShift action_14
action_119 (85) = happyShift action_124
action_119 (86) = happyShift action_125
action_119 (99) = happyShift action_126
action_119 (101) = happyShift action_127
action_119 (103) = happyShift action_128
action_119 (6) = happyGoto action_108
action_119 (7) = happyGoto action_109
action_119 (11) = happyGoto action_132
action_119 (12) = happyGoto action_133
action_119 (13) = happyGoto action_134
action_119 (14) = happyGoto action_111
action_119 (15) = happyGoto action_112
action_119 (16) = happyGoto action_113
action_119 (24) = happyGoto action_152
action_119 (58) = happyGoto action_136
action_119 _ = happyFail

action_120 (59) = happyShift action_11
action_120 (60) = happyShift action_31
action_120 (61) = happyShift action_32
action_120 (62) = happyShift action_33
action_120 (63) = happyShift action_34
action_120 (71) = happyShift action_118
action_120 (72) = happyShift action_119
action_120 (74) = happyShift action_137
action_120 (75) = happyShift action_120
action_120 (77) = happyShift action_121
action_120 (78) = happyShift action_122
action_120 (81) = happyShift action_138
action_120 (82) = happyShift action_12
action_120 (83) = happyShift action_13
action_120 (84) = happyShift action_14
action_120 (85) = happyShift action_124
action_120 (86) = happyShift action_125
action_120 (99) = happyShift action_126
action_120 (101) = happyShift action_127
action_120 (103) = happyShift action_128
action_120 (6) = happyGoto action_108
action_120 (7) = happyGoto action_109
action_120 (11) = happyGoto action_151
action_120 (12) = happyGoto action_133
action_120 (13) = happyGoto action_134
action_120 (14) = happyGoto action_111
action_120 (15) = happyGoto action_112
action_120 (16) = happyGoto action_113
action_120 (58) = happyGoto action_136
action_120 _ = happyFail

action_121 (59) = happyShift action_11
action_121 (60) = happyShift action_31
action_121 (61) = happyShift action_32
action_121 (62) = happyShift action_33
action_121 (63) = happyShift action_34
action_121 (71) = happyShift action_118
action_121 (72) = happyShift action_119
action_121 (75) = happyShift action_120
action_121 (77) = happyShift action_121
action_121 (78) = happyShift action_122
action_121 (82) = happyShift action_123
action_121 (83) = happyShift action_13
action_121 (84) = happyShift action_14
action_121 (85) = happyShift action_124
action_121 (86) = happyShift action_125
action_121 (99) = happyShift action_126
action_121 (101) = happyShift action_127
action_121 (103) = happyShift action_128
action_121 (6) = happyGoto action_108
action_121 (7) = happyGoto action_109
action_121 (13) = happyGoto action_110
action_121 (14) = happyGoto action_111
action_121 (15) = happyGoto action_112
action_121 (16) = happyGoto action_113
action_121 (21) = happyGoto action_148
action_121 (22) = happyGoto action_149
action_121 (25) = happyGoto action_114
action_121 (26) = happyGoto action_115
action_121 (27) = happyGoto action_150
action_121 (58) = happyGoto action_117
action_121 _ = happyFail

action_122 (59) = happyShift action_11
action_122 (60) = happyShift action_31
action_122 (61) = happyShift action_32
action_122 (62) = happyShift action_33
action_122 (63) = happyShift action_34
action_122 (71) = happyShift action_118
action_122 (72) = happyShift action_119
action_122 (74) = happyShift action_137
action_122 (75) = happyShift action_120
action_122 (77) = happyShift action_121
action_122 (78) = happyShift action_122
action_122 (79) = happyShift action_147
action_122 (81) = happyShift action_138
action_122 (82) = happyShift action_12
action_122 (83) = happyShift action_13
action_122 (84) = happyShift action_14
action_122 (85) = happyShift action_124
action_122 (86) = happyShift action_125
action_122 (99) = happyShift action_126
action_122 (101) = happyShift action_127
action_122 (103) = happyShift action_128
action_122 (6) = happyGoto action_108
action_122 (7) = happyGoto action_109
action_122 (11) = happyGoto action_144
action_122 (12) = happyGoto action_133
action_122 (13) = happyGoto action_134
action_122 (14) = happyGoto action_111
action_122 (15) = happyGoto action_112
action_122 (16) = happyGoto action_113
action_122 (18) = happyGoto action_145
action_122 (20) = happyGoto action_146
action_122 (58) = happyGoto action_136
action_122 _ = happyFail

action_123 (64) = happyReduce_162
action_123 (65) = happyReduce_162
action_123 (66) = happyReduce_162
action_123 (67) = happyReduce_162
action_123 (68) = happyReduce_162
action_123 (69) = happyReduce_162
action_123 (70) = happyReduce_162
action_123 (85) = happyReduce_162
action_123 (86) = happyReduce_162
action_123 (87) = happyReduce_162
action_123 (88) = happyReduce_162
action_123 (89) = happyReduce_162
action_123 (90) = happyReduce_162
action_123 (91) = happyReduce_162
action_123 (92) = happyReduce_162
action_123 (93) = happyReduce_162
action_123 (94) = happyReduce_162
action_123 (95) = happyReduce_162
action_123 (96) = happyReduce_162
action_123 (101) = happyReduce_162
action_123 (103) = happyReduce_162
action_123 (108) = happyReduce_162
action_123 (109) = happyReduce_90
action_123 _ = happyFail

action_124 _ = happyReduce_24

action_125 _ = happyReduce_25

action_126 (59) = happyShift action_11
action_126 (60) = happyShift action_31
action_126 (61) = happyShift action_32
action_126 (62) = happyShift action_33
action_126 (63) = happyShift action_34
action_126 (71) = happyShift action_118
action_126 (72) = happyShift action_119
action_126 (74) = happyShift action_137
action_126 (75) = happyShift action_120
action_126 (77) = happyShift action_121
action_126 (78) = happyShift action_122
action_126 (81) = happyShift action_138
action_126 (82) = happyShift action_12
action_126 (83) = happyShift action_13
action_126 (84) = happyShift action_14
action_126 (85) = happyShift action_124
action_126 (86) = happyShift action_125
action_126 (99) = happyShift action_126
action_126 (100) = happyShift action_143
action_126 (101) = happyShift action_127
action_126 (103) = happyShift action_128
action_126 (6) = happyGoto action_108
action_126 (7) = happyGoto action_109
action_126 (11) = happyGoto action_142
action_126 (12) = happyGoto action_133
action_126 (13) = happyGoto action_134
action_126 (14) = happyGoto action_111
action_126 (15) = happyGoto action_112
action_126 (16) = happyGoto action_113
action_126 (58) = happyGoto action_136
action_126 _ = happyFail

action_127 (59) = happyShift action_11
action_127 (60) = happyShift action_31
action_127 (61) = happyShift action_32
action_127 (62) = happyShift action_33
action_127 (63) = happyShift action_34
action_127 (71) = happyShift action_118
action_127 (72) = happyShift action_119
action_127 (74) = happyShift action_137
action_127 (75) = happyShift action_120
action_127 (77) = happyShift action_121
action_127 (78) = happyShift action_122
action_127 (81) = happyShift action_138
action_127 (82) = happyShift action_12
action_127 (83) = happyShift action_13
action_127 (84) = happyShift action_14
action_127 (85) = happyShift action_124
action_127 (86) = happyShift action_125
action_127 (99) = happyShift action_126
action_127 (101) = happyShift action_127
action_127 (103) = happyShift action_128
action_127 (6) = happyGoto action_108
action_127 (7) = happyGoto action_109
action_127 (11) = happyGoto action_141
action_127 (12) = happyGoto action_133
action_127 (13) = happyGoto action_134
action_127 (14) = happyGoto action_111
action_127 (15) = happyGoto action_112
action_127 (16) = happyGoto action_113
action_127 (58) = happyGoto action_136
action_127 _ = happyFail

action_128 (59) = happyShift action_11
action_128 (60) = happyShift action_31
action_128 (61) = happyShift action_32
action_128 (62) = happyShift action_33
action_128 (63) = happyShift action_34
action_128 (71) = happyShift action_118
action_128 (72) = happyShift action_119
action_128 (74) = happyShift action_137
action_128 (75) = happyShift action_120
action_128 (77) = happyShift action_121
action_128 (78) = happyShift action_122
action_128 (81) = happyShift action_138
action_128 (82) = happyShift action_12
action_128 (83) = happyShift action_13
action_128 (84) = happyShift action_14
action_128 (85) = happyShift action_124
action_128 (86) = happyShift action_125
action_128 (99) = happyShift action_126
action_128 (101) = happyShift action_127
action_128 (103) = happyShift action_128
action_128 (6) = happyGoto action_108
action_128 (7) = happyGoto action_109
action_128 (11) = happyGoto action_132
action_128 (12) = happyGoto action_133
action_128 (13) = happyGoto action_134
action_128 (14) = happyGoto action_111
action_128 (15) = happyGoto action_112
action_128 (16) = happyGoto action_113
action_128 (23) = happyGoto action_139
action_128 (24) = happyGoto action_140
action_128 (58) = happyGoto action_136
action_128 _ = happyReduce_83

action_129 (59) = happyShift action_11
action_129 (60) = happyShift action_31
action_129 (61) = happyShift action_32
action_129 (62) = happyShift action_33
action_129 (63) = happyShift action_34
action_129 (71) = happyShift action_118
action_129 (72) = happyShift action_119
action_129 (74) = happyShift action_137
action_129 (75) = happyShift action_120
action_129 (77) = happyShift action_121
action_129 (78) = happyShift action_122
action_129 (81) = happyShift action_138
action_129 (82) = happyShift action_12
action_129 (83) = happyShift action_13
action_129 (84) = happyShift action_14
action_129 (85) = happyShift action_124
action_129 (86) = happyShift action_125
action_129 (99) = happyShift action_126
action_129 (101) = happyShift action_127
action_129 (103) = happyShift action_128
action_129 (6) = happyGoto action_108
action_129 (7) = happyGoto action_109
action_129 (11) = happyGoto action_132
action_129 (12) = happyGoto action_133
action_129 (13) = happyGoto action_134
action_129 (14) = happyGoto action_111
action_129 (15) = happyGoto action_112
action_129 (16) = happyGoto action_113
action_129 (24) = happyGoto action_135
action_129 (58) = happyGoto action_136
action_129 _ = happyFail

action_130 (42) = happyGoto action_131
action_130 _ = happyReduce_117

action_131 (59) = happyShift action_11
action_131 (82) = happyShift action_12
action_131 (83) = happyShift action_13
action_131 (84) = happyShift action_14
action_131 (86) = happyShift action_246
action_131 (38) = happyGoto action_244
action_131 (43) = happyGoto action_245
action_131 (58) = happyGoto action_38
action_131 _ = happyReduce_115

action_132 (105) = happyShift action_243
action_132 _ = happyReduce_84

action_133 _ = happyReduce_47

action_134 (64) = happyShift action_165
action_134 (65) = happyShift action_166
action_134 (66) = happyShift action_167
action_134 (67) = happyShift action_168
action_134 (85) = happyShift action_169
action_134 (86) = happyShift action_170
action_134 (97) = happyShift action_241
action_134 (98) = happyShift action_242
action_134 (3) = happyGoto action_163
action_134 _ = happyReduce_50

action_135 _ = happyReduce_92

action_136 (101) = happyShift action_240
action_136 (103) = happyShift action_154
action_136 (108) = happyShift action_155
action_136 _ = happyReduce_27

action_137 (59) = happyShift action_11
action_137 (60) = happyShift action_31
action_137 (61) = happyShift action_32
action_137 (62) = happyShift action_33
action_137 (63) = happyShift action_34
action_137 (71) = happyShift action_118
action_137 (72) = happyShift action_119
action_137 (74) = happyShift action_137
action_137 (75) = happyShift action_120
action_137 (77) = happyShift action_121
action_137 (78) = happyShift action_122
action_137 (81) = happyShift action_138
action_137 (82) = happyShift action_12
action_137 (83) = happyShift action_13
action_137 (84) = happyShift action_14
action_137 (85) = happyShift action_124
action_137 (86) = happyShift action_125
action_137 (99) = happyShift action_126
action_137 (101) = happyShift action_127
action_137 (103) = happyShift action_128
action_137 (6) = happyGoto action_108
action_137 (7) = happyGoto action_109
action_137 (11) = happyGoto action_239
action_137 (12) = happyGoto action_133
action_137 (13) = happyGoto action_134
action_137 (14) = happyGoto action_111
action_137 (15) = happyGoto action_112
action_137 (16) = happyGoto action_113
action_137 (58) = happyGoto action_136
action_137 _ = happyFail

action_138 (60) = happyShift action_237
action_138 (101) = happyShift action_238
action_138 _ = happyFail

action_139 (104) = happyShift action_236
action_139 _ = happyFail

action_140 _ = happyReduce_82

action_141 (102) = happyShift action_235
action_141 _ = happyFail

action_142 (105) = happyShift action_233
action_142 (107) = happyShift action_234
action_142 (17) = happyGoto action_232
action_142 _ = happyReduce_73

action_143 _ = happyReduce_58

action_144 (80) = happyShift action_61
action_144 (19) = happyGoto action_231
action_144 _ = happyReduce_76

action_145 (106) = happyShift action_230
action_145 _ = happyReduce_77

action_146 (73) = happyShift action_228
action_146 (79) = happyShift action_229
action_146 _ = happyFail

action_147 (59) = happyShift action_11
action_147 (60) = happyShift action_31
action_147 (61) = happyShift action_32
action_147 (62) = happyShift action_33
action_147 (63) = happyShift action_34
action_147 (71) = happyShift action_118
action_147 (72) = happyShift action_119
action_147 (74) = happyShift action_137
action_147 (75) = happyShift action_120
action_147 (77) = happyShift action_121
action_147 (78) = happyShift action_122
action_147 (81) = happyShift action_138
action_147 (82) = happyShift action_12
action_147 (83) = happyShift action_13
action_147 (84) = happyShift action_14
action_147 (85) = happyShift action_124
action_147 (86) = happyShift action_125
action_147 (99) = happyShift action_126
action_147 (101) = happyShift action_127
action_147 (103) = happyShift action_128
action_147 (6) = happyGoto action_108
action_147 (7) = happyGoto action_109
action_147 (11) = happyGoto action_227
action_147 (12) = happyGoto action_133
action_147 (13) = happyGoto action_134
action_147 (14) = happyGoto action_111
action_147 (15) = happyGoto action_112
action_147 (16) = happyGoto action_113
action_147 (58) = happyGoto action_136
action_147 _ = happyFail

action_148 (106) = happyShift action_226
action_148 _ = happyReduce_80

action_149 (73) = happyShift action_225
action_149 _ = happyFail

action_150 (109) = happyShift action_224
action_150 _ = happyFail

action_151 (76) = happyShift action_223
action_151 _ = happyFail

action_152 (73) = happyShift action_222
action_152 _ = happyFail

action_153 (59) = happyShift action_11
action_153 (60) = happyShift action_31
action_153 (61) = happyShift action_32
action_153 (62) = happyShift action_33
action_153 (63) = happyShift action_34
action_153 (71) = happyShift action_118
action_153 (72) = happyShift action_119
action_153 (74) = happyShift action_137
action_153 (75) = happyShift action_120
action_153 (77) = happyShift action_121
action_153 (78) = happyShift action_122
action_153 (81) = happyShift action_138
action_153 (82) = happyShift action_12
action_153 (83) = happyShift action_13
action_153 (84) = happyShift action_14
action_153 (85) = happyShift action_124
action_153 (86) = happyShift action_125
action_153 (99) = happyShift action_126
action_153 (101) = happyShift action_127
action_153 (103) = happyShift action_128
action_153 (6) = happyGoto action_108
action_153 (7) = happyGoto action_109
action_153 (11) = happyGoto action_132
action_153 (12) = happyGoto action_133
action_153 (13) = happyGoto action_134
action_153 (14) = happyGoto action_111
action_153 (15) = happyGoto action_112
action_153 (16) = happyGoto action_113
action_153 (23) = happyGoto action_221
action_153 (24) = happyGoto action_140
action_153 (58) = happyGoto action_136
action_153 _ = happyReduce_83

action_154 (59) = happyShift action_11
action_154 (60) = happyShift action_31
action_154 (61) = happyShift action_32
action_154 (62) = happyShift action_33
action_154 (63) = happyShift action_34
action_154 (71) = happyShift action_118
action_154 (72) = happyShift action_119
action_154 (74) = happyShift action_137
action_154 (75) = happyShift action_120
action_154 (77) = happyShift action_121
action_154 (78) = happyShift action_122
action_154 (81) = happyShift action_138
action_154 (82) = happyShift action_12
action_154 (83) = happyShift action_13
action_154 (84) = happyShift action_14
action_154 (85) = happyShift action_124
action_154 (86) = happyShift action_125
action_154 (99) = happyShift action_126
action_154 (101) = happyShift action_127
action_154 (103) = happyShift action_128
action_154 (6) = happyGoto action_108
action_154 (7) = happyGoto action_109
action_154 (11) = happyGoto action_132
action_154 (12) = happyGoto action_133
action_154 (13) = happyGoto action_134
action_154 (14) = happyGoto action_111
action_154 (15) = happyGoto action_112
action_154 (16) = happyGoto action_113
action_154 (23) = happyGoto action_220
action_154 (24) = happyGoto action_140
action_154 (58) = happyGoto action_136
action_154 _ = happyReduce_83

action_155 (59) = happyShift action_11
action_155 (82) = happyShift action_12
action_155 (83) = happyShift action_13
action_155 (84) = happyShift action_14
action_155 (58) = happyGoto action_219
action_155 _ = happyFail

action_156 (59) = happyShift action_11
action_156 (60) = happyShift action_31
action_156 (61) = happyShift action_32
action_156 (62) = happyShift action_33
action_156 (63) = happyShift action_34
action_156 (71) = happyShift action_118
action_156 (72) = happyShift action_119
action_156 (75) = happyShift action_120
action_156 (77) = happyShift action_121
action_156 (78) = happyShift action_122
action_156 (82) = happyShift action_12
action_156 (83) = happyShift action_13
action_156 (84) = happyShift action_14
action_156 (85) = happyShift action_124
action_156 (86) = happyShift action_125
action_156 (99) = happyShift action_126
action_156 (101) = happyShift action_127
action_156 (103) = happyShift action_128
action_156 (6) = happyGoto action_108
action_156 (7) = happyGoto action_109
action_156 (13) = happyGoto action_110
action_156 (14) = happyGoto action_111
action_156 (15) = happyGoto action_112
action_156 (16) = happyGoto action_113
action_156 (25) = happyGoto action_114
action_156 (26) = happyGoto action_218
action_156 (58) = happyGoto action_117
action_156 _ = happyFail

action_157 (59) = happyShift action_11
action_157 (60) = happyShift action_31
action_157 (61) = happyShift action_32
action_157 (62) = happyShift action_33
action_157 (63) = happyShift action_34
action_157 (71) = happyShift action_118
action_157 (72) = happyShift action_119
action_157 (75) = happyShift action_120
action_157 (77) = happyShift action_121
action_157 (78) = happyShift action_122
action_157 (82) = happyShift action_12
action_157 (83) = happyShift action_13
action_157 (84) = happyShift action_14
action_157 (85) = happyShift action_124
action_157 (86) = happyShift action_125
action_157 (99) = happyShift action_126
action_157 (101) = happyShift action_127
action_157 (103) = happyShift action_128
action_157 (6) = happyGoto action_108
action_157 (7) = happyGoto action_109
action_157 (15) = happyGoto action_217
action_157 (16) = happyGoto action_113
action_157 (58) = happyGoto action_136
action_157 _ = happyFail

action_158 _ = happyReduce_21

action_159 _ = happyReduce_22

action_160 _ = happyReduce_23

action_161 _ = happyReduce_19

action_162 _ = happyReduce_20

action_163 (59) = happyShift action_11
action_163 (60) = happyShift action_31
action_163 (61) = happyShift action_32
action_163 (62) = happyShift action_33
action_163 (63) = happyShift action_34
action_163 (71) = happyShift action_118
action_163 (72) = happyShift action_119
action_163 (75) = happyShift action_120
action_163 (77) = happyShift action_121
action_163 (78) = happyShift action_122
action_163 (82) = happyShift action_12
action_163 (83) = happyShift action_13
action_163 (84) = happyShift action_14
action_163 (85) = happyShift action_124
action_163 (86) = happyShift action_125
action_163 (99) = happyShift action_126
action_163 (101) = happyShift action_127
action_163 (103) = happyShift action_128
action_163 (6) = happyGoto action_108
action_163 (7) = happyGoto action_109
action_163 (14) = happyGoto action_216
action_163 (15) = happyGoto action_112
action_163 (16) = happyGoto action_113
action_163 (58) = happyGoto action_136
action_163 _ = happyFail

action_164 (59) = happyShift action_11
action_164 (60) = happyShift action_31
action_164 (61) = happyShift action_32
action_164 (62) = happyShift action_33
action_164 (63) = happyShift action_34
action_164 (71) = happyShift action_118
action_164 (72) = happyShift action_119
action_164 (75) = happyShift action_120
action_164 (77) = happyShift action_121
action_164 (78) = happyShift action_122
action_164 (82) = happyShift action_12
action_164 (83) = happyShift action_13
action_164 (84) = happyShift action_14
action_164 (85) = happyShift action_124
action_164 (86) = happyShift action_125
action_164 (99) = happyShift action_126
action_164 (101) = happyShift action_127
action_164 (103) = happyShift action_128
action_164 (6) = happyGoto action_108
action_164 (7) = happyGoto action_109
action_164 (13) = happyGoto action_215
action_164 (14) = happyGoto action_111
action_164 (15) = happyGoto action_112
action_164 (16) = happyGoto action_113
action_164 (58) = happyGoto action_136
action_164 _ = happyFail

action_165 _ = happyReduce_7

action_166 _ = happyReduce_8

action_167 _ = happyReduce_9

action_168 _ = happyReduce_10

action_169 _ = happyReduce_5

action_170 _ = happyReduce_6

action_171 _ = happyReduce_11

action_172 _ = happyReduce_12

action_173 _ = happyReduce_13

action_174 _ = happyReduce_14

action_175 _ = happyReduce_15

action_176 _ = happyReduce_16

action_177 _ = happyReduce_17

action_178 _ = happyReduce_18

action_179 _ = happyReduce_55

action_180 _ = happyReduce_39

action_181 (59) = happyShift action_11
action_181 (60) = happyShift action_74
action_181 (61) = happyShift action_75
action_181 (82) = happyShift action_12
action_181 (83) = happyShift action_13
action_181 (84) = happyShift action_14
action_181 (99) = happyShift action_76
action_181 (101) = happyShift action_77
action_181 (103) = happyShift action_78
action_181 (51) = happyGoto action_214
action_181 (52) = happyGoto action_70
action_181 (53) = happyGoto action_71
action_181 (54) = happyGoto action_72
action_181 (58) = happyGoto action_73
action_181 _ = happyFail

action_182 _ = happyReduce_130

action_183 _ = happyReduce_137

action_184 _ = happyReduce_144

action_185 (104) = happyShift action_213
action_185 _ = happyFail

action_186 (102) = happyShift action_212
action_186 (105) = happyShift action_102
action_186 _ = happyFail

action_187 _ = happyReduce_146

action_188 _ = happyReduce_143

action_189 _ = happyReduce_154

action_190 (88) = happyShift action_210
action_190 (101) = happyShift action_211
action_190 _ = happyReduce_157

action_191 (100) = happyShift action_209
action_191 _ = happyFail

action_192 (59) = happyShift action_11
action_192 (82) = happyShift action_12
action_192 (83) = happyShift action_13
action_192 (84) = happyShift action_14
action_192 (99) = happyShift action_191
action_192 (103) = happyShift action_193
action_192 (55) = happyGoto action_207
action_192 (57) = happyGoto action_208
action_192 (58) = happyGoto action_190
action_192 _ = happyFail

action_193 (61) = happyShift action_206
action_193 _ = happyFail

action_194 _ = happyReduce_151

action_195 _ = happyReduce_139

action_196 _ = happyReduce_149

action_197 (59) = happyShift action_11
action_197 (60) = happyShift action_74
action_197 (61) = happyShift action_75
action_197 (82) = happyShift action_12
action_197 (83) = happyShift action_13
action_197 (84) = happyShift action_14
action_197 (99) = happyShift action_76
action_197 (101) = happyShift action_77
action_197 (103) = happyShift action_78
action_197 (51) = happyGoto action_205
action_197 (52) = happyGoto action_70
action_197 (53) = happyGoto action_71
action_197 (54) = happyGoto action_72
action_197 (58) = happyGoto action_73
action_197 _ = happyFail

action_198 _ = happyReduce_134

action_199 _ = happyReduce_103

action_200 (88) = happyShift action_89
action_200 _ = happyFail

action_201 _ = happyReduce_104

action_202 (100) = happyShift action_204
action_202 _ = happyFail

action_203 _ = happyReduce_100

action_204 _ = happyReduce_99

action_205 (80) = happyShift action_270
action_205 (44) = happyGoto action_276
action_205 _ = happyReduce_122

action_206 (104) = happyShift action_275
action_206 _ = happyFail

action_207 (102) = happyShift action_273
action_207 (105) = happyShift action_274
action_207 _ = happyFail

action_208 _ = happyReduce_153

action_209 _ = happyReduce_160

action_210 (61) = happyShift action_272
action_210 _ = happyFail

action_211 (102) = happyShift action_271
action_211 _ = happyFail

action_212 _ = happyReduce_147

action_213 _ = happyReduce_150

action_214 (80) = happyShift action_270
action_214 (44) = happyGoto action_269
action_214 _ = happyReduce_122

action_215 (64) = happyShift action_165
action_215 (65) = happyShift action_166
action_215 (66) = happyShift action_167
action_215 (67) = happyShift action_168
action_215 (85) = happyShift action_169
action_215 (86) = happyShift action_170
action_215 (3) = happyGoto action_163
action_215 _ = happyReduce_87

action_216 (68) = happyShift action_158
action_216 (69) = happyShift action_159
action_216 (70) = happyShift action_160
action_216 (87) = happyShift action_161
action_216 (88) = happyShift action_162
action_216 (5) = happyGoto action_157
action_216 _ = happyReduce_51

action_217 _ = happyReduce_53

action_218 _ = happyReduce_89

action_219 (101) = happyShift action_268
action_219 _ = happyFail

action_220 (104) = happyShift action_267
action_220 _ = happyFail

action_221 (102) = happyShift action_266
action_221 _ = happyFail

action_222 _ = happyReduce_65

action_223 (59) = happyShift action_11
action_223 (60) = happyShift action_31
action_223 (61) = happyShift action_32
action_223 (62) = happyShift action_33
action_223 (63) = happyShift action_34
action_223 (71) = happyShift action_118
action_223 (72) = happyShift action_119
action_223 (74) = happyShift action_137
action_223 (75) = happyShift action_120
action_223 (77) = happyShift action_121
action_223 (78) = happyShift action_122
action_223 (81) = happyShift action_138
action_223 (82) = happyShift action_12
action_223 (83) = happyShift action_13
action_223 (84) = happyShift action_14
action_223 (85) = happyShift action_124
action_223 (86) = happyShift action_125
action_223 (99) = happyShift action_126
action_223 (101) = happyShift action_127
action_223 (103) = happyShift action_128
action_223 (6) = happyGoto action_108
action_223 (7) = happyGoto action_109
action_223 (11) = happyGoto action_144
action_223 (12) = happyGoto action_133
action_223 (13) = happyGoto action_134
action_223 (14) = happyGoto action_111
action_223 (15) = happyGoto action_112
action_223 (16) = happyGoto action_113
action_223 (18) = happyGoto action_145
action_223 (20) = happyGoto action_265
action_223 (58) = happyGoto action_136
action_223 _ = happyFail

action_224 (59) = happyShift action_11
action_224 (60) = happyShift action_31
action_224 (61) = happyShift action_32
action_224 (62) = happyShift action_33
action_224 (63) = happyShift action_34
action_224 (71) = happyShift action_118
action_224 (72) = happyShift action_119
action_224 (74) = happyShift action_137
action_224 (75) = happyShift action_120
action_224 (77) = happyShift action_121
action_224 (78) = happyShift action_122
action_224 (81) = happyShift action_138
action_224 (82) = happyShift action_12
action_224 (83) = happyShift action_13
action_224 (84) = happyShift action_14
action_224 (85) = happyShift action_124
action_224 (86) = happyShift action_125
action_224 (99) = happyShift action_126
action_224 (101) = happyShift action_127
action_224 (103) = happyShift action_128
action_224 (6) = happyGoto action_108
action_224 (7) = happyGoto action_109
action_224 (11) = happyGoto action_132
action_224 (12) = happyGoto action_133
action_224 (13) = happyGoto action_134
action_224 (14) = happyGoto action_111
action_224 (15) = happyGoto action_112
action_224 (16) = happyGoto action_113
action_224 (24) = happyGoto action_264
action_224 (58) = happyGoto action_136
action_224 _ = happyFail

action_225 _ = happyReduce_67

action_226 (59) = happyShift action_11
action_226 (60) = happyShift action_31
action_226 (61) = happyShift action_32
action_226 (62) = happyShift action_33
action_226 (63) = happyShift action_34
action_226 (71) = happyShift action_118
action_226 (72) = happyShift action_119
action_226 (75) = happyShift action_120
action_226 (77) = happyShift action_121
action_226 (78) = happyShift action_122
action_226 (82) = happyShift action_123
action_226 (83) = happyShift action_13
action_226 (84) = happyShift action_14
action_226 (85) = happyShift action_124
action_226 (86) = happyShift action_125
action_226 (99) = happyShift action_126
action_226 (101) = happyShift action_127
action_226 (103) = happyShift action_128
action_226 (6) = happyGoto action_108
action_226 (7) = happyGoto action_109
action_226 (13) = happyGoto action_110
action_226 (14) = happyGoto action_111
action_226 (15) = happyGoto action_112
action_226 (16) = happyGoto action_113
action_226 (21) = happyGoto action_148
action_226 (22) = happyGoto action_263
action_226 (25) = happyGoto action_114
action_226 (26) = happyGoto action_115
action_226 (27) = happyGoto action_150
action_226 (58) = happyGoto action_117
action_226 _ = happyFail

action_227 (109) = happyShift action_262
action_227 _ = happyFail

action_228 _ = happyReduce_69

action_229 (59) = happyShift action_11
action_229 (60) = happyShift action_31
action_229 (61) = happyShift action_32
action_229 (62) = happyShift action_33
action_229 (63) = happyShift action_34
action_229 (71) = happyShift action_118
action_229 (72) = happyShift action_119
action_229 (74) = happyShift action_137
action_229 (75) = happyShift action_120
action_229 (77) = happyShift action_121
action_229 (78) = happyShift action_122
action_229 (81) = happyShift action_138
action_229 (82) = happyShift action_12
action_229 (83) = happyShift action_13
action_229 (84) = happyShift action_14
action_229 (85) = happyShift action_124
action_229 (86) = happyShift action_125
action_229 (99) = happyShift action_126
action_229 (101) = happyShift action_127
action_229 (103) = happyShift action_128
action_229 (6) = happyGoto action_108
action_229 (7) = happyGoto action_109
action_229 (11) = happyGoto action_261
action_229 (12) = happyGoto action_133
action_229 (13) = happyGoto action_134
action_229 (14) = happyGoto action_111
action_229 (15) = happyGoto action_112
action_229 (16) = happyGoto action_113
action_229 (58) = happyGoto action_136
action_229 _ = happyFail

action_230 (59) = happyShift action_11
action_230 (60) = happyShift action_31
action_230 (61) = happyShift action_32
action_230 (62) = happyShift action_33
action_230 (63) = happyShift action_34
action_230 (71) = happyShift action_118
action_230 (72) = happyShift action_119
action_230 (74) = happyShift action_137
action_230 (75) = happyShift action_120
action_230 (77) = happyShift action_121
action_230 (78) = happyShift action_122
action_230 (81) = happyShift action_138
action_230 (82) = happyShift action_12
action_230 (83) = happyShift action_13
action_230 (84) = happyShift action_14
action_230 (85) = happyShift action_124
action_230 (86) = happyShift action_125
action_230 (99) = happyShift action_126
action_230 (101) = happyShift action_127
action_230 (103) = happyShift action_128
action_230 (6) = happyGoto action_108
action_230 (7) = happyGoto action_109
action_230 (11) = happyGoto action_144
action_230 (12) = happyGoto action_133
action_230 (13) = happyGoto action_134
action_230 (14) = happyGoto action_111
action_230 (15) = happyGoto action_112
action_230 (16) = happyGoto action_113
action_230 (18) = happyGoto action_145
action_230 (20) = happyGoto action_260
action_230 (58) = happyGoto action_136
action_230 _ = happyFail

action_231 (109) = happyShift action_259
action_231 _ = happyFail

action_232 (100) = happyShift action_258
action_232 _ = happyFail

action_233 (59) = happyShift action_11
action_233 (60) = happyShift action_31
action_233 (61) = happyShift action_32
action_233 (62) = happyShift action_33
action_233 (63) = happyShift action_34
action_233 (71) = happyShift action_118
action_233 (72) = happyShift action_119
action_233 (74) = happyShift action_137
action_233 (75) = happyShift action_120
action_233 (77) = happyShift action_121
action_233 (78) = happyShift action_122
action_233 (81) = happyShift action_138
action_233 (82) = happyShift action_12
action_233 (83) = happyShift action_13
action_233 (84) = happyShift action_14
action_233 (85) = happyShift action_124
action_233 (86) = happyShift action_125
action_233 (99) = happyShift action_126
action_233 (101) = happyShift action_127
action_233 (103) = happyShift action_128
action_233 (6) = happyGoto action_108
action_233 (7) = happyGoto action_109
action_233 (11) = happyGoto action_257
action_233 (12) = happyGoto action_133
action_233 (13) = happyGoto action_134
action_233 (14) = happyGoto action_111
action_233 (15) = happyGoto action_112
action_233 (16) = happyGoto action_113
action_233 (58) = happyGoto action_136
action_233 _ = happyFail

action_234 (59) = happyShift action_11
action_234 (60) = happyShift action_31
action_234 (61) = happyShift action_32
action_234 (62) = happyShift action_33
action_234 (63) = happyShift action_34
action_234 (71) = happyShift action_118
action_234 (72) = happyShift action_119
action_234 (74) = happyShift action_137
action_234 (75) = happyShift action_120
action_234 (77) = happyShift action_121
action_234 (78) = happyShift action_122
action_234 (81) = happyShift action_138
action_234 (82) = happyShift action_12
action_234 (83) = happyShift action_13
action_234 (84) = happyShift action_14
action_234 (85) = happyShift action_124
action_234 (86) = happyShift action_125
action_234 (99) = happyShift action_126
action_234 (101) = happyShift action_127
action_234 (103) = happyShift action_128
action_234 (6) = happyGoto action_108
action_234 (7) = happyGoto action_109
action_234 (11) = happyGoto action_256
action_234 (12) = happyGoto action_133
action_234 (13) = happyGoto action_134
action_234 (14) = happyGoto action_111
action_234 (15) = happyGoto action_112
action_234 (16) = happyGoto action_113
action_234 (58) = happyGoto action_136
action_234 _ = happyFail

action_235 _ = happyReduce_64

action_236 _ = happyReduce_60

action_237 (88) = happyShift action_254
action_237 (108) = happyShift action_255
action_237 _ = happyFail

action_238 (59) = happyShift action_11
action_238 (60) = happyShift action_31
action_238 (61) = happyShift action_32
action_238 (62) = happyShift action_33
action_238 (63) = happyShift action_34
action_238 (82) = happyShift action_12
action_238 (83) = happyShift action_13
action_238 (84) = happyShift action_14
action_238 (99) = happyShift action_35
action_238 (103) = happyShift action_36
action_238 (7) = happyGoto action_26
action_238 (8) = happyGoto action_27
action_238 (10) = happyGoto action_28
action_238 (29) = happyGoto action_253
action_238 (58) = happyGoto action_30
action_238 _ = happyReduce_93

action_239 _ = happyReduce_43

action_240 (59) = happyShift action_11
action_240 (60) = happyShift action_31
action_240 (61) = happyShift action_32
action_240 (62) = happyShift action_33
action_240 (63) = happyShift action_34
action_240 (71) = happyShift action_118
action_240 (72) = happyShift action_119
action_240 (74) = happyShift action_137
action_240 (75) = happyShift action_120
action_240 (77) = happyShift action_121
action_240 (78) = happyShift action_122
action_240 (81) = happyShift action_138
action_240 (82) = happyShift action_12
action_240 (83) = happyShift action_13
action_240 (84) = happyShift action_14
action_240 (85) = happyShift action_124
action_240 (86) = happyShift action_125
action_240 (99) = happyShift action_126
action_240 (101) = happyShift action_127
action_240 (103) = happyShift action_128
action_240 (6) = happyGoto action_108
action_240 (7) = happyGoto action_109
action_240 (11) = happyGoto action_132
action_240 (12) = happyGoto action_133
action_240 (13) = happyGoto action_134
action_240 (14) = happyGoto action_111
action_240 (15) = happyGoto action_112
action_240 (16) = happyGoto action_113
action_240 (23) = happyGoto action_252
action_240 (24) = happyGoto action_140
action_240 (58) = happyGoto action_136
action_240 _ = happyReduce_83

action_241 (59) = happyShift action_11
action_241 (60) = happyShift action_31
action_241 (61) = happyShift action_32
action_241 (62) = happyShift action_33
action_241 (63) = happyShift action_34
action_241 (71) = happyShift action_118
action_241 (72) = happyShift action_119
action_241 (74) = happyShift action_137
action_241 (75) = happyShift action_120
action_241 (77) = happyShift action_121
action_241 (78) = happyShift action_122
action_241 (81) = happyShift action_138
action_241 (82) = happyShift action_12
action_241 (83) = happyShift action_13
action_241 (84) = happyShift action_14
action_241 (85) = happyShift action_124
action_241 (86) = happyShift action_125
action_241 (99) = happyShift action_126
action_241 (101) = happyShift action_127
action_241 (103) = happyShift action_128
action_241 (6) = happyGoto action_108
action_241 (7) = happyGoto action_109
action_241 (11) = happyGoto action_251
action_241 (12) = happyGoto action_133
action_241 (13) = happyGoto action_134
action_241 (14) = happyGoto action_111
action_241 (15) = happyGoto action_112
action_241 (16) = happyGoto action_113
action_241 (58) = happyGoto action_136
action_241 _ = happyFail

action_242 (59) = happyShift action_11
action_242 (60) = happyShift action_31
action_242 (61) = happyShift action_32
action_242 (62) = happyShift action_33
action_242 (63) = happyShift action_34
action_242 (71) = happyShift action_118
action_242 (72) = happyShift action_119
action_242 (74) = happyShift action_137
action_242 (75) = happyShift action_120
action_242 (77) = happyShift action_121
action_242 (78) = happyShift action_122
action_242 (81) = happyShift action_138
action_242 (82) = happyShift action_12
action_242 (83) = happyShift action_13
action_242 (84) = happyShift action_14
action_242 (85) = happyShift action_124
action_242 (86) = happyShift action_125
action_242 (99) = happyShift action_126
action_242 (101) = happyShift action_127
action_242 (103) = happyShift action_128
action_242 (6) = happyGoto action_108
action_242 (7) = happyGoto action_109
action_242 (11) = happyGoto action_250
action_242 (12) = happyGoto action_133
action_242 (13) = happyGoto action_134
action_242 (14) = happyGoto action_111
action_242 (15) = happyGoto action_112
action_242 (16) = happyGoto action_113
action_242 (58) = happyGoto action_136
action_242 _ = happyFail

action_243 (59) = happyShift action_11
action_243 (60) = happyShift action_31
action_243 (61) = happyShift action_32
action_243 (62) = happyShift action_33
action_243 (63) = happyShift action_34
action_243 (71) = happyShift action_118
action_243 (72) = happyShift action_119
action_243 (74) = happyShift action_137
action_243 (75) = happyShift action_120
action_243 (77) = happyShift action_121
action_243 (78) = happyShift action_122
action_243 (81) = happyShift action_138
action_243 (82) = happyShift action_12
action_243 (83) = happyShift action_13
action_243 (84) = happyShift action_14
action_243 (85) = happyShift action_124
action_243 (86) = happyShift action_125
action_243 (99) = happyShift action_126
action_243 (101) = happyShift action_127
action_243 (103) = happyShift action_128
action_243 (6) = happyGoto action_108
action_243 (7) = happyGoto action_109
action_243 (11) = happyGoto action_132
action_243 (12) = happyGoto action_133
action_243 (13) = happyGoto action_134
action_243 (14) = happyGoto action_111
action_243 (15) = happyGoto action_112
action_243 (16) = happyGoto action_113
action_243 (24) = happyGoto action_249
action_243 (58) = happyGoto action_136
action_243 _ = happyFail

action_244 _ = happyReduce_120

action_245 (110) = happyShift action_248
action_245 _ = happyFail

action_246 (83) = happyShift action_247
action_246 _ = happyFail

action_247 (59) = happyShift action_11
action_247 (82) = happyShift action_12
action_247 (83) = happyShift action_13
action_247 (84) = happyShift action_14
action_247 (36) = happyGoto action_292
action_247 (37) = happyGoto action_293
action_247 (58) = happyGoto action_294
action_247 _ = happyFail

action_248 _ = happyReduce_116

action_249 _ = happyReduce_85

action_250 _ = happyReduce_48

action_251 _ = happyReduce_49

action_252 (102) = happyShift action_291
action_252 _ = happyFail

action_253 (102) = happyShift action_290
action_253 _ = happyFail

action_254 (61) = happyShift action_289
action_254 _ = happyFail

action_255 (60) = happyShift action_288
action_255 _ = happyFail

action_256 _ = happyReduce_71

action_257 (105) = happyShift action_233
action_257 (107) = happyShift action_234
action_257 (17) = happyGoto action_287
action_257 _ = happyReduce_73

action_258 _ = happyReduce_59

action_259 (59) = happyShift action_11
action_259 (60) = happyShift action_31
action_259 (61) = happyShift action_32
action_259 (62) = happyShift action_33
action_259 (63) = happyShift action_34
action_259 (71) = happyShift action_118
action_259 (72) = happyShift action_119
action_259 (74) = happyShift action_137
action_259 (75) = happyShift action_120
action_259 (77) = happyShift action_121
action_259 (78) = happyShift action_122
action_259 (81) = happyShift action_138
action_259 (82) = happyShift action_12
action_259 (83) = happyShift action_13
action_259 (84) = happyShift action_14
action_259 (85) = happyShift action_124
action_259 (86) = happyShift action_125
action_259 (99) = happyShift action_126
action_259 (101) = happyShift action_127
action_259 (103) = happyShift action_128
action_259 (6) = happyGoto action_108
action_259 (7) = happyGoto action_109
action_259 (11) = happyGoto action_132
action_259 (12) = happyGoto action_133
action_259 (13) = happyGoto action_134
action_259 (14) = happyGoto action_111
action_259 (15) = happyGoto action_112
action_259 (16) = happyGoto action_113
action_259 (24) = happyGoto action_286
action_259 (58) = happyGoto action_136
action_259 _ = happyFail

action_260 _ = happyReduce_78

action_261 (109) = happyShift action_285
action_261 _ = happyFail

action_262 (59) = happyShift action_11
action_262 (60) = happyShift action_31
action_262 (61) = happyShift action_32
action_262 (62) = happyShift action_33
action_262 (63) = happyShift action_34
action_262 (71) = happyShift action_118
action_262 (72) = happyShift action_119
action_262 (74) = happyShift action_137
action_262 (75) = happyShift action_120
action_262 (77) = happyShift action_121
action_262 (78) = happyShift action_122
action_262 (81) = happyShift action_138
action_262 (82) = happyShift action_12
action_262 (83) = happyShift action_13
action_262 (84) = happyShift action_14
action_262 (85) = happyShift action_124
action_262 (86) = happyShift action_125
action_262 (99) = happyShift action_126
action_262 (101) = happyShift action_127
action_262 (103) = happyShift action_128
action_262 (6) = happyGoto action_108
action_262 (7) = happyGoto action_109
action_262 (11) = happyGoto action_132
action_262 (12) = happyGoto action_133
action_262 (13) = happyGoto action_134
action_262 (14) = happyGoto action_111
action_262 (15) = happyGoto action_112
action_262 (16) = happyGoto action_113
action_262 (24) = happyGoto action_284
action_262 (58) = happyGoto action_136
action_262 _ = happyFail

action_263 _ = happyReduce_81

action_264 _ = happyReduce_79

action_265 (73) = happyShift action_283
action_265 _ = happyFail

action_266 (64) = happyReduce_62
action_266 (65) = happyReduce_62
action_266 (66) = happyReduce_62
action_266 (67) = happyReduce_62
action_266 (68) = happyReduce_62
action_266 (69) = happyReduce_62
action_266 (70) = happyReduce_62
action_266 (85) = happyReduce_62
action_266 (86) = happyReduce_62
action_266 (87) = happyReduce_62
action_266 (88) = happyReduce_62
action_266 (89) = happyReduce_62
action_266 (90) = happyReduce_62
action_266 (91) = happyReduce_62
action_266 (92) = happyReduce_62
action_266 (93) = happyReduce_62
action_266 (94) = happyReduce_62
action_266 (95) = happyReduce_62
action_266 (96) = happyReduce_62
action_266 (105) = happyReduce_86
action_266 (109) = happyReduce_86
action_266 _ = happyFail

action_267 _ = happyReduce_61

action_268 (59) = happyShift action_11
action_268 (60) = happyShift action_31
action_268 (61) = happyShift action_32
action_268 (62) = happyShift action_33
action_268 (63) = happyShift action_34
action_268 (71) = happyShift action_118
action_268 (72) = happyShift action_119
action_268 (74) = happyShift action_137
action_268 (75) = happyShift action_120
action_268 (77) = happyShift action_121
action_268 (78) = happyShift action_122
action_268 (81) = happyShift action_138
action_268 (82) = happyShift action_12
action_268 (83) = happyShift action_13
action_268 (84) = happyShift action_14
action_268 (85) = happyShift action_124
action_268 (86) = happyShift action_125
action_268 (99) = happyShift action_126
action_268 (101) = happyShift action_127
action_268 (103) = happyShift action_128
action_268 (6) = happyGoto action_108
action_268 (7) = happyGoto action_109
action_268 (11) = happyGoto action_132
action_268 (12) = happyGoto action_133
action_268 (13) = happyGoto action_134
action_268 (14) = happyGoto action_111
action_268 (15) = happyGoto action_112
action_268 (16) = happyGoto action_113
action_268 (23) = happyGoto action_282
action_268 (24) = happyGoto action_140
action_268 (58) = happyGoto action_136
action_268 _ = happyReduce_83

action_269 _ = happyReduce_111

action_270 (59) = happyShift action_11
action_270 (60) = happyShift action_281
action_270 (61) = happyShift action_75
action_270 (82) = happyShift action_12
action_270 (83) = happyShift action_13
action_270 (84) = happyShift action_14
action_270 (99) = happyShift action_76
action_270 (101) = happyShift action_77
action_270 (103) = happyShift action_78
action_270 (45) = happyGoto action_278
action_270 (46) = happyGoto action_279
action_270 (51) = happyGoto action_280
action_270 (52) = happyGoto action_70
action_270 (53) = happyGoto action_71
action_270 (54) = happyGoto action_72
action_270 (58) = happyGoto action_73
action_270 _ = happyFail

action_271 _ = happyReduce_156

action_272 _ = happyReduce_158

action_273 _ = happyReduce_155

action_274 (59) = happyShift action_11
action_274 (82) = happyShift action_12
action_274 (83) = happyShift action_13
action_274 (84) = happyShift action_14
action_274 (99) = happyShift action_191
action_274 (103) = happyShift action_193
action_274 (57) = happyGoto action_277
action_274 (58) = happyGoto action_190
action_274 _ = happyFail

action_275 _ = happyReduce_159

action_276 _ = happyReduce_110

action_277 _ = happyReduce_152

action_278 (106) = happyShift action_304
action_278 _ = happyReduce_121

action_279 _ = happyReduce_124

action_280 (92) = happyShift action_302
action_280 (98) = happyShift action_303
action_280 _ = happyFail

action_281 (111) = happyShift action_301
action_281 _ = happyReduce_141

action_282 (102) = happyShift action_300
action_282 _ = happyFail

action_283 _ = happyReduce_66

action_284 (73) = happyShift action_299
action_284 _ = happyFail

action_285 (59) = happyShift action_11
action_285 (60) = happyShift action_31
action_285 (61) = happyShift action_32
action_285 (62) = happyShift action_33
action_285 (63) = happyShift action_34
action_285 (71) = happyShift action_118
action_285 (72) = happyShift action_119
action_285 (74) = happyShift action_137
action_285 (75) = happyShift action_120
action_285 (77) = happyShift action_121
action_285 (78) = happyShift action_122
action_285 (81) = happyShift action_138
action_285 (82) = happyShift action_12
action_285 (83) = happyShift action_13
action_285 (84) = happyShift action_14
action_285 (85) = happyShift action_124
action_285 (86) = happyShift action_125
action_285 (99) = happyShift action_126
action_285 (101) = happyShift action_127
action_285 (103) = happyShift action_128
action_285 (6) = happyGoto action_108
action_285 (7) = happyGoto action_109
action_285 (11) = happyGoto action_132
action_285 (12) = happyGoto action_133
action_285 (13) = happyGoto action_134
action_285 (14) = happyGoto action_111
action_285 (15) = happyGoto action_112
action_285 (16) = happyGoto action_113
action_285 (24) = happyGoto action_298
action_285 (58) = happyGoto action_136
action_285 _ = happyFail

action_286 _ = happyReduce_74

action_287 _ = happyReduce_72

action_288 (88) = happyShift action_297
action_288 _ = happyFail

action_289 _ = happyReduce_45

action_290 (109) = happyShift action_296
action_290 _ = happyFail

action_291 _ = happyReduce_62

action_292 _ = happyReduce_119

action_293 _ = happyReduce_118

action_294 (101) = happyShift action_295
action_294 _ = happyFail

action_295 (60) = happyShift action_81
action_295 (49) = happyGoto action_312
action_295 (50) = happyGoto action_80
action_295 _ = happyReduce_133

action_296 (59) = happyShift action_11
action_296 (60) = happyShift action_31
action_296 (61) = happyShift action_32
action_296 (62) = happyShift action_33
action_296 (63) = happyShift action_34
action_296 (71) = happyShift action_118
action_296 (72) = happyShift action_119
action_296 (74) = happyShift action_137
action_296 (75) = happyShift action_120
action_296 (77) = happyShift action_121
action_296 (78) = happyShift action_122
action_296 (81) = happyShift action_138
action_296 (82) = happyShift action_12
action_296 (83) = happyShift action_13
action_296 (84) = happyShift action_14
action_296 (85) = happyShift action_124
action_296 (86) = happyShift action_125
action_296 (99) = happyShift action_126
action_296 (101) = happyShift action_127
action_296 (103) = happyShift action_128
action_296 (6) = happyGoto action_108
action_296 (7) = happyGoto action_109
action_296 (11) = happyGoto action_311
action_296 (12) = happyGoto action_133
action_296 (13) = happyGoto action_134
action_296 (14) = happyGoto action_111
action_296 (15) = happyGoto action_112
action_296 (16) = happyGoto action_113
action_296 (58) = happyGoto action_136
action_296 _ = happyFail

action_297 (61) = happyShift action_310
action_297 _ = happyFail

action_298 (73) = happyShift action_309
action_298 _ = happyFail

action_299 _ = happyReduce_68

action_300 _ = happyReduce_63

action_301 (59) = happyShift action_11
action_301 (82) = happyShift action_12
action_301 (83) = happyShift action_13
action_301 (84) = happyShift action_14
action_301 (99) = happyShift action_191
action_301 (101) = happyShift action_192
action_301 (103) = happyShift action_193
action_301 (56) = happyGoto action_308
action_301 (57) = happyGoto action_189
action_301 (58) = happyGoto action_190
action_301 _ = happyFail

action_302 (98) = happyShift action_307
action_302 _ = happyFail

action_303 (59) = happyShift action_11
action_303 (60) = happyShift action_74
action_303 (61) = happyShift action_75
action_303 (82) = happyShift action_12
action_303 (83) = happyShift action_13
action_303 (84) = happyShift action_14
action_303 (99) = happyShift action_76
action_303 (101) = happyShift action_77
action_303 (103) = happyShift action_78
action_303 (51) = happyGoto action_306
action_303 (52) = happyGoto action_70
action_303 (53) = happyGoto action_71
action_303 (54) = happyGoto action_72
action_303 (58) = happyGoto action_73
action_303 _ = happyFail

action_304 (59) = happyShift action_11
action_304 (60) = happyShift action_281
action_304 (61) = happyShift action_75
action_304 (82) = happyShift action_12
action_304 (83) = happyShift action_13
action_304 (84) = happyShift action_14
action_304 (99) = happyShift action_76
action_304 (101) = happyShift action_77
action_304 (103) = happyShift action_78
action_304 (46) = happyGoto action_305
action_304 (51) = happyGoto action_280
action_304 (52) = happyGoto action_70
action_304 (53) = happyGoto action_71
action_304 (54) = happyGoto action_72
action_304 (58) = happyGoto action_73
action_304 _ = happyFail

action_305 _ = happyReduce_123

action_306 _ = happyReduce_126

action_307 (59) = happyShift action_11
action_307 (60) = happyShift action_74
action_307 (61) = happyShift action_75
action_307 (82) = happyShift action_12
action_307 (83) = happyShift action_13
action_307 (84) = happyShift action_14
action_307 (99) = happyShift action_76
action_307 (101) = happyShift action_77
action_307 (103) = happyShift action_78
action_307 (51) = happyGoto action_315
action_307 (52) = happyGoto action_70
action_307 (53) = happyGoto action_71
action_307 (54) = happyGoto action_72
action_307 (58) = happyGoto action_73
action_307 _ = happyFail

action_308 _ = happyReduce_127

action_309 _ = happyReduce_70

action_310 _ = happyReduce_46

action_311 (73) = happyShift action_314
action_311 _ = happyFail

action_312 (102) = happyShift action_313
action_312 _ = happyFail

action_313 (80) = happyShift action_270
action_313 (98) = happyShift action_197
action_313 (44) = happyGoto action_316
action_313 _ = happyReduce_122

action_314 _ = happyReduce_44

action_315 _ = happyReduce_125

action_316 _ = happyReduce_109

happyReduce_1 = happySpecReduce_2 1 reduction where {
  reduction
	(HappyAbsSyn2  happy_var_2)
	_
	 =  HappyAbsSyn1
		 (It's_a_prog   happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_2 = happySpecReduce_2 1 reduction where {
  reduction
	(HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn1
		 (It's_an_iface happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_3 = happySpecReduce_0 2 reduction where {
  reduction
	 =  HappyAbsSyn2
		 ([])}

happyReduce_4 = happySpecReduce_2 2 reduction where {
  reduction
	(HappyAbsSyn2  happy_var_2)
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn2
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_5 = happySpecReduce_1 3 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Add);
  reduction _  = notHappyAtAll }

happyReduce_6 = happySpecReduce_1 3 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Subtract);
  reduction _  = notHappyAtAll }

happyReduce_7 = happySpecReduce_1 3 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Bor);
  reduction _  = notHappyAtAll }

happyReduce_8 = happySpecReduce_1 3 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Bxor);
  reduction _  = notHappyAtAll }

happyReduce_9 = happySpecReduce_1 3 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Bsl);
  reduction _  = notHappyAtAll }

happyReduce_10 = happySpecReduce_1 3 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Bsr);
  reduction _  = notHappyAtAll }

happyReduce_11 = happySpecReduce_1 4 reduction where {
  reduction
	_
	 =  HappyAbsSyn4
		 (O_Eq);
  reduction _  = notHappyAtAll }

happyReduce_12 = happySpecReduce_1 4 reduction where {
  reduction
	_
	 =  HappyAbsSyn4
		 (O_Neq);
  reduction _  = notHappyAtAll }

happyReduce_13 = happySpecReduce_1 4 reduction where {
  reduction
	_
	 =  HappyAbsSyn4
		 (O_Leq);
  reduction _  = notHappyAtAll }

happyReduce_14 = happySpecReduce_1 4 reduction where {
  reduction
	_
	 =  HappyAbsSyn4
		 (O_Lt);
  reduction _  = notHappyAtAll }

happyReduce_15 = happySpecReduce_1 4 reduction where {
  reduction
	_
	 =  HappyAbsSyn4
		 (O_Geq);
  reduction _  = notHappyAtAll }

happyReduce_16 = happySpecReduce_1 4 reduction where {
  reduction
	_
	 =  HappyAbsSyn4
		 (O_Gt);
  reduction _  = notHappyAtAll }

happyReduce_17 = happySpecReduce_1 4 reduction where {
  reduction
	_
	 =  HappyAbsSyn4
		 (O_ExactEq);
  reduction _  = notHappyAtAll }

happyReduce_18 = happySpecReduce_1 4 reduction where {
  reduction
	_
	 =  HappyAbsSyn4
		 (O_ExactNeq);
  reduction _  = notHappyAtAll }

happyReduce_19 = happySpecReduce_1 5 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Multiply);
  reduction _  = notHappyAtAll }

happyReduce_20 = happySpecReduce_1 5 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Divide);
  reduction _  = notHappyAtAll }

happyReduce_21 = happySpecReduce_1 5 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Div);
  reduction _  = notHappyAtAll }

happyReduce_22 = happySpecReduce_1 5 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Rem);
  reduction _  = notHappyAtAll }

happyReduce_23 = happySpecReduce_1 5 reduction where {
  reduction
	_
	 =  HappyAbsSyn3
		 (O_Band);
  reduction _  = notHappyAtAll }

happyReduce_24 = happySpecReduce_1 6 reduction where {
  reduction
	_
	 =  HappyAbsSyn6
		 (O_Plus);
  reduction _  = notHappyAtAll }

happyReduce_25 = happySpecReduce_1 6 reduction where {
  reduction
	_
	 =  HappyAbsSyn6
		 (O_Negate);
  reduction _  = notHappyAtAll }

happyReduce_26 = happySpecReduce_1 6 reduction where {
  reduction
	_
	 =  HappyAbsSyn6
		 (O_Bnot);
  reduction _  = notHappyAtAll }

happyReduce_27 = happySpecReduce_1 7 reduction where {
  reduction
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn7
		 (E_Atom happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_28 = happySpecReduce_1 7 reduction where {
  reduction
	(HappyTerminal (T_Int happy_var_1))
	 =  HappyAbsSyn7
		 (E_Int happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_29 = happySpecReduce_1 7 reduction where {
  reduction
	(HappyTerminal (T_Float happy_var_1))
	 =  HappyAbsSyn7
		 (E_Float happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_30 = happySpecReduce_1 7 reduction where {
  reduction
	(HappyTerminal (T_String happy_var_1))
	 =  HappyAbsSyn7
		 (foldr E_Cons E_Nil (map (E_Int . ord) happy_var_1));
  reduction _  = notHappyAtAll }

happyReduce_31 = happySpecReduce_1 7 reduction where {
  reduction
	(HappyTerminal (T_Var happy_var_1))
	 =  HappyAbsSyn7
		 (E_Var happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_32 = happySpecReduce_1 8 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_33 = happySpecReduce_2 8 reduction where {
  reduction
	_
	_
	 =  HappyAbsSyn7
		 (E_Nil);
  reduction _ _  = notHappyAtAll }

happyReduce_34 = happyReduce 4 8 reduction where {
  reduction
	(_ :
	(HappyAbsSyn7  happy_var_3) :
	(HappyAbsSyn7  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn7
		 (E_Cons happy_var_2 happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_35 = happySpecReduce_2 8 reduction where {
  reduction
	_
	_
	 =  HappyAbsSyn7
		 (E_Tuple []);
  reduction _ _  = notHappyAtAll }

happyReduce_36 = happySpecReduce_3 8 reduction where {
  reduction
	_
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (E_Tuple happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_37 = happyReduce 4 8 reduction where {
  reduction
	(_ :
	(HappyAbsSyn10  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn7
		 (E_Struct happy_var_1 happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_38 = happySpecReduce_2 9 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_39 = happySpecReduce_3 9 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (E_Cons happy_var_2 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_40 = happySpecReduce_0 9 reduction where {
  reduction
	 =  HappyAbsSyn7
		 (E_Nil)}

happyReduce_41 = happySpecReduce_1 10 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn10
		 ([ happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_42 = happySpecReduce_3 10 reduction where {
  reduction
	(HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_43 = happySpecReduce_2 11 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (E_Catch happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_44 = happyReduce 7 11 reduction where {
  reduction
	(_ :
	(HappyAbsSyn7  happy_var_6) :
	_ :
	_ :
	(HappyAbsSyn10  happy_var_3) :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn7
		 (E_Fun happy_var_3 happy_var_6) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_45 = happyReduce 4 11 reduction where {
  reduction
	((HappyTerminal (T_Int happy_var_4)) :
	_ :
	(HappyTerminal (T_Var happy_var_2)) :
	_ :
	happyRest)
	 = HappyAbsSyn7
		 (E_FunName (LocFun happy_var_2 happy_var_4)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_46 = happyReduce 6 11 reduction where {
  reduction
	((HappyTerminal (T_Int happy_var_6)) :
	_ :
	(HappyTerminal (T_Var happy_var_4)) :
	_ :
	(HappyTerminal (T_Var happy_var_2)) :
	_ :
	happyRest)
	 = HappyAbsSyn7
		 (E_FunName (ExtFun happy_var_2 happy_var_4 happy_var_6)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_47 = happySpecReduce_1 11 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_48 = happySpecReduce_3 12 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (E_Match happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_49 = happySpecReduce_3 12 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (E_Send happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_50 = happySpecReduce_1 12 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_51 = happySpecReduce_3 13 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn3  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (E_BinOp happy_var_2 happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_52 = happySpecReduce_1 13 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_53 = happySpecReduce_3 14 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn3  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (E_BinOp happy_var_2 happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_54 = happySpecReduce_1 14 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_55 = happySpecReduce_2 15 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (E_UnOp happy_var_1 happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_56 = happySpecReduce_1 15 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_57 = happySpecReduce_1 16 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_58 = happySpecReduce_2 16 reduction where {
  reduction
	_
	_
	 =  HappyAbsSyn7
		 (E_Nil);
  reduction _ _  = notHappyAtAll }

happyReduce_59 = happyReduce 4 16 reduction where {
  reduction
	(_ :
	(HappyAbsSyn7  happy_var_3) :
	(HappyAbsSyn7  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn7
		 (E_Cons happy_var_2 happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_60 = happySpecReduce_3 16 reduction where {
  reduction
	_
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (E_Tuple happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_61 = happyReduce 4 16 reduction where {
  reduction
	(_ :
	(HappyAbsSyn10  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn7
		 (E_Struct happy_var_1 happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_62 = happyReduce 4 16 reduction where {
  reduction
	(_ :
	(HappyAbsSyn10  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn7
		 (E_Call (LocFun happy_var_1 (length happy_var_3)) happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_63 = happyReduce 6 16 reduction where {
  reduction
	(_ :
	(HappyAbsSyn10  happy_var_5) :
	_ :
	(HappyAbsSyn58  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn7
		 (E_Call (ExtFun happy_var_1 happy_var_3 (length happy_var_5)) happy_var_5) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_64 = happySpecReduce_3 16 reduction where {
  reduction
	_
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_65 = happySpecReduce_3 16 reduction where {
  reduction
	_
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (E_Block happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_66 = happyReduce 5 16 reduction where {
  reduction
	(_ :
	(HappyAbsSyn20  happy_var_4) :
	_ :
	(HappyAbsSyn7  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn7
		 (E_Case happy_var_2 happy_var_4) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_67 = happySpecReduce_3 16 reduction where {
  reduction
	_
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (E_If happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_68 = happyReduce 6 16 reduction where {
  reduction
	(_ :
	(HappyAbsSyn10  happy_var_5) :
	_ :
	(HappyAbsSyn7  happy_var_3) :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn7
		 (E_Receive [] (Just (happy_var_3,happy_var_5))) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_69 = happySpecReduce_3 16 reduction where {
  reduction
	_
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (E_Receive happy_var_2 Nothing);
  reduction _ _ _  = notHappyAtAll }

happyReduce_70 = happyReduce 7 16 reduction where {
  reduction
	(_ :
	(HappyAbsSyn10  happy_var_6) :
	_ :
	(HappyAbsSyn7  happy_var_4) :
	_ :
	(HappyAbsSyn20  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn7
		 (E_Receive happy_var_2 (Just (happy_var_4,happy_var_6))) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_71 = happySpecReduce_2 17 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_72 = happySpecReduce_3 17 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (E_Cons happy_var_2 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_73 = happySpecReduce_0 17 reduction where {
  reduction
	 =  HappyAbsSyn7
		 (E_Nil)}

happyReduce_74 = happyReduce 4 18 reduction where {
  reduction
	((HappyAbsSyn10  happy_var_4) :
	_ :
	(HappyAbsSyn19  happy_var_2) :
	(HappyAbsSyn7  happy_var_1) :
	happyRest)
	 = HappyAbsSyn18
		 ((happy_var_1,happy_var_2,happy_var_4)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_75 = happySpecReduce_2 19 reduction where {
  reduction
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_76 = happySpecReduce_0 19 reduction where {
  reduction
	 =  HappyAbsSyn19
		 ([])}

happyReduce_77 = happySpecReduce_1 20 reduction where {
  reduction
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn20
		 ([ happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_78 = happySpecReduce_3 20 reduction where {
  reduction
	(HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_79 = happySpecReduce_3 21 reduction where {
  reduction
	(HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn21
		 ((happy_var_1,happy_var_3));
  reduction _ _ _  = notHappyAtAll }

happyReduce_80 = happySpecReduce_1 22 reduction where {
  reduction
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn22
		 ([ happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_81 = happySpecReduce_3 22 reduction where {
  reduction
	(HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_82 = happySpecReduce_1 23 reduction where {
  reduction
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_83 = happySpecReduce_0 23 reduction where {
  reduction
	 =  HappyAbsSyn10
		 ([])}

happyReduce_84 = happySpecReduce_1 24 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn10
		 ([ happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_85 = happySpecReduce_3 24 reduction where {
  reduction
	(HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_86 = happyReduce 4 25 reduction where {
  reduction
	(_ :
	(HappyAbsSyn10  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn25
		 (G_Bif happy_var_1 happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_87 = happySpecReduce_3 25 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn25
		 (G_Cmp happy_var_2 happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_88 = happySpecReduce_1 26 reduction where {
  reduction
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn19
		 ([ happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_89 = happySpecReduce_3 26 reduction where {
  reduction
	(HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_90 = happySpecReduce_1 27 reduction where {
  reduction
	_
	 =  HappyAbsSyn19
		 ([]);
  reduction _  = notHappyAtAll }

happyReduce_91 = happySpecReduce_1 27 reduction where {
  reduction
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_92 = happyReduce 7 28 reduction where {
  reduction
	((HappyAbsSyn10  happy_var_7) :
	_ :
	(HappyAbsSyn19  happy_var_5) :
	_ :
	(HappyAbsSyn10  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn28
		 ((LocFun happy_var_1 (length happy_var_3),happy_var_3,happy_var_5,happy_var_7)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_93 = happySpecReduce_0 29 reduction where {
  reduction
	 =  HappyAbsSyn10
		 ([])}

happyReduce_94 = happySpecReduce_1 29 reduction where {
  reduction
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_95 = happySpecReduce_1 30 reduction where {
  reduction
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn30
		 ([ happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_96 = happySpecReduce_3 30 reduction where {
  reduction
	(HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_97 = happySpecReduce_1 31 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn31
		 (A_Pat happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_98 = happySpecReduce_3 31 reduction where {
  reduction
	_
	(HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (A_Funs happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_99 = happyReduce 5 31 reduction where {
  reduction
	(_ :
	(HappyAbsSyn32  happy_var_4) :
	_ :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn31
		 (A_AtomAndFuns happy_var_1 happy_var_4) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_100 = happySpecReduce_1 32 reduction where {
  reduction
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_101 = happySpecReduce_0 32 reduction where {
  reduction
	 =  HappyAbsSyn32
		 ([])}

happyReduce_102 = happySpecReduce_1 33 reduction where {
  reduction
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn32
		 ([ happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_103 = happySpecReduce_3 33 reduction where {
  reduction
	(HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_104 = happySpecReduce_3 34 reduction where {
  reduction
	(HappyTerminal (T_Int happy_var_3))
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn34
		 (LocFun happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_105 = happyReduce 6 35 reduction where {
  reduction
	(_ :
	_ :
	(HappyAbsSyn31  happy_var_4) :
	_ :
	(HappyAbsSyn58  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn35
		 (F_Directive happy_var_2 happy_var_4) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_106 = happyReduce 4 35 reduction where {
  reduction
	(_ :
	(HappyAbsSyn35  happy_var_3) :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn35
		 (happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_107 = happyReduce 4 35 reduction where {
  reduction
	(_ :
	(HappyAbsSyn35  happy_var_3) :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn35
		 (happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_108 = happySpecReduce_2 35 reduction where {
  reduction
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn35
		 (F_Function happy_var_1);
  reduction _ _  = notHappyAtAll }

happyReduce_109 = happyReduce 5 36 reduction where {
  reduction
	((HappyAbsSyn44  happy_var_5) :
	_ :
	(HappyAbsSyn40  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn35
		 (F_AbsTypeDef (Tycon happy_var_1 (length happy_var_3)) happy_var_3 (snd happy_var_5)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_110 = happyReduce 7 37 reduction where {
  reduction
	((HappyAbsSyn44  happy_var_7) :
	(HappyAbsSyn51  happy_var_6) :
	_ :
	_ :
	(HappyAbsSyn40  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn35
		 (F_TypeDef (Tycon happy_var_1 (length happy_var_3)) happy_var_3 happy_var_6 (fst happy_var_7) (snd happy_var_7)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_111 = happyReduce 7 38 reduction where {
  reduction
	((HappyAbsSyn44  happy_var_7) :
	(HappyAbsSyn51  happy_var_6) :
	_ :
	_ :
	(HappyAbsSyn47  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn35
		 (F_TypeSig  (happy_var_1,length happy_var_3) happy_var_3 happy_var_6 (fst happy_var_7) (snd happy_var_7)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_112 = happyReduce 4 39 reduction where {
  reduction
	(_ :
	(HappyAbsSyn47  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn39
		 ((happy_var_1, length happy_var_3, happy_var_3)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_113 = happySpecReduce_3 40 reduction where {
  reduction
	(HappyTerminal (T_Var happy_var_3))
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (STyVar happy_var_3 : happy_var_1);
  reduction _ _ _  = notHappyAtAll }

happyReduce_114 = happySpecReduce_1 40 reduction where {
  reduction
	(HappyTerminal (T_Var happy_var_1))
	 =  HappyAbsSyn40
		 ([ STyVar happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_115 = happyReduce 7 41 reduction where {
  reduction
	((HappyAbsSyn42  happy_var_7) :
	_ :
	_ :
	(HappyAbsSyn58  happy_var_4) :
	_ :
	_ :
	_ :
	happyRest)
	 = HappyAbsSyn41
		 ((happy_var_4, happy_var_7)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_116 = happySpecReduce_3 42 reduction where {
  reduction
	_
	(HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_2 : happy_var_1);
  reduction _ _ _  = notHappyAtAll }

happyReduce_117 = happySpecReduce_0 42 reduction where {
  reduction
	 =  HappyAbsSyn42
		 ([])}

happyReduce_118 = happySpecReduce_3 43 reduction where {
  reduction
	(HappyAbsSyn35  happy_var_3)
	_
	_
	 =  HappyAbsSyn35
		 (happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_119 = happySpecReduce_3 43 reduction where {
  reduction
	(HappyAbsSyn35  happy_var_3)
	_
	_
	 =  HappyAbsSyn35
		 (happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_120 = happySpecReduce_1 43 reduction where {
  reduction
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_121 = happySpecReduce_2 44 reduction where {
  reduction
	(HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn44
		 (splitConstraints happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_122 = happySpecReduce_0 44 reduction where {
  reduction
	 =  HappyAbsSyn44
		 (([],[]))}

happyReduce_123 = happySpecReduce_3 45 reduction where {
  reduction
	(HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1 ++ happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_124 = happySpecReduce_1 45 reduction where {
  reduction
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_125 = happyReduce 4 46 reduction where {
  reduction
	((HappyAbsSyn51  happy_var_4) :
	_ :
	_ :
	(HappyAbsSyn51  happy_var_1) :
	happyRest)
	 = HappyAbsSyn45
		 ([TypeCon (happy_var_1,happy_var_4)]) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_126 = happySpecReduce_3 46 reduction where {
  reduction
	(HappyAbsSyn51  happy_var_3)
	_
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn45
		 ([TypeCon (happy_var_1,happy_var_3),TypeCon(happy_var_3,happy_var_1)]);
  reduction _ _ _  = notHappyAtAll }

happyReduce_127 = happySpecReduce_3 46 reduction where {
  reduction
	(HappyAbsSyn55  happy_var_3)
	_
	(HappyTerminal (T_Var happy_var_1))
	 =  HappyAbsSyn45
		 ([VarCon (STyVar happy_var_1,(canonTags happy_var_3))]);
  reduction _ _ _  = notHappyAtAll }

happyReduce_128 = happySpecReduce_1 47 reduction where {
  reduction
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (reverse happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_129 = happySpecReduce_0 47 reduction where {
  reduction
	 =  HappyAbsSyn47
		 ([])}

happyReduce_130 = happySpecReduce_3 48 reduction where {
  reduction
	(HappyAbsSyn51  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (happy_var_3 : happy_var_1);
  reduction _ _ _  = notHappyAtAll }

happyReduce_131 = happySpecReduce_1 48 reduction where {
  reduction
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn47
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_132 = happySpecReduce_1 49 reduction where {
  reduction
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (reverse happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_133 = happySpecReduce_0 49 reduction where {
  reduction
	 =  HappyAbsSyn40
		 ([])}

happyReduce_134 = happySpecReduce_3 50 reduction where {
  reduction
	(HappyTerminal (T_Var happy_var_3))
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (STyVar happy_var_3 : happy_var_1);
  reduction _ _ _  = notHappyAtAll }

happyReduce_135 = happySpecReduce_1 50 reduction where {
  reduction
	(HappyTerminal (T_Var happy_var_1))
	 =  HappyAbsSyn40
		 ([ STyVar happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_136 = happySpecReduce_1 51 reduction where {
  reduction
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn51
		 (U (reverse happy_var_1) []);
  reduction _  = notHappyAtAll }

happyReduce_137 = happySpecReduce_3 51 reduction where {
  reduction
	(HappyAbsSyn52  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn51
		 (U (reverse happy_var_1) [happy_var_3]);
  reduction _ _ _  = notHappyAtAll }

happyReduce_138 = happySpecReduce_1 51 reduction where {
  reduction
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn51
		 (U [] [happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_139 = happySpecReduce_3 51 reduction where {
  reduction
	_
	(HappyAbsSyn51  happy_var_2)
	_
	 =  HappyAbsSyn51
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_140 = happySpecReduce_2 51 reduction where {
  reduction
	_
	_
	 =  HappyAbsSyn51
		 (U [] []);
  reduction _ _  = notHappyAtAll }

happyReduce_141 = happySpecReduce_1 52 reduction where {
  reduction
	(HappyTerminal (T_Var happy_var_1))
	 =  HappyAbsSyn52
		 (TyVar [] (STyVar happy_var_1));
  reduction _  = notHappyAtAll }

happyReduce_142 = happySpecReduce_1 52 reduction where {
  reduction
	(HappyTerminal (T_Int happy_var_1))
	 =  HappyAbsSyn52
		 (if happy_var_1 /= 1 then
						error "Illegal type variable"
					  else universalTyVar);
  reduction _  = notHappyAtAll }

happyReduce_143 = happySpecReduce_3 52 reduction where {
  reduction
	(HappyAbsSyn55  happy_var_3)
	_
	(HappyTerminal (T_Int happy_var_1))
	 =  HappyAbsSyn52
		 (if happy_var_1 /= 1 then
						error "Illegal type variable"
					  else partialUniversalTyVar happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_144 = happySpecReduce_3 53 reduction where {
  reduction
	(HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_3 : happy_var_1);
  reduction _ _ _  = notHappyAtAll }

happyReduce_145 = happySpecReduce_1 53 reduction where {
  reduction
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn53
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_146 = happySpecReduce_3 54 reduction where {
  reduction
	_
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn54
		 (conToType happy_var_1 []);
  reduction _ _ _  = notHappyAtAll }

happyReduce_147 = happyReduce 4 54 reduction where {
  reduction
	(_ :
	(HappyAbsSyn47  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn54
		 (conToType happy_var_1 (reverse happy_var_3)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_148 = happySpecReduce_1 54 reduction where {
  reduction
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn54
		 (TyAtom happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_149 = happySpecReduce_3 54 reduction where {
  reduction
	_
	(HappyAbsSyn47  happy_var_2)
	_
	 =  HappyAbsSyn54
		 (tytuple (reverse happy_var_2));
  reduction _ _ _  = notHappyAtAll }

happyReduce_150 = happyReduce 4 54 reduction where {
  reduction
	(_ :
	(HappyAbsSyn47  happy_var_3) :
	_ :
	(HappyAbsSyn58  happy_var_1) :
	happyRest)
	 = HappyAbsSyn54
		 (TyStruct happy_var_1 happy_var_3) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_151 = happySpecReduce_3 54 reduction where {
  reduction
	_
	(HappyAbsSyn51  happy_var_2)
	_
	 =  HappyAbsSyn54
		 (tylist happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_152 = happySpecReduce_3 55 reduction where {
  reduction
	(HappyAbsSyn57  happy_var_3)
	_
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_3 : happy_var_1);
  reduction _ _ _  = notHappyAtAll }

happyReduce_153 = happySpecReduce_1 55 reduction where {
  reduction
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn55
		 ([ happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_154 = happySpecReduce_1 56 reduction where {
  reduction
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn55
		 ([ happy_var_1 ]);
  reduction _  = notHappyAtAll }

happyReduce_155 = happySpecReduce_3 56 reduction where {
  reduction
	_
	(HappyAbsSyn55  happy_var_2)
	_
	 =  HappyAbsSyn55
		 (happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_156 = happySpecReduce_3 57 reduction where {
  reduction
	_
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn57
		 (conToTag happy_var_1);
  reduction _ _ _  = notHappyAtAll }

happyReduce_157 = happySpecReduce_1 57 reduction where {
  reduction
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn57
		 (TagAtom happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_158 = happySpecReduce_3 57 reduction where {
  reduction
	(HappyTerminal (T_Int happy_var_3))
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn57
		 (TagStruct happy_var_1 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_159 = happySpecReduce_3 57 reduction where {
  reduction
	_
	(HappyTerminal (T_Int happy_var_2))
	_
	 =  HappyAbsSyn57
		 (tagtuple happy_var_2);
  reduction _ _ _  = notHappyAtAll }

happyReduce_160 = happySpecReduce_2 57 reduction where {
  reduction
	_
	_
	 =  HappyAbsSyn57
		 (taglist);
  reduction _ _  = notHappyAtAll }

happyReduce_161 = happySpecReduce_1 58 reduction where {
  reduction
	(HappyTerminal (T_Atom happy_var_1))
	 =  HappyAbsSyn58
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_162 = happySpecReduce_1 58 reduction where {
  reduction
	_
	 =  HappyAbsSyn58
		 ("true");
  reduction _  = notHappyAtAll }

happyReduce_163 = happySpecReduce_1 58 reduction where {
  reduction
	_
	 =  HappyAbsSyn58
		 ("deftype");
  reduction _  = notHappyAtAll }

happyReduce_164 = happySpecReduce_1 58 reduction where {
  reduction
	_
	 =  HappyAbsSyn58
		 ("type");
  reduction _  = notHappyAtAll }

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	T_EOF -> action 114 114 (error "reading EOF!") (HappyState action) sts stk;
	T_Atom _ -> cont 59;
	T_Var _ -> cont 60;
	T_Int _ -> cont 61;
	T_Float _ -> cont 62;
	T_String _ -> cont 63;
	T_Bor -> cont 64;
	T_Bxor -> cont 65;
	T_Bsl -> cont 66;
	T_Bsr -> cont 67;
	T_Div -> cont 68;
	T_Rem -> cont 69;
	T_Band -> cont 70;
	T_Bnot -> cont 71;
	T_Begin -> cont 72;
	T_End -> cont 73;
	T_Catch -> cont 74;
	T_Case -> cont 75;
	T_Of -> cont 76;
	T_If -> cont 77;
	T_Receive -> cont 78;
	T_After -> cont 79;
	T_When -> cont 80;
	T_Fun -> cont 81;
	T_True -> cont 82;
	T_DefType -> cont 83;
	T_Type -> cont 84;
	T_Plus -> cont 85;
	T_Minus -> cont 86;
	T_Mult -> cont 87;
	T_Divide -> cont 88;
	T_Eq -> cont 89;
	T_Neq -> cont 90;
	T_Leq -> cont 91;
	T_Lt -> cont 92;
	T_Geq -> cont 93;
	T_Gt -> cont 94;
	T_ExactEq -> cont 95;
	T_ExactNeq -> cont 96;
	T_Pling -> cont 97;
	T_Equals -> cont 98;
	T_LSquare -> cont 99;
	T_RSquare -> cont 100;
	T_LParen -> cont 101;
	T_RParen -> cont 102;
	T_LCurly -> cont 103;
	T_RCurly -> cont 104;
	T_Comma -> cont 105;
	T_SemiColon -> cont 106;
	T_Bar -> cont 107;
	T_Colon -> cont 108;
	T_Arrow -> cont 109;
	T_Dot -> cont 110;
	T_BackSlash -> cont 111;
	T_Prog -> cont 112;
	T_Interface -> cont 113;
	})

happyThen = thenP
happyReturn = returnP
parse = happyParse


utypeToVar (U [] [TyVar [] x]) = x
utypeToVar _ = error "Type constructor arguments must be variables\n"

happyError :: P a
happyError s line = failP (show line ++ ": Parse error\n") s line
-- $Id: ErlParser.ly,v 1.1 1997/09/24 10:10:44 simonm Exp $

{-
	The stack is in the following order throughout the parse:

	i	current token number
	j	another copy of this to avoid messing with the stack
	tk	current token semantic value
	st	current state
	sts	state stack
	stk	semantic stack
-}

-----------------------------------------------------------------------------

happyParse = happyNewToken action_0 [] []

-- All this HappyState stuff is simply because we can't have recursive
-- types in Haskell without an intervening data structure.

newtype HappyState b c = HappyState
        (Int ->                         -- token number
         Int ->                         -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts [ HappyAbsSyn1 ans ] = happyReturn ans
happyAccept j tk st sts _                    = notHappyAtAll

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (-1) tk st sts stk@(HappyErrorToken i : _) =
--     _trace "shifting the error token" $
     new_state i i tk (HappyState new_state) (st:sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (st:sts) (HappyTerminal tk:stk)

-----------------------------------------------------------------------------
-- Reducing

-- happyReduce is specialised for the common cases.

-- don't allow reductions when we're in error recovery, because this can
-- lead to an infinite loop.

happySpecReduce_0 i fn (-1) tk _ sts stk
     = case sts of
	st@(HappyState action):sts -> action (-1) (-1) tk st sts stk
	_ -> happyError
happySpecReduce_0 i fn j tk st@(HappyState action) sts stk
     = action i j tk st (st:sts) (fn : stk)

happySpecReduce_1 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_1 i fn j tk _ sts@(st@(HappyState action):_) (v1:stk')
     = action i j tk st sts (fn v1 : stk')

happySpecReduce_2 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_2 i fn j tk _ (_:sts@(st@(HappyState action):_)) (v1:v2:stk')
     = action i j tk st sts (fn v1 v2 : stk')

happySpecReduce_3 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_3 i fn j tk _ (_:_:sts@(st@(HappyState action):_)) 
	(v1:v2:v3:stk')
     = action i j tk st sts (fn v1 v2 v3 : stk')

happyReduce k i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happyReduce k i fn j tk st sts stk = action i j tk st' sts' (fn stk)
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)

happyMonadReduce k i c fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happyMonadReduce k i c fn j tk st sts stk =
	happyThen (fn stk) (\r -> action i j tk st' sts' (c r : stk'))
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)
	     stk' = drop (k::Int) stk

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto action j tk st = action j j tk (HappyState action)

-----------------------------------------------------------------------------
-- Error recovery (-1 is the error token)

-- fail if we are in recovery and no more states to discard
happyFail  (-1) tk st' [] stk = happyError

-- discard a state
happyFail  (-1) tk st' (st@(HappyState action):sts) stk =
--	_trace "discarding state" $
	action (-1) (-1) tk st sts stk

-- Enter error recovery: generate an error token,
-- 			 save the old token and carry on.

-- we push the error token on the stack in anticipation of a shift,
-- and also because this is a convenient place to store the saved token.

happyFail  i tk st@(HappyState action) sts stk =
--	_trace "entering error recovery" $
	action (-1) (-1) tk st sts (HappyErrorToken i : stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-- end of Happy Template.
