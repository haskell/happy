
module Scan(lx_lx,literate) where

import Alex

scrap :: GTokenAction () String
scrap _ _ inp len cont st = strip len inp
	where
	strip 0 _ = cont st
	strip (n+1) (c:rst) =
		if c=='\n'
		   then '\n':strip_nl n rst
		   else c:strip n rst
	strip _ _ = error "scrap"

	strip_nl (n+1) ('>':rst) = ' ':strip n rst
	strip_nl n rst = strip n rst

comment :: GTokenAction () String
comment _ _ inp len cont st = strip len inp
	where
	strip 0 _ = cont st
	strip (n+1) (c:rst) = if c=='\n' then c:strip n rst else strip n rst
	strip _ _ = error "comment"


literate:: String -> String
literate inp = drop 2 (gscan lit_scan () ('\n':'\n':inp))

lit_scan:: GScan () String
lit_scan = load_gscan (lit_acts,stop_act) lit_lx
	where
	stop_act p _ "" st = []
	stop_act p _ _  _  = error (msg ++ loc p ++ "\n")

	msg  = "literate preprocessing error at "

	loc (Pn _ l c) = "line " ++ show(l-2) ++ ", column " ++ show c

lx_lx :: [(Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))]
lx_lx = [lx__0_0,lx__1_0,lx__2_0,lx__3_0,lx__4_0,lx__5_0,lx__6_0,lx__7_0,lx__8_0,lx__9_0,lx__10_0,lx__11_0,lx__12_0,lx__13_0,lx__14_0,lx__15_0,lx__16_0,lx__17_0,lx__18_0,lx__19_0,lx__20_0,lx__21_0,lx__22_0,lx__23_0,lx__24_0,lx__25_0,lx__26_0,lx__27_0,lx__28_0,lx__29_0,lx__30_0,lx__31_0,lx__32_0,lx__33_0,lx__34_0,lx__35_0,lx__36_0,lx__37_0,lx__38_0,lx__39_0,lx__40_0,lx__41_0,lx__42_0,lx__43_0,lx__44_0,lx__45_0,lx__46_0,lx__47_0,lx__48_0,lx__49_0,lx__50_0,lx__51_0,lx__52_0]
lx__0_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__0_0 = (False,[],-1,(('\t','~'),[('\t',1),('\n',1),('\v',1),('\f',1),('\r',1),(' ',1),('"',29),('#',41),('$',41),('%',4),('(',41),(')',41),('*',41),('+',41),(',',41),('-',2),('.',48),('/',41),('0',42),('1',42),('2',42),('3',42),('4',42),('5',42),('6',42),('7',42),('8',42),('9',42),(':',37),('<',34),('=',41),('?',41),('A',42),('B',42),('C',42),('D',42),('E',42),('F',42),('G',42),('H',42),('I',42),('J',42),('K',42),('L',42),('M',42),('N',42),('O',42),('P',42),('Q',42),('R',42),('S',42),('T',42),('U',42),('V',42),('W',42),('X',42),('Y',42),('Z',42),('[',41),('\\',41),(']',41),('^',43),('`',50),('a',42),('b',42),('c',42),('d',42),('e',42),('f',42),('g',42),('h',42),('i',42),('j',42),('k',42),('l',42),('m',42),('n',42),('o',42),('p',42),('q',42),('r',42),('s',42),('t',42),('u',42),('v',42),('w',42),('x',42),('y',42),('z',42),('{',41),('|',41),('}',41),('~',41)]))
lx__1_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__1_0 = (True,[(0,"",[],Nothing,Nothing)],-1,(('\t',' '),[('\t',1),('\n',1),('\v',1),('\f',1),('\r',1),(' ',1)]))
lx__2_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__2_0 = (True,[(8,"spe",[],Nothing,Nothing)],-1,(('-','-'),[('-',3)]))
lx__3_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__3_0 = (True,[(1,"",[],Nothing,Nothing)],3,(('\n','\n'),[('\n',-1)]))
lx__4_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__4_0 = (False,[],-1,(('A','{'),[('A',49),('B',49),('C',49),('D',49),('E',49),('F',49),('G',49),('H',49),('I',49),('J',49),('K',49),('L',49),('M',49),('N',49),('O',49),('P',49),('Q',49),('R',49),('S',49),('T',49),('U',49),('V',49),('W',49),('X',49),('Y',49),('Z',49),('a',49),('b',49),('c',49),('d',49),('e',49),('f',49),('g',49),('h',49),('i',49),('j',49),('k',49),('l',49),('m',49),('n',49),('o',49),('p',49),('q',49),('r',49),('s',49),('t',49),('u',49),('v',49),('w',49),('x',49),('y',49),('z',49),('{',6)]))
lx__5_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__5_0 = (False,[],5,(('\n','%'),[('\n',-1),('%',7)]))
lx__6_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__6_0 = (False,[],5,(('\t','%'),[('\t',6),('\n',9),('\v',6),('\f',6),('\r',6),(' ',6),('%',7)]))
lx__7_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__7_0 = (False,[],5,(('\n','}'),[('\n',-1),('}',28)]))
lx__8_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__8_0 = (False,[],10,(('\n','%'),[('\n',8),('%',17)]))
lx__9_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__9_0 = (False,[],11,(('\n','%'),[('\n',9),(' ',12),('%',18)]))
lx__10_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__10_0 = (False,[],10,(('\n','\n'),[('\n',8)]))
lx__11_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__11_0 = (False,[],11,(('\n','\n'),[('\n',9)]))
lx__12_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__12_0 = (False,[],13,(('\n','%'),[('\n',9),('%',14)]))
lx__13_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__13_0 = (False,[],13,(('\n','\n'),[('\n',9)]))
lx__14_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__14_0 = (False,[],15,(('\n','}'),[('\n',9),('}',16)]))
lx__15_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__15_0 = (False,[],15,(('\n','\n'),[('\n',9)]))
lx__16_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__16_0 = (True,[(2,"code",[],Nothing,Nothing)],10,(('\n','\n'),[('\n',8)]))
lx__17_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__17_0 = (False,[],19,(('\n','}'),[('\n',8),('}',28)]))
lx__18_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__18_0 = (False,[],20,(('\n','}'),[('\n',9),('}',23)]))
lx__19_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__19_0 = (False,[],19,(('\n','\n'),[('\n',8)]))
lx__20_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__20_0 = (False,[],20,(('\n','\n'),[('\n',9)]))
lx__21_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__21_0 = (False,[],22,(('\n',' '),[('\n',21),(' ',24)]))
lx__22_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__22_0 = (False,[],22,(('\n','\n'),[('\n',21)]))
lx__23_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__23_0 = (True,[(2,"code",[],Nothing,Nothing)],22,(('\n','\n'),[('\n',21)]))
lx__24_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__24_0 = (False,[],25,(('\n','%'),[('\n',21),('%',26)]))
lx__25_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__25_0 = (False,[],25,(('\n','\n'),[('\n',21)]))
lx__26_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__26_0 = (False,[],27,(('\n','}'),[('\n',21),('}',28)]))
lx__27_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__27_0 = (False,[],27,(('\n','\n'),[('\n',21)]))
lx__28_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__28_0 = (True,[(2,"code",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__29_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__29_0 = (False,[],-1,(('0','z'),[('0',30),('a',32),('b',32),('c',32),('d',32),('e',32),('f',32),('g',32),('h',32),('i',32),('j',32),('k',32),('l',32),('m',32),('n',32),('o',32),('p',32),('q',32),('r',32),('s',32),('t',32),('u',32),('v',32),('w',32),('x',32),('y',32),('z',32)]))
lx__30_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__30_0 = (False,[],-1,(('"','"'),[('"',31)]))
lx__31_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__31_0 = (True,[(3,"zero",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__32_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__32_0 = (False,[],-1,(('"','z'),[('"',33),('\'',32),('0',32),('1',32),('2',32),('3',32),('4',32),('5',32),('6',32),('7',32),('8',32),('9',32),('A',32),('B',32),('C',32),('D',32),('E',32),('F',32),('G',32),('H',32),('I',32),('J',32),('K',32),('L',32),('M',32),('N',32),('O',32),('P',32),('Q',32),('R',32),('S',32),('T',32),('U',32),('V',32),('W',32),('X',32),('Y',32),('Z',32),('_',32),('a',32),('b',32),('c',32),('d',32),('e',32),('f',32),('g',32),('h',32),('i',32),('j',32),('k',32),('l',32),('m',32),('n',32),('o',32),('p',32),('q',32),('r',32),('s',32),('t',32),('u',32),('v',32),('w',32),('x',32),('y',32),('z',32)]))
lx__33_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__33_0 = (True,[(4,"ide",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__34_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__34_0 = (False,[],-1,(('>','z'),[('>',36),('a',35),('b',35),('c',35),('d',35),('e',35),('f',35),('g',35),('h',35),('i',35),('j',35),('k',35),('l',35),('m',35),('n',35),('o',35),('p',35),('q',35),('r',35),('s',35),('t',35),('u',35),('v',35),('w',35),('x',35),('y',35),('z',35)]))
lx__35_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__35_0 = (False,[],-1,(('\'','z'),[('\'',35),('0',35),('1',35),('2',35),('3',35),('4',35),('5',35),('6',35),('7',35),('8',35),('9',35),('>',36),('A',35),('B',35),('C',35),('D',35),('E',35),('F',35),('G',35),('H',35),('I',35),('J',35),('K',35),('L',35),('M',35),('N',35),('O',35),('P',35),('Q',35),('R',35),('S',35),('T',35),('U',35),('V',35),('W',35),('X',35),('Y',35),('Z',35),('_',35),('a',35),('b',35),('c',35),('d',35),('e',35),('f',35),('g',35),('h',35),('i',35),('j',35),('k',35),('l',35),('m',35),('n',35),('o',35),('p',35),('q',35),('r',35),('s',35),('t',35),('u',35),('v',35),('w',35),('x',35),('y',35),('z',35)]))
lx__36_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__36_0 = (True,[(5,"tkn",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__37_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__37_0 = (True,[(8,"spe",[],Nothing,Nothing)],-1,(('-',':'),[('-',38),(':',39)]))
lx__38_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__38_0 = (True,[(6,"bnd",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__39_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__39_0 = (False,[],-1,(('=','='),[('=',40)]))
lx__40_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__40_0 = (True,[(7,"prd",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__41_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__41_0 = (True,[(8,"spe",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__42_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__42_0 = (True,[(9,"ch",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__43_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__43_0 = (False,[],-1,((' ','~'),[(' ',44),('!',44),('"',44),('#',44),('$',44),('%',44),('&',44),('\'',44),('(',44),(')',44),('*',44),('+',44),(',',44),('-',44),('.',44),('/',44),('0',45),('1',45),('2',45),('3',45),('4',45),('5',45),('6',45),('7',45),('8',45),('9',45),(':',44),(';',44),('<',44),('=',44),('>',44),('?',44),('@',44),('A',48),('B',48),('C',48),('D',48),('E',48),('F',48),('G',48),('H',48),('I',48),('J',48),('K',48),('L',48),('M',48),('N',48),('O',48),('P',48),('Q',48),('R',48),('S',48),('T',48),('U',48),('V',48),('W',48),('X',48),('Y',48),('Z',48),('[',44),('\\',44),(']',44),('^',44),('_',44),('`',44),('a',48),('b',48),('c',48),('d',48),('e',48),('f',48),('g',48),('h',48),('i',48),('j',48),('k',48),('l',48),('m',48),('n',48),('o',48),('p',48),('q',48),('r',48),('s',48),('t',48),('u',48),('v',48),('w',48),('x',48),('y',48),('z',48),('{',44),('|',44),('}',44),('~',44)]))
lx__44_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__44_0 = (True,[(10,"ech",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__45_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__45_0 = (True,[(11,"cch",[],Nothing,Nothing)],-1,(('0','9'),[('0',46),('1',46),('2',46),('3',46),('4',46),('5',46),('6',46),('7',46),('8',46),('9',46)]))
lx__46_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__46_0 = (True,[(11,"cch",[],Nothing,Nothing)],-1,(('0','9'),[('0',47),('1',47),('2',47),('3',47),('4',47),('5',47),('6',47),('7',47),('8',47),('9',47)]))
lx__47_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__47_0 = (True,[(11,"cch",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__48_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__48_0 = (True,[(12,"smac",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__49_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__49_0 = (True,[(13,"rmac",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__50_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__50_0 = (False,[],-1,((' ','~'),[(' ',51),('!',51),('"',51),('#',51),('$',51),('%',51),('&',51),('(',51),(')',51),('*',51),('+',51),(',',51),('-',51),('.',51),('/',51),('0',51),('1',51),('2',51),('3',51),('4',51),('5',51),('6',51),('7',51),('8',51),('9',51),(':',51),(';',51),('<',51),('=',51),('>',51),('?',51),('@',51),('A',51),('B',51),('C',51),('D',51),('E',51),('F',51),('G',51),('H',51),('I',51),('J',51),('K',51),('L',51),('M',51),('N',51),('O',51),('P',51),('Q',51),('R',51),('S',51),('T',51),('U',51),('V',51),('W',51),('X',51),('Y',51),('Z',51),('[',51),('\\',51),(']',51),('^',51),('_',51),('`',51),('a',51),('b',51),('c',51),('d',51),('e',51),('f',51),('g',51),('h',51),('i',51),('j',51),('k',51),('l',51),('m',51),('n',51),('o',51),('p',51),('q',51),('r',51),('s',51),('t',51),('u',51),('v',51),('w',51),('x',51),('y',51),('z',51),('{',51),('|',51),('}',51),('~',51)]))
lx__51_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__51_0 = (False,[],-1,((' ','~'),[(' ',51),('!',51),('"',51),('#',51),('$',51),('%',51),('&',51),('\'',52),('(',51),(')',51),('*',51),('+',51),(',',51),('-',51),('.',51),('/',51),('0',51),('1',51),('2',51),('3',51),('4',51),('5',51),('6',51),('7',51),('8',51),('9',51),(':',51),(';',51),('<',51),('=',51),('>',51),('?',51),('@',51),('A',51),('B',51),('C',51),('D',51),('E',51),('F',51),('G',51),('H',51),('I',51),('J',51),('K',51),('L',51),('M',51),('N',51),('O',51),('P',51),('Q',51),('R',51),('S',51),('T',51),('U',51),('V',51),('W',51),('X',51),('Y',51),('Z',51),('[',51),('\\',51),(']',51),('^',51),('_',51),('`',51),('a',51),('b',51),('c',51),('d',51),('e',51),('f',51),('g',51),('h',51),('i',51),('j',51),('k',51),('l',51),('m',51),('n',51),('o',51),('p',51),('q',51),('r',51),('s',51),('t',51),('u',51),('v',51),('w',51),('x',51),('y',51),('z',51),('{',51),('|',51),('}',51),('~',51)]))
lx__52_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__52_0 = (True,[(14,"quot",[],Nothing,Nothing)],-1,(('0','0'),[]))


lit_acts = [("comment",comment),("scrap",scrap)]

lit_lx :: [(Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))]
lit_lx = [lx__0_1,lx__1_1,lx__2_1,lx__3_1,lx__4_1,lx__5_1,lx__6_1,lx__7_1,lx__8_1]
lx__0_1 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__0_1 = (False,[],-1,(('\n','\n'),[('\n',1)]))
lx__1_1 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__1_1 = (True,[(1,"comment",[],Nothing,Nothing)],-1,(('\t',' '),[('\t',1),('\n',4),('\v',1),('\f',1),('\r',1),(' ',1)]))
lx__2_1 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__2_1 = (True,[(0,"scrap",[],Nothing,Nothing)],2,(('\n','\n'),[('\n',3)]))
lx__3_1 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__3_1 = (False,[],-1,(('>','>'),[('>',2)]))
lx__4_1 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__4_1 = (False,[],5,(('\t','>'),[('\t',8),('\n',-1),('\v',8),('\f',8),('\r',8),(' ',8),('>',2)]))
lx__5_1 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__5_1 = (True,[(1,"comment",[],Nothing,Nothing)],5,(('\n','\n'),[('\n',7)]))
lx__6_1 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__6_1 = (True,[(1,"comment",[],Nothing,Nothing)],6,(('\n','\n'),[('\n',7)]))
lx__7_1 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__7_1 = (False,[],5,(('\t','>'),[('\t',8),('\n',-1),('\v',8),('\f',8),('\r',8),(' ',8),('>',-1)]))
lx__8_1 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__8_1 = (False,[],6,(('\t',' '),[('\t',8),('\n',-1),('\v',8),('\f',8),('\r',8),(' ',8)]))

