{

-- (c) 2004 University of Durham, Julia Fischer
-- Portions of the grammar are derived from work by Leung/Mellish/Robertson

import Char
}

%tokentype { Token }

%token
	a 	{ Base_A }
	c 	{ Base_C }
	g 	{ Base_G }
	t 	{ Base_T }

%lexer { lexer } { TokenEOF } 

%%

M 
 :  Intergenic_noise Match Intergenic_noise {}				
		-- replace NSkip by Intergenic_noise?

Intergenic_noise 
 : {}
 | Intergenic_noise N {}		-- Left-assoc, less stack?

Match  
 : Promoter Translation {} 

Promoter :: {Int}
 : Promoter_consensus {1}
 | Promoter_hcv_large {2}
 | Promoter_cart {3}
 | Promoter_hcv_small {4}


--------------------
-- HCV SMALL
--------------------
-- regions [data from Leung (hvc_small.gr)]
Promoter_hcv_small 
 : N V N7_skip K B K N20_skip R N12_skip  {}	--mod 3 = 0
 | K N B N N D N18_skip H N9_skip V N     {}	--mod 3 = 0
 | t N20_skip N6_skip t N4_skip t N6_skip {}	--mod 3 = 0



--------------------
-- CONSENSUS
--------------------
-- regions [data from Leung (consensus.gr)]

Promoter_consensus 
 : Minus_35 N15_skip Minus_10 {}
 | Minus_35 N15_skip N1_skip Minus_10 N5_skip {}
 | Minus_35 N15_skip N2_skip Minus_10 N5_skip {}
 | Minus_35 N15_skip N3_skip Minus_10 N5_skip {}
 | Minus_35 N15_skip N4_skip Minus_10 N5_skip {}

Minus_35  
 : t t g a c a {}

Minus_10  
 : t a t a a t {}

--------------------
-- HVC LARGE
--------------------
-- regions [data from Leung (hvc_large.gr)]
    
Promoter_hcv_large 
 : H N11_skip D Y B N3_skip H N12_skip B N5_skip Y N2_skip W N4_skip {}
 | N D N3_skip V N1_skip B N12_skip H N2_skip B D N2_skip H N2_skip H B N4_skip W N6_skip H H {}
 | N H N B N D N6_skip H N4_skip K B N6_skip D B N3_skip B N4_skip V N4_skip H N2_skip D N7_skip {}
 | N N D N12_skip B D N2_skip V N2_skip H D N2_skip D H B N7_skip B D N5_skip H H N6_skip {}
 | D N D N12_skip B N5_skip H N13_skip B N H H W N6_skip H Y {}
 | N N D N B N D N H N3_skip D N4_skip V N2_skip H N D H N6_skip H N3_skip D N6_skip H N2_skip B N3_skip {}
 | D N8_skip H N1_skip H N1_skip D N4_skip H N3_skip V H N11_skip H N2_skip H N5_skip D N1_skip V N1_skip H {}
 | H N3_skip B N9_skip H N12_skip H D N4_skip W B N2_skip D D H N1_skip D N5_skip D H   {}
 | V N7_skip V N2_skip D N2_skip D N6_skip B H N11_skip D D N1_skip H N1_skip H H N1_skip B N2_skip   {}
 | D N8_skip B D D N2_skip B N6_skip H N4_skip D N5_skip D N1_skip H D N2_skip D N3_skip D D N6_skip   {}
 | B N13_skip H N1_skip D H V N14_skip B N1_skip V N2_skip D N1_skip D V D N1_skip D N3_skip H   {}
 | H V N4_skip B N1_skip D N6_skip D N4_skip D N4_skip H H N3_skip B N6_skip B N1_skip D N3_skip D N1_skip D N4_skip {}
 | W N3_skip V N9_skip D N11_skip B N1_skip D H N5_skip D H N1_skip D N1_skip H D N6_skip   {}
 | K N2_skip D N3_skip H N1_skip H N6_skip H N2_skip B N5_skip D D N7_skip V N2_skip D N1_skip H H N7_skip   {}
 | D N11_skip H D D N2_skip D N6_skip D N3_skip H N6_skip V N1_skip D D N2_skip H B N1_skip B N1_skip   {}            
 | H N3_skip B N1_skip H N6_skip V N1_skip B N2_skip V N2_skip D N7_skip B N8_skip H N3_skip H D N1_skip H N1_skip H N1_skip   {}
 | B N4_skip B N12_skip H N4_skip V N2_skip H D N2_skip V H N1_skip H N2_skip H N3_skip B N1_skip K N4_skip   {}              
 | W D N7_skip B N1_skip D N2_skip D N2_skip W N1_skip D H N2_skip D N12_skip D N5_skip H   {}
 | a N2_skip t N4_skip g N18_skip {}

--------------------
-- CART
--------------------
-- regions [data from Leung (cart.gr)]

Promoter_cart 
 : N N t a N N N N N N N N N N N {}
 | N N V a N N N t N N N N N N N {}
 | t N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N t B N N N t N N N N N N N t N N N N N N N {}


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

Translation 
 : Start Mincodon Stop {}
 | Start Mincodon Codon Stop {}
 | Start Mincodon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Stop {}
 | Start Mincodon Mincodon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Stop {}
 | Start Mincodon Mincodon Mincodon Mincodon Mincodon Mincodon Stop {} --252 Basen


Mincodon : Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon Codon {} --42 Basen




N0_skip 
 : {}
N1_skip 
 : N {}				-- match starts one place on
N2_skip 
 : N N {}			-- match starts two places on
N3_skip  
 : N N N {}			-- missing an entire codon
N4_skip 
 : N N N N {}			-- missing 4 bases
N5_skip 
 : N N N N N {}			-- missing 5 bases
N6_skip 
 : N N N N N N {}		-- missing 6 bases
N7_skip 
 : N N N N N N N {}		-- missing 8 bases
N8_skip 
 : N N N N N N N N {}		-- missing 7 bases
N9_skip 
 : N N N N N N N N N {}		-- missing 9 bases
N10_skip 
 : N N N N N N N N N N {}	-- missing 10 bases
N11_skip 
 : N10_skip N1_skip {}		-- missing 11 bases
N12_skip 
 : N10_skip N2_skip {}		-- missing 12 bases
N13_skip 
 : N10_skip N3_skip {}		-- missing 13 bases
N14_skip 
 : N10_skip N4_skip {}		-- missing 14 bases
N15_skip 
 : N10_skip N5_skip {}		-- missing 15 bases
N16_skip 
 : N10_skip N6_skip {}		-- missing 16 bases
N17_skip 
 : N10_skip N7_skip {}		-- missing 17 bases
N18_skip 
 : N10_skip N8_skip {}		-- missing 18 bases
N19_skip 
 : N10_skip N9_skip {}		-- missing 19 bases
N20_skip 
 : N10_skip N10_skip {}		-- missing 20 bases
N30_skip 
 : N10_skip N10_skip N10_skip {}		-- missing 30 bases
N40_skip 
 : N10_skip N10_skip N10_skip N10_skip {}		-- missing 40 bases
N50_skip 
 : N10_skip N10_skip N10_skip N10_skip N10_skip {}		-- missing 50 bases
N60_skip 
 : N10_skip N50_skip {}		-- missing 40 bases
N70_skip 
 : N10_skip N10_skip N50_skip {}		-- missing 50 bases
N80_skip 
 : N10_skip N10_skip N10_skip N50_skip {}		-- missing 40 bases
N90_skip 
 : N10_skip N10_skip N10_skip N10_skip N50_skip{}		-- missing 50 bases
N100_skip 
 : N50_skip N50_skip {}		



--  Definitions of base categories according to the
--  International Union of Biochemistry (IUB)
--  Standard Nucleotide Codes. [Leung_data]

N			-- any base
 : a {}
 | c {}
 | g {}
 | t {}

Y			-- pyrimidin
 : c {}
 | t {}

R			-- purine
 : a {}
 | g {}

S			-- strong bonding bases
 : g {}
 | c {}

W			-- weak bonding bases
 : a {}
 | t {}

K			-- keto bases
 : g {}
 | t {}

AM			-- aMino bases
 : a {}
 | c {}

B			-- not base a
 : g {}
 | c {}
 | t {}

D			-- not base c
 : a {}
 | g {}
 | t {}

H			-- not base g
 : a {}
 | c {}
 | t {}

V			-- not base t
 : a {}
 | c {}
 | g {}



Base 
 : a {}
 | c {}
 | g {}
 | t {}

--------------------
-- codons

Start : a t g {}		-- start codon

Stop				-- stop codons
 : t a a {}
 | t a g {}
 | t g a {}

Codon 				-- any other codon
 : a a a {}
 | a a c {}
 | a a g {}
 | a a t {}
 | a c a {}
 | a c c {}
 | a c g {}
 | a c t {}
 | a g a {}
 | a g c {}
 | a g g {}
 | a g t {}
 | a t a {}
 | a t c {}
 | a t g {}
 | a t t {}
 | c a a {}
 | c a c {}
 | c a g {}
 | c a t {}
 | c c a {}
 | c c c {}
 | c c g {}
 | c c t {}
 | c g a {}
 | c g c {}
 | c g g {}
 | c g t {}
 | c t a {}
 | c t c {}
 | c t g {}
 | c t t {}
 | g a a {}
 | g a c {}
 | g a g {}
 | g a t {}
 | g c a {}
 | g c c {}
 | g c g {}
 | g c t {}
 | g g a {}
 | g g c {}
 | g g g {}
 | g g t {}
 | g t a {}
 | g t c {}
 | g t g {}
 | g t t {}
 | t a c {}
 | t a t {}
 | t c a {}
 | t c c {}
 | t c g {}
 | t c t {}
 | t g c {}
 | t g g {}
 | t g t {}
 | t t a {}
 | t t c {}
 | t t g {}
 | t t t {}

--------------------



--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--some aux code
{

data Token
	= TokenEOF
	| Base_A
	| Base_C
	| Base_G
	| Base_T
  deriving (Show,Eq, Ord)


lexer :: String -> [Token]
lexer [] = []
lexer (' ':cs) = lexer cs
lexer ('\n':cs) = lexer cs
lexer ('a':cs) = Base_A : lexer cs
lexer ('c':cs) = Base_C : lexer cs
lexer ('g':cs) = Base_G : lexer cs
lexer ('t':cs) = Base_T : lexer cs

}
