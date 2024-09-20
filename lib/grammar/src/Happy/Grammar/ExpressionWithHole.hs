module Happy.Grammar.ExpressionWithHole where

-- | The overall expression is
-- 'tokLeft ++ substitutedForHole ++ tokRight'.
data ExpressionWithHole
      = ExpressionWithHole {
              exprLeft :: String,
              exprRight :: String
      }
      deriving (Eq, Show)

substExpressionWithHole :: ExpressionWithHole -> String -> String
substExpressionWithHole (ExpressionWithHole l r) = \repr -> l ++ repr ++ r
