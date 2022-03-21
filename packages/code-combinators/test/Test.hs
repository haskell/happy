import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Haskell.TH as TH
import Test.CodeCombinators.GenExp
import Language.Haskell.Meta.Parse
import Data.Either

prop_exp :: Property
prop_exp =
  property $ do
    exp <- forAll genExp
    let parse_result = parseExp (expToString exp)
    assert $ isRight parse_result
    let Right result = parse_result
    exp === deleteParensE result

main :: IO Bool
main = checkParallel $$(discover)

