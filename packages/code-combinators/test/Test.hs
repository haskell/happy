import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Haskell.TH as TH
import Test.CodeCombinators.GenExp
import Test.CodeCombinators.GenPat
import Test.CodeCombinators.GenType
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

prop_pat :: Property
prop_pat =
  property $ do
    pat <- forAll genPat
    let parse_result = parsePat (patToString pat)
    assert $ isRight parse_result
    let Right result = parse_result
    pat === deleteParensP result

prop_type :: Property
prop_type =
  property $ do
    pat <- forAll genType
    let parse_result = parseType (typeToString pat)
    assert $ isRight parse_result
    let Right result = parse_result
    pat === deleteParensT result

main :: IO Bool
main = checkParallel $$(discover)

