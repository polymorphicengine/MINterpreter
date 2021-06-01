module FormattingSpec where

import Formatting
import ParseMINI
import Prettyprinter(pretty)
import Text.Parsec(parse)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck(Arbitrary, arbitrary, Property, suchThat, (===), generate, quickCheck, oneof)
import Test.QuickCheck.Gen(Gen)
import Generic.Random

main :: IO ()
main = hspec spec

--for simplicity only variables of length one and don't contain digits or '_'
instance Arbitrary Var where
  arbitrary = fmap Var (suchThat arbitrary (\s -> (all (\c -> isSmallLetter c) s) && (length s == 1) ))

instance Arbitrary Call where
  arbitrary = genericArbitrary uniform

instance Arbitrary ExpressionNested where
  arbitrary =  oneof  [ECall <$> arbitrary, ENum <$> (suchThat arbitrary (\i -> i >= 0)), Exp <$> arbitrary]

instance Arbitrary Expression where
  arbitrary = genericArbitrary' uniform `withBaseCase` (return (Pos $ ENum 1))

instance Arbitrary Operator where
  arbitrary = genericArbitrary uniform

instance Arbitrary Relator where
  arbitrary = genericArbitrary uniform

instance Arbitrary Boolean where
  arbitrary = genericArbitrary uniform

instance Arbitrary Assign where
  arbitrary = genericArbitrary uniform

instance Arbitrary If where
  arbitrary = genericArbitrary uniform

instance Arbitrary While where
  arbitrary = genericArbitrary uniform

instance Arbitrary Print where
  arbitrary = genericArbitrary uniform

instance Arbitrary ReadSt where
  arbitrary = genericArbitrary uniform

instance Arbitrary Statement where
  arbitrary = genericArbitrary uniform

instance Arbitrary Statements where
  arbitrary = genericArbitrary' uniform `withBaseCase` (return Eps)

instance Arbitrary ProcedureBody where
  arbitrary = genericArbitrary uniform

instance Arbitrary Arguments where
  arbitrary = genericArbitrary' uniform `withBaseCase` (Arg <$> arbitrary)

instance Arbitrary Return where
  arbitrary = Return <$> arbitrary

instance Arbitrary Main where
  arbitrary = genericArbitrary uniform

instance Arbitrary Procedure where
  arbitrary = genericArbitrary uniform

instance Arbitrary Procedures where
  arbitrary = genericArbitrary' uniform `withBaseCase` (return Nil)

instance Arbitrary ArgList where
  arbitrary = genericArbitrary' uniform `withBaseCase` (ArgI <$> arbitrary)

instance Arbitrary Program where
  arbitrary = genericArbitrary uniform

roundTrip :: Program -> Property
roundTrip p = strip parsed  === p
        where parsed = parse programParseEOF "" (show $ pretty p)
              strip (Right x) = x

spec :: Spec
spec = do
  prop "checks if the prettyfied program is equivalent to the original program" $ roundTrip
