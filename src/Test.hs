import Test.HUnit
import Math
import Syntax

expressions = [Constant 1,
               Variable "x",
              (Constant 1 |+| Variable "x"),
              (Variable "x" |+| (Variable "x" |^| Constant 2))
              ]

genTestCase e = TestCase (assertEqual "Identity incorrect" e (simplify . simplify $ (derivate (integrate e))))

cases  = map genTestCase expressions
labels = map (TestLabel "test" ) Main.cases
tests = TestList labels

main = do
  runTestTT tests
