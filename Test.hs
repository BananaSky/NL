import Test.HUnit
import Math
import Syntax

genTestCase e = TestCase (assertEqual "Identity incorrect" e (simplify . simplify $ (derivate (integrate e))))

expressions = [Constant 1,
               Variable "x",
              (Constant 1 |+| Variable "x"),
              (Variable "x" |+| (Variable "x" |^| Constant 2)),
              (Variable "x" |^| (Variable "x" |+| Constant 1))
              ]

cases  = map genTestCase expressions
labels = map (TestLabel "test" ) Main.cases
tests = TestList labels

main = do
  runTestTT tests
