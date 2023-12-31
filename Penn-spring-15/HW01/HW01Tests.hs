-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests =
  [ Test
      "lastDigit test"
      testLastDigit
      [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)],
    Test
      "dropLastDigit test"
      testDropLastDigit
      [(123, 12), (1234, 123), (5, 0), (10, 1), (0, 0)]
  ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = []

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = []

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = []

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, b) = luhn n == b

ex5Tests :: [Test]
ex5Tests =
  [ Test
      "luhn test"
      testLuhn
      [ (5594589764218858, True),
        (1234567898765432, False),
        (4419971795032571, True),
        (5579684475492832, True),
        (6011386277013436, True),
        (344374835020854, True)
      ]
  ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests =
  concat
    [ ex1Tests,
      ex2Tests,
      ex3Tests,
      ex4Tests,
      ex5Tests,
      ex6Tests
    ]
