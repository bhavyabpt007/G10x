module Test where

import Test.HUnit
import Main 

testParseBool :: Test
testParseBool =
    TestList [
        (TestCase (assertEqual "Should return False for n"
                            False (parseBool "n"))),
        (TestCase (assertEqual "Should return False for N"
                            False (parseBool "N"))),
        (TestCase (assertEqual "Should return False for f"
                            False (parseBool "f"))),
        (TestCase (assertEqual "Should return False for F"
                            False (parseBool "F"))),
        (TestCase (assertEqual "Should return True for y"
                            True (parseBool "y"))),
        (TestCase (assertEqual "Should return True for Y"
                            True (parseBool "Y"))),
        (TestCase (assertEqual "Should return True for t"
                            True (parseBool "t"))),
        (TestCase (assertEqual "Should return True for T"
                            True (parseBool "T")))
    ]

testArrayParseHelperBool  :: Test
testArrayParseHelperBool = 
    TestList [ 
                (TestCase (assertEqual "Should return [False] for empty string"
                            [False] (arrayParseHelperBool ""))),
                (TestCase (assertEqual "Should return [False] for empty string"
                            [False] (arrayParseHelperBool "n"))),
                (TestCase (assertEqual "Should return [True] for empty string"
                            [True] (arrayParseHelperBool "y"))),
                (TestCase (assertEqual "Should return [False,True] for empty string"
                            [False,True] (arrayParseHelperBool "F,T"))),
                (TestCase (assertEqual "Should return [True,False] for empty string"
                            [True,False] (arrayParseHelperBool "T,F")))
             ]

testArrayParseHelperNumber  :: Test
testArrayParseHelperNumber = 
    TestList [ 
                (TestCase (assertEqual "Should return [] for empty string"
                            [] (arrayParseHelperNumber ""))),
                (TestCase (assertEqual "Should return [20.0] for empty string"
                            [20.0] (arrayParseHelperNumber "20"))),
                (TestCase (assertEqual "Should return [-20.0] for empty string"
                            [-20.0] (arrayParseHelperNumber "-20"))),
                (TestCase (assertEqual "Should return [20.0e29] for empty string"
                            [2.0e29] (arrayParseHelperNumber "200000000000000000000000000000"))),
                (TestCase (assertEqual "Should return [20.0,20.0] for empty string"
                            [20.0,20.0] (arrayParseHelperNumber "20,20")))
             ]

testArrayParseHelper :: Test
testArrayParseHelper =
    TestList [
                (TestCase (assertEqual "Should return [\"bob\"] for empty string"
                            ["bob"] (arrayParseHelper "bob"))),
                (TestCase (assertEqual "Should return [\"bob\",\"alex\"] for empty string"
                            ["bob","alex"] (arrayParseHelper "bob,alex")))
             ]


main :: IO Counts
main = runTestTT $ TestList [testParseBool,testArrayParseHelperBool,testArrayParseHelperNumber,testArrayParseHelper]