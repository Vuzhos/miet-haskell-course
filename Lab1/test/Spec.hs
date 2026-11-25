import FirstSteps
import Lists
import Luhn
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
        -- Можно вложить глубже: describe "xor" $ do ... чтобы дать названия отдельным тестам
        it "xor" $ do
            xor True True `shouldBe` False
            xor True False `shouldBe` True
            xor False True `shouldBe` True
            xor False False `shouldBe` False

        it "max3" $ do
            max3 1 3 2 `shouldBe` 3
            max3 5 2 5 `shouldBe` 5
            max3 7 7 7 `shouldBe` 7
            max3 (-5) (-2) (-8) `shouldBe` (-2)

        it "median3" $ do
            median3 1 3 2 `shouldBe` 2
            median3 3 2 1 `shouldBe` 2
            median3 5 2 5 `shouldBe` 5
            median3 4 4 4 `shouldBe` 4
            median3 (-5) (-2) (-8) `shouldBe` (-5)

        it "rgbToCmyk" $ do
            rgbToCmyk (RGB 0 0 0) `shouldBe` CMYK 0 0 0 1
            rgbToCmyk (RGB 255 255 255) `shouldBe` CMYK 0 0 0 0
            rgbToCmyk (RGB 255 0 0) `shouldBe` CMYK 0 1 1 0
            rgbToCmyk (RGB 0 255 0) `shouldBe` CMYK 1 0 1 0
            rgbToCmyk (RGB 0 0 255) `shouldBe` CMYK 1 1 0 0

        it "geomProgression" $ do
            geomProgression 3 2 0 `shouldBe` 3
            geomProgression 3 2 2 `shouldBe` 12
            geomProgression (-2) (-3) 3 `shouldBe` 54
            geomProgression 7 0 0 `shouldBe` 7
            geomProgression 7 0 3 `shouldBe` 0
            evaluate (geomProgression 3 2 (-1)) `shouldThrow` anyException

        it "coprime" $ do
            coprime 10 15 `shouldBe` False
            coprime 12 35 `shouldBe` True
            coprime (-12) 35 `shouldBe` True
            coprime 12 (-18) `shouldBe` False
            coprime 0 1 `shouldBe` True
            coprime 0 (-1) `shouldBe` True
            coprime 0 2 `shouldBe` False
            coprime 0 0 `shouldBe` False
            coprime 1 1 `shouldBe` True

    describe "lists" $ do
        it "removeFirst" $ do
            removeFirst 2 [1, 2, 3, 2] `shouldBe` [1, 3, 2]
            removeFirst 4 [1, 2, 3] `shouldBe` [1, 2, 3]
            removeFirst 2 [2, 2] `shouldBe` [2]
            removeFirst 2 [2] `shouldBe` []
            removeFirst 2 [] `shouldBe` []

        it "interleave" $ do
            interleave [1, 2, 3] [10, 20, 30] `shouldBe` [1, 10, 2, 20, 3, 30]
            interleave [1, 2, 3] [10] `shouldBe` [1, 10, 2, 3]
            interleave [1] [10, 20, 30] `shouldBe` [1, 10, 20, 30]
            interleave [] [10, 20] `shouldBe` [10, 20]
            interleave [1, 2] [] `shouldBe` [1, 2]
            interleave ([] :: [Int]) ([] :: [Int]) `shouldBe` []

        it "distance" $ do
            distance (Point []) (Point []) `shouldBe` 0
            distance (Point [3]) (Point [8]) `shouldBe` 5
            distance (Point [0, 0]) (Point [3, 4]) `shouldBe` 5
            distance (Point [2, -1]) (Point [2, -1]) `shouldBe` 0
            distance (Point [1, 0]) (Point [0, 1]) `shouldSatisfy` (\d -> abs (d - sqrt 2) < 1e-10)
            evaluate (distance (Point [1]) (Point [1, 2])) `shouldThrow` anyException
            evaluate (distance (Point []) (Point [1])) `shouldThrow` anyException

        it "find" $ do
            find (> 0) [-1, 2, -3, 4] `shouldBe` Just 2
            find (even) [2, 4, 6] `shouldBe` Just 2
            find (> 0) [-1, -2, -3] `shouldBe` Nothing
            find (> 0) [] `shouldBe` Nothing

        it "findLast" $ do
            findLast (> 0) [-1, 2, -3, 4] `shouldBe` Just 4
            findLast (even) [2, 4, 6] `shouldBe` Just 6
            findLast (> 0) [-1, -2, -3] `shouldBe` Nothing
            findLast (> 0) [] `shouldBe` Nothing

        it "find2" $ do
            find2 (> 0) [-1, 2, -3, 4] `shouldBe` Just 2
            find2 (even) [2, 4, 6] `shouldBe` Just 2
            find2 (> 0) [-1, -2, -3] `shouldBe` Nothing
            find2 (> 0) [] `shouldBe` Nothing

        it "findLast2" $ do
            findLast2 (> 0) [-1, 2, -3, 4] `shouldBe` Just 4
            findLast2 (even) [2, 4, 6] `shouldBe` Just 6
            findLast2 (> 0) [-1, -2, -3] `shouldBe` Nothing
            findLast2 (> 0) [] `shouldBe` Nothing

        it "mapFuncs" $ do
            mapFuncs [(* 2), (+ 1), \n -> n * n] 3 `shouldBe` [6, 4, 9]
            mapFuncs ([] :: [Integer -> Integer]) 3 `shouldBe` []

        it "satisfiesAll" $ do
            satisfiesAll [even, (> 0)] 10 `shouldBe` True
            satisfiesAll [even, (> 0)] (-2) `shouldBe` False
            satisfiesAll [] 4 `shouldBe` True

        it "tailNel" $ do
            tailNel (NEL 1 [2, 3]) `shouldBe` [2, 3]
            tailNel (NEL 1 []) `shouldBe` []

        it "lastNel" $ do
            lastNel (NEL 1 []) `shouldBe` 1
            lastNel (NEL 1 [2, 3]) `shouldBe` 3

        it "zipNel" $ do
            zipNel (NEL 1 [2, 3]) (NEL 'a' ['b', 'c']) `shouldBe` NEL (1, 'a') [(2, 'b'), (3, 'c')]
            zipNel (NEL 1 [2, 3]) (NEL 'a' []) `shouldBe` NEL (1, 'a') []

        it "listToNel" $ do
            listToNel ([] :: [Int]) `shouldBe` Nothing
            listToNel [1] `shouldBe` Just (NEL 1 [])
            listToNel [1, 2, 3] `shouldBe` Just (NEL 1 [2, 3])

        it "nelToList" $ do
            nelToList (NEL 1 []) `shouldBe` [1]
            nelToList (NEL 1 [2, 3]) `shouldBe` [1, 2, 3]

    describe "luhn" $ do
        it "getDigitsReversed" $ do
            getDigitsReversed 0 `shouldBe` []
            getDigitsReversed 7 `shouldBe` [7]
            getDigitsReversed 12030 `shouldBe` [0, 3, 0, 2, 1]
            getDigitsReversed 48260713598 `shouldBe` [8, 9, 5, 3, 1, 7, 0, 6, 2, 8, 4]

        it "doubleDigit" $ do
            doubleDigit 0 `shouldBe` 0
            doubleDigit 4 `shouldBe` 8
            doubleDigit 5 `shouldBe` 1
            doubleDigit 9 `shouldBe` 9

        it "sumForLuhn" $ do
            sumForLuhn [] `shouldBe` 0
            sumForLuhn [7] `shouldBe` 7
            sumForLuhn [1, 8] `shouldBe` 8
            sumForLuhn [3, 1, 7] `shouldBe` 12
            sumForLuhn (getDigitsReversed 48260713598) `shouldBe` 50

        it "isLuhnValid" $ do
            isLuhnValid 18 `shouldBe` True
            isLuhnValid 48260713598 `shouldBe` True
            isLuhnValid 730914002863 `shouldBe` False
            isLuhnValid 905001247634 `shouldBe` True
            isLuhnValid (-905001247634) `shouldBe` False
            isLuhnValid 621730480153 `shouldBe` False
            isLuhnValid 0 `shouldBe` False
