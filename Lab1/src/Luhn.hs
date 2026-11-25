module Luhn where

-- Проверка корректности номера банковской карты алгоритмом Луна https://ru.wikipedia.org/wiki/Алгоритм_Луна.
-- Алгоритм:
-- 1. Все цифры, стоящие на чётных местах (считая с конца), удваиваются. Если при этом получается число, большее 9, то из него вычитается 9. Цифры, стояшие на нечётных местах, не изменяются.
-- То есть: последняя цифра не меняется; предпоследнее удваивается; 3-е с конца (предпредпоследнее) не меняется; 4-е с конца удваивается и т.д.
-- 2. Все полученные числа складываются.
-- 3. Если полученная сумма кратна 10, то исходный список корректен.

-- Не пытайтесь собрать всё в одну функцию, используйте вспомогательные.
-- Например: разбить число на цифры (возможно, сразу в обратном порядке).
-- Не забудьте добавить тесты, в том числе для вспомогательных функций!

getDigitsReversed :: Integer -> [Integer]
getDigitsReversed 0 = []
getDigitsReversed n =
  let (rest, digit) = n `divMod` 10
  in digit : getDigitsReversed rest

doubleDigit :: Integer -> Integer
doubleDigit d
  | doubled > 9 = doubled - 9
  | otherwise = doubled
  where
    doubled = 2 * d

sumForLuhn :: [Integer] -> Integer
sumForLuhn [] = 0
sumForLuhn [x] = x
sumForLuhn (x:y:digits) = x + doubleDigit y + sumForLuhn digits

isLuhnValid :: Integer -> Bool
isLuhnValid n
  | n <= 0 = False
  | otherwise = sumForLuhn (getDigitsReversed n) `mod` 10 == 0
