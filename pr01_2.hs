-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------
module Pr01_2 where

myZipSave :: [a] -> [b] -> ([(a, b)], Either [a] [b])
myZipSave [] ys = ([], Right ys)
myZipSave xs [] = ([], Left xs)
myZipSave (x:xs) (y:ys) = ((x,y):pairs, remainder)
  where
    (pairs, remainder) = myZipSave xs ys

myUnzipSave :: ([(a, b)], Either [a] [b]) -> ([a], [b])
myUnzipSave ([], Left x) = (x,[]) 
myUnzipSave ([], Right x) = ([],x) 
myUnzipSave ((x, y) : xys, z) = (x : xs, y : ys)
  where (xs, ys) = myUnzipSave (xys, z)

myRevers :: [a] -> [a]
myRevers = foldl (flip (:)) []

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 _ [] = error "Пустой список"
myFoldl1 pred (x:xs) = foldl pred x xs

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [] = error "Пустой список"
myFoldr1 pred [x] = x
myFoldr1 pred (x:xs) = pred x (myFoldr1 pred xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred (x:xs) | pred x = x : myTakeWhile pred xs
                        | otherwise = []

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan pred = foldr (\x (ys, zs) -> if pred x then (x:ys, zs) else ([], x:ys)) ([], [])

data MyMaybe x = MyNothing | MyJust x deriving (Show)

data MyList a = MyEmpty | MyCons a (MyList a) deriving Show
myMap :: (a -> b) -> MyList a -> MyList b
myMap _ MyEmpty = MyEmpty
myMap f (MyCons head tail) = MyCons (f head) (myMap f tail)

myUnFoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnFoldr f x = next (f x)
  where next Nothing = []
        next (Just (x, nextX)) = x : myUnFoldr f nextX
{-

Напишите реализацию функций:
myUnzipSave ( myZipSave [...] [...]) должно работать

-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
-- myTakeWhile - реализовать с использованием сверток
-- mySpan - реализовать с использованием сверток
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
-- myUnFoldr - развертка (операция обратная к свертке)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}
