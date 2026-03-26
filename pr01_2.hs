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

data IngredientsName = Oil | Chocolate | Strawberry | Mango | Egg | Flour | Sugar | BakingPowder deriving Show
data Ingredients = Ingredients {oil :: Int  
                              , chocolate :: Int
                              , strawberry :: Int
                              , mango :: Int
                              , egg :: Int
                              , flour :: Int
                              , sugar :: Int
                              , bakingPowder :: Int}

data FillingMix = OilChocolateMix | OilStrawberryMix | OilMangoMix | ErrorMix String deriving Show
data Dough = CakeDough | ErrorDough String deriving Show
data CakeDough = ChocolateCakeDough | StrawberryCakeDough | MangoCakeDough | ErrorCakeDough String deriving Show
data Cake = ChocolateCake | StrawberryCake | MangoCake | ErrorCake String deriving Show
data Action = Bake deriving Show

storeIng :: Ingredients
storeIng = Ingredients {oil = 1000
                      , chocolate = 100
                      , strawberry = 80
                      , mango = 90
                      , egg = 15
                      , flour = 900
                      , sugar = 500
                      , bakingPowder = 20}

getAmount :: Ingredients -> IngredientsName -> Int
getAmount ing Oil = oil ing
getAmount ing Chocolate = chocolate ing
getAmount ing Strawberry = strawberry ing
getAmount ing Mango = mango ing
getAmount ing Egg = egg ing
getAmount ing Flour = flour ing
getAmount ing Sugar = sugar ing
getAmount ing BakingPowder = bakingPowder ing

useIngredient :: Ingredients -> IngredientsName -> Int -> Ingredients
useIngredient store Oil amount = store { oil = oil store - amount }
useIngredient store Chocolate amount = store { chocolate = chocolate store - amount }
useIngredient store Strawberry amount = store { strawberry = strawberry store - amount }
useIngredient store Mango amount = store { mango = mango store - amount }
useIngredient store Egg amount = store { egg = egg store - amount }
useIngredient store Flour amount = store { flour = flour store - amount }
useIngredient store Sugar amount = store { sugar = sugar store - amount }
useIngredient store BakingPowder amount = store { bakingPowder = bakingPowder store - amount }

useTwoIngredients :: Ingredients -> IngredientsName -> Int -> IngredientsName -> Int -> Ingredients
useTwoIngredients store ing1 amount1 = useIngredient store1
  where store1 = useIngredient store ing1 amount1

rightIngMix :: Ingredients -> IngredientsName -> Int -> Maybe String
rightIngMix i Oil x 
    | x /= 200 = Just ("Ошибка рецепта: для начинки нужно 200 масла, а вы хотите " ++ show x)
    | oil i - x < 0 = Just ("Недостаточно масла: нужно " ++ show x ++ ", есть " ++ show (oil i))
    | otherwise = Nothing
rightIngMix i Chocolate x 
    | x /= 20 = Just ("Ошибка рецепта: для начинки нужно 20 шоколада, а вы хотите " ++ show x)
    | chocolate i - x < 0 = Just ("Недостаточно шоколада: нужно " ++ show x ++ ", есть " ++ show (chocolate i))
    | otherwise = Nothing
rightIngMix i Strawberry x 
    | x /= 20 = Just ("Ошибка рецепта: для начинки нужно 20 клубники, а вы хотите " ++ show x)
    | strawberry i - x < 0 = Just ("Недостаточно клубники: нужно " ++ show x ++ ", есть " ++ show (strawberry i))
    | otherwise = Nothing
rightIngMix i Mango x 
    | x /= 20 = Just ("Ошибка рецепта: для начинки нужно 20 манго, а вы хотите " ++ show x)
    | mango i - x < 0 = Just ("Недостаточно манго: нужно " ++ show x ++ ", есть " ++ show (mango i))
    | otherwise = Nothing
rightIngMix i Egg x 
    | x /= 8 = Just ("Ошибка рецепта: для теста нужно 8 яиц, а вы хотите " ++ show x)
    | egg i - x < 0 = Just ("Недостаточно яиц: нужно " ++ show x ++ ", есть " ++ show (egg i))
    | otherwise = Nothing
rightIngMix i Flour x 
    | x /= 200 = Just ("Ошибка рецепта: для теста нужно 200 муки, а вы хотите " ++ show x)
    | flour i - x < 0 = Just ("Недостаточно муки: нужно " ++ show x ++ ", есть " ++ show (flour i))
    | otherwise = Nothing
rightIngMix i Sugar x 
    | x /= 300 = Just ("Ошибка рецепта: для теста нужно 300 сахара, а вы хотите " ++ show x)
    | sugar i - x < 0 = Just ("Недостаточно сахара: нужно " ++ show x ++ ", есть " ++ show (sugar i))
    | otherwise = Nothing
rightIngMix i BakingPowder x 
    | x /= 2 = Just ("Ошибка рецепта: для теста нужно 2 разрыхлителя, а вы хотите " ++ show x)
    | bakingPowder i - x < 0 = Just ("Недостаточно разрыхлителя: нужно " ++ show x ++ ", есть " ++ show (bakingPowder i))
    | otherwise = Nothing

getRecipeAmount :: IngredientsName -> Int
getRecipeAmount Oil = 200
getRecipeAmount Chocolate = 20
getRecipeAmount Strawberry = 20
getRecipeAmount Mango = 20
getRecipeAmount Egg = 8
getRecipeAmount Flour = 200
getRecipeAmount Sugar = 300
getRecipeAmount BakingPowder = 2

getErrorMessage :: Ingredients -> IngredientsName -> Int -> String
getErrorMessage i ing need = 
    if not (need == getRecipeAmount ing)
        then "Ошибка рецепта: для " ++ show ing ++ " нужно " ++ show (getRecipeAmount ing) ++ ", а вы хотите " ++ show need
        else "Недостаточно " ++ show ing ++ ": нужно " ++ show need ++ ", есть " ++ show (getAmount i ing)

-- Функции, которые описывают процесс приготовления частей торта
makeCakeMix :: Ingredients -> (IngredientsName, Int) -> (IngredientsName, Int) -> (FillingMix, Ingredients)

makeCakeMix store (ing1, need1) (ing2, need2) 
    | rightIngMix store ing1 need1 /= Nothing = 
        (ErrorMix (getErrorMessage store ing1 need1), store)
makeCakeMix store (ing1, need1) (ing2, need2) 
    | rightIngMix store ing2 need2 /= Nothing = 
        (ErrorMix (getErrorMessage store ing2 need2), store)
makeCakeMix store (Oil, need1) (Chocolate, need2) = 
    (OilChocolateMix, useTwoIngredients store Oil need1 Chocolate need2)
makeCakeMix store (Oil, need1) (Strawberry, need2) = 
    (OilStrawberryMix, useTwoIngredients store Oil need1 Strawberry need2)
makeCakeMix store (Oil, need1) (Mango, need2) = 
    (OilMangoMix, useTwoIngredients store Oil need1 Mango need2)
makeCakeMix store (ing1, _) (ing2, _) = 
    (ErrorMix ("Ошибка: нельзя смешать " ++ show ing1 ++ " и " ++ show ing2), store)

cakeDough :: Ingredients -> (IngredientsName, Int) -> (IngredientsName, Int) -> (IngredientsName, Int) -> (IngredientsName, Int) -> (Dough, Ingredients)
cakeDough i (Egg, x) (Flour, c) (Sugar, v) (BakingPowder, b) 
    | rightIngMix i Egg x /= Nothing = 
        (ErrorDough (getErrorMessage i Egg x), i)
cakeDough i (Egg, x) (Flour, c) (Sugar, v) (BakingPowder, b) 
    | rightIngMix i Flour c /= Nothing = 
        (ErrorDough (getErrorMessage i Flour c), i)
cakeDough i (Egg, x) (Flour, c) (Sugar, v) (BakingPowder, b) 
    | rightIngMix i Sugar v /= Nothing = 
        (ErrorDough (getErrorMessage i Sugar v), i)
cakeDough i (Egg, x) (Flour, c) (Sugar, v) (BakingPowder, b) 
    | rightIngMix i BakingPowder b /= Nothing = 
        (ErrorDough (getErrorMessage i BakingPowder b), i)
cakeDough i (Egg, x) (Flour, c) (Sugar, v) (BakingPowder, b) = 
    (CakeDough, i { egg = egg i - x, flour = flour i - c, 
                   sugar = sugar i - v, bakingPowder = bakingPowder i - b })
cakeDough i (ing1, _) (ing2, _) (ing3, _) (ing4, _) = 
    (ErrorDough ("Ошибка: неправильные ингредиенты. Ожидались: Egg, Flour, Sugar, BakingPowder. Получены: " ++ 
                 show ing1 ++ ", " ++ show ing2 ++ ", " ++ show ing3 ++ ", " ++ show ing4), i)

chocolateCakeDough :: Dough -> FillingMix -> CakeDough
chocolateCakeDough (ErrorDough msg) _ = ErrorCakeDough ("Ошибка в тесте: " ++ msg)
chocolateCakeDough _ (ErrorMix msg) = ErrorCakeDough ("Ошибка в начинке: " ++ msg)
chocolateCakeDough CakeDough OilChocolateMix = ChocolateCakeDough
chocolateCakeDough CakeDough OilStrawberryMix = ErrorCakeDough "Для шоколадного торта нужна шоколадная начинка"
chocolateCakeDough CakeDough OilMangoMix = ErrorCakeDough "Для шоколадного торта нужна шоколадная начинка"
chocolateCakeDough _ _ = ErrorCakeDough "Не те ингредиенты"

strawberryCakeDough :: Dough -> FillingMix -> CakeDough
strawberryCakeDough (ErrorDough msg) _ = ErrorCakeDough ("Ошибка теста: " ++ msg)
strawberryCakeDough _ (ErrorMix msg) = ErrorCakeDough ("Ошибка начинки: " ++ msg)
strawberryCakeDough CakeDough OilStrawberryMix = StrawberryCakeDough
strawberryCakeDough CakeDough _ = ErrorCakeDough "Для клубничного торта нужна клубничная начинка"
strawberryCakeDough _ _ = ErrorCakeDough "Не те ингредиенты"

mangoCakeDough :: Dough -> FillingMix -> CakeDough
mangoCakeDough (ErrorDough msg) _ = ErrorCakeDough ("Ошибка теста: " ++ msg)
mangoCakeDough _ (ErrorMix msg) = ErrorCakeDough ("Ошибка начинки: " ++ msg)
mangoCakeDough CakeDough OilMangoMix = MangoCakeDough
mangoCakeDough CakeDough _ = ErrorCakeDough "Для мангового торта нужна манговая начинка"
mangoCakeDough _ _ = ErrorCakeDough "Не те ингредиенты"

chocolateCake :: CakeDough -> Action -> Int -> Int -> Cake
chocolateCake (ErrorCakeDough msg) _ _ _ = ErrorCake ("Ошибка в тесте: " ++ msg)
chocolateCake ChocolateCakeDough Bake 200 25 = ChocolateCake
chocolateCake ChocolateCakeDough Bake temp _ 
    | temp /= 200 = ErrorCake ("Неправильная температура: " ++ show temp ++ "°C (нужно 200°C)")
chocolateCake ChocolateCakeDough Bake _ time 
    | time /= 25 = ErrorCake ("Неправильное время: " ++ show time ++ " мин (нужно 25 мин)")
chocolateCake ChocolateCakeDough _ _ _ = ErrorCake "Неправильное действие (нужно Bake)"
chocolateCake _ _ _ _ = ErrorCake "Для шоколадного торта нужно шоколадное тесто"

strawberryCake :: CakeDough -> Action -> Int -> Int -> Cake
strawberryCake (ErrorCakeDough msg) _ _ _ = ErrorCake ("Ошибка в тесте: " ++ msg)
strawberryCake StrawberryCakeDough Bake 200 25 = StrawberryCake
strawberryCake StrawberryCakeDough Bake temp _ 
    | temp /= 200 = ErrorCake ("Неправильная температура: " ++ show temp ++ "°C (нужно 200°C)")
strawberryCake StrawberryCakeDough Bake _ time 
    | time /= 25 = ErrorCake ("Неправильное время: " ++ show time ++ " мин (нужно 25 мин)")
strawberryCake StrawberryCakeDough _ _ _ = ErrorCake "Неправильное действие (нужно Bake)"
strawberryCake _ _ _ _ = ErrorCake "Для клубничного торта нужно клубничное тесто"

mangoCake :: CakeDough -> Action -> Int -> Int -> Cake
mangoCake (ErrorCakeDough msg) _ _ _ = ErrorCake ("Ошибка в тесте: " ++ msg)
mangoCake MangoCakeDough Bake 200 25 = MangoCake
mangoCake MangoCakeDough Bake temp _ 
    | temp /= 200 = ErrorCake ("Неправильная температура: " ++ show temp ++ "°C (нужно 200°C)")
mangoCake MangoCakeDough Bake _ time 
    | time /= 25 = ErrorCake ("Неправильное время: " ++ show time ++ " мин (нужно 25 мин)")
mangoCake MangoCakeDough _ _ _ = ErrorCake "Неправильное действие (нужно Bake)"
mangoCake _ _ _ _ = ErrorCake "Для мангового торта нужно манговое тесто"

(mixc, storeIng1) = makeCakeMix storeIng (Oil, 200) (Chocolate, 20)
(mixs, storeIng2) = makeCakeMix storeIng1 (Strawberry, 20) (Oil, 200)
(mixm, storeIng3) = makeCakeMix storeIng2 (Oil, 200) (Mango, 20)  -- вместо Cherry
(mixSpoil, storeIngs) = makeCakeMix storeIng3 (Oil, 200) (Mango, 20)  -- недостаточно ингредиентов
(mixN, storeIngn) = makeCakeMix storeIng3 (Egg, 200) (Mango, 20)