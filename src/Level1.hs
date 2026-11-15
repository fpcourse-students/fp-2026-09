{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Обязательный уровень: обход структуры и небольшой язык парсеров.
module Level1 where

import Control.Applicative
import Data.Char qualified as Char
import Lexer (Token)
import MetaUtils
import MyIO


-- 1.1. Обходы и учебная консоль.
-- Реализуйте Traversable Point3 через Applicative: обход слева направо,
-- форма точки сохраняется. Не используйте deriving Traversable.

data Point3 a = Point3 a a a
  deriving (Show, Eq)

instance Functor Point3 where
  fmap f (Point3 x y z) = Point3 (f x) (f y) (f z)

instance Foldable Point3 where
  foldMap f (Point3 x y z) = f x <> f y <> f z

instance Applicative Point3 where
  pure x = Point3 x x x

  Point3 f g h <*> Point3 x y z = Point3 (f x) (g y) (h z)

todoImpl ''Traversable ''Point3

-- Через свой traverse и writeInt напишите printSum:
-- печать компонент первой точки, второй и их суммы, слева направо.
-- Функции *UnsafeMyIO используйте только для проверки.

printSum :: Point3 Int -> Point3 Int -> MyIO ()
printSum = todo "printSum"

-- Лексер удаляет пробелы и выделяет токены: tokenize "name = 42" == ["name", "=", "42"].
-- Парсер возвращает Just (остаток, результат) при успехе и Nothing при неудаче.

newtype MyParser a = MyParser
  { runMyParser :: [Token] -> Maybe ([Token], a) }

-- Прочитать первый токен, если он удовлетворяет предикату.
satisfy :: (Token -> Bool) -> MyParser Token
satisfy p = MyParser \case
  t:ts' | p t -> Just (ts', t)
  _ -> Nothing

token :: Token -> MyParser Token
token t = satisfy (== t)

anyToken :: MyParser Token
anyToken = satisfy (const True)

-- Имя -- непустая последовательность латинских букв (без цифр и подчёркиваний).
identifier :: MyParser String
identifier = satisfy \s -> not (null s) && all isLetter s
  where
    isLetter c = Char.isAsciiLower c || Char.isAsciiUpper c

-- Успех только на пустом вводе. Ничего не читает.
eof :: MyParser ()
eof = MyParser \case
  [] -> Just ([], ())
  _ -> Nothing


-- Числовой токен: непустая последовательность цифр 0..9.
-- Во входных данных числа помещаются в Int.
decimal :: Token -> Bool
decimal s = not (null s) && all Char.isDigit s

data Assignment = Assignment String Int
  deriving (Show, Eq)

-- Готовый разбор name = 42: проверяем успех каждого шага и передаём остаток.
-- Работает до заполнения инстансов MyParser.
parseAssignmentManual :: MyParser Assignment
parseAssignmentManual = MyParser \ts -> case runMyParser identifier ts of
  Nothing -> Nothing
  Just (rest1, name) -> case runMyParser (token "=") rest1 of
    Nothing -> Nothing
    Just (rest2, _) -> case runMyParser (satisfy decimal) rest2 of
      Nothing -> Nothing
      Just (rest3, digits) -> Just (rest3, Assignment name (read digits))

-- Повторяющиеся case обобщим в комбинаторы и перепишем этот разбор в 1.4.


-- 1.2. Прочитайте, прежде чем писать.
-- Запишите результаты значениями, без вызовов парсеров и других функций.
-- a) runMyParser parseAssignmentManual ["count", "=", "42", ";"]
-- b) runMyParser parseAssignmentManual ["count", "=", "oops", ";"]
-- c) Для стандартного Functor MyParser:
--    runMyParser (length <$> anyToken) ["hello", "!"]

answerAssignment :: Maybe ([Token], Assignment)
answerAssignment = todo "answerAssignment"

answerInvalid :: Maybe ([Token], Assignment)
answerInvalid = todo "answerInvalid"

answerMapped :: Maybe ([Token], Int)
answerMapped = todo "answerMapped"


-- 1.3. Три операции над парсерами.
-- Заполните по одному пропуску в каждом инстансе:
-- fmap преобразует результат, сохраняя остаток;
-- <*> запускает второй парсер на остатке после первого;
-- <|> при неудаче левой ветки запускает правую на исходном вводе.

instance Functor MyParser where
  fmap f (MyParser p) = MyParser \ts -> case p ts of
    Nothing -> Nothing
    Just (rest, x) -> todo "fmap success"

instance Applicative MyParser where
  pure x = MyParser \ts -> Just (ts, x)

  MyParser pf <*> MyParser px = MyParser \ts -> case pf ts of
    Nothing -> Nothing
    Just (rest, f) -> case todo "applicative next" of
      Nothing -> Nothing
      Just (rest', x) -> Just (rest', f x)

instance Alternative MyParser where
  empty = MyParser $ const Nothing

  MyParser p <|> MyParser q = MyParser \ts -> case p ts of
    Nothing -> todo "alternative fallback"
    success -> success

-- Через fmap преобразуем числовой токен в натуральное число (включая 0).
natural :: MyParser Int
natural = read <$> satisfy decimal


-- 1.4. Файл настроек.
-- Через комбинаторы напишите parseAssignment для записи name = 42
-- и parseAssignments для списка таких записей, каждая завершается ';'.
-- Список может быть пустым; сохраните порядок и повторы, прочитайте весь ввод.
-- Сравните запись parseAssignment с данным parseAssignmentManual.
-- runMyParser parseAssignments (tokenize "count = 42; limit = 10;")
--   == Just ([], [Assignment "count" 42, Assignment "limit" 10])

parseAssignment :: MyParser Assignment
parseAssignment = todo "parseAssignment"

parseAssignments :: MyParser [Assignment]
parseAssignments = todo "parseAssignments"
