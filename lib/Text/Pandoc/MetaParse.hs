{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-|
Module      : Text.Pandoc.MetaParse
Copyright   : (C) 2019 Christian Despres
License     : MIT
Stability   : experimental

Simple parsing of Pandoc `Meta` metadata inspired by the Aeson library.
-}

module Text.Pandoc.MetaParse
  (
  -- * Using this library
  -- $use

  -- ** @MetaObject@ and @MetaValue@ parsers
  -- $metaparse

  -- ** Further examples
  -- $complicate

  -- * Parse and result types
    Parse
  , Result(..)
  , MetaObject(..)
  , ParseValue
  , ParseObject

  -- ** Running parsers
  , runParse
  , runParseValue
  , runParseObject
  , parseMeta
  , parseMetaWith

  -- * Parsing @MetaObject@ and @MetaValue@ values
  -- ** @MetaObject@ parsers

  , FromObject(..)

  -- *** Parsing fields
  -- $field

  , (.!)
  , (.?)
  , field
  , maybeField
  , (.?!)
  , maybeF

  -- ** Guarding fields
  , onlyFields
  , guardNoField

  -- ** @MetaValue@ parsers

  , FromValue(..)
  , object
  , fromObject
  , symbol
  , symbolFrom
  , charLike
  , symbolLike
  , stringified
  , metaMap
  , weakString
  , weakInlines
  , weakBlocks
  -- ** Lower-level inspection, embedding a @Result@ in a parser
  , inspect
  , embedResult
  , liftResult
  -- * Errors
  , MetaError(..)
  , Expectation(..)
  , expectationFromList
  , expectationToList
  -- ** Modifying thrown errors
  , (<?>)
  , expect
  -- ** Throwing errors
  , throwExpectGot
  , throwTypeError
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List            (intercalate)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Text.Pandoc
import           Text.Pandoc.Builder  (Blocks, Inlines, fromList)
import           Text.Pandoc.Shared   (stringify)

-- $use
-- Suppose you expect the @authors@ field of your document's `Meta` to consist of a list of @{ name: [Inline]; location: [Inline]}@ entries.
-- You can create the type
--
-- > data Author = Author
-- >   { name     :: [Inline]
-- >   , location :: [Inline]
-- >   }
-- >
--
-- and write the instance
--
-- > instance FromValue Author where
-- >   parseValue = object $ Author <$> field "name" <*> field "location"
--
-- Then @`parseMetaWith` (`field` "authors")@ will give a function @`Meta` -> `Result` [Author]@.
--
-- There are other possibilities: you can work solely with `FromObject` and write
--
-- > instance FromObject Author where
-- >   parseObject = Author <$> field "name" <*> field "location"
-- >
-- > instance FromObject [Author] where
-- >   parseObject = field "authors"
--
-- and have `parseMeta` give you the same function @`Meta` -> `Result` [Author]@.
-- Or you can mix instances of both `FromValue` and `FromObject`, or use
-- neither and simply write the parser you need directly.

-- $metaparse
-- In this library we think of the @Map String MetaValue@ in a @MetaMap@ as
-- representing something like a JSON @Object@, where the @String@ keys
-- represent named fields. We wrap this @Map@ in a `MetaObject` newtype wrapper
-- to reflect this, and write @MetaValue@ and @MetaObject@ parsers in different
-- ways.
--
-- The `ParseObject` and `ParseValue` monads are linked by two families of functions.
--
--   * In one direction, we have functions like `.!` and `field` that parse the fields of the input `MetaObject` using `ParseValue` parsers.
--
--   * In the other direction, we have the functions `object` and `fromObject` that parse the input `MetaValue` using `ParseObject` parsers.

-- $complicate
-- If we have a field @prefix@ that can be @no-prefix@, @{ with-prefix: string
-- }@, or be absent (with defailt @no-prefix@) then we can parse this directly
-- (and somewhat confusingly) with
--
-- > data Prefix = WithPrefix String | NoPrefix
-- >
-- > p :: ParseObject Prefix
-- > p = maybeF NoPrefix $ "prefix" .? symbol "no-prefix" NoPrefix <|> object (WithPrefix <$> field "with-prefix") <?> "prefix: no-prefix or prefix: { with-prefix: string }"
--
-- or create instances
--
-- > instance FromValue Prefix where
-- >   parseValue = pNo <|> pWith <?> "no-prefix or { with-prefix: string }"
-- >     where
-- >       pNo   = symbol "no-prefix" NoPrefix
-- >       pWith = object $ WithPrefix <$> field "with-prefix"
-- >
-- > instance FromObject Prefix where
-- >  parseObject = "prefix" .?! NoPrefix
--
-- and have @parseMeta :: Meta -> Result Prefix@.

-- | Expected values when parsing.

newtype Expectation = Expectation
  { unExpectation :: Set.Set String
  } deriving (Eq, Ord, Show, Semigroup, Monoid)

expectationToList :: Expectation -> [String]
expectationToList = Set.toList . unExpectation

expectationFromList :: [String] -> Expectation
expectationFromList = Expectation . Set.fromList

-- | Possible errors during parsing. The `Semigroup` instance determines how the
-- errors are combined with `\<|\>`. Briefly, we prefer to keep constructors
-- that appear earlier in the declaration and are leftmost if the constructors
-- are the same. The exception is that two `MetaExpectGot` values will have
-- their `Expectation` fields merged together, keeping the leftmost @String@.
data MetaError
  = MetaExpectGotError Expectation (Maybe String) -- ^ Expected @x@; got @Just s@ if something was present, otherwise @Nothing@.
  | MetaFieldUnknown String   -- ^ Field should not be present
  | MetaSomeError String             -- ^ Some error
  | MetaFieldError String MetaError  -- ^ In field @k@, had error @e@
  | MetaNullError                    -- ^ Thrown by `empty`
  deriving (Eq, Ord, Show)

instance Semigroup MetaError where
  (<>) = go
    where
      mmerge (Just x) _ = Just x
      mmerge Nothing  x = x

      go (MetaExpectGotError e ms) (MetaExpectGotError e' ms') = MetaExpectGotError (e <> e') (mmerge ms ms')
      go (MetaExpectGotError e s) _ = MetaExpectGotError e s
      go _ (MetaExpectGotError e s) = MetaExpectGotError e s
      go (MetaFieldUnknown s) _ = MetaFieldUnknown s
      go _ (MetaFieldUnknown s) = MetaFieldUnknown s
      go (MetaSomeError s) _ = MetaSomeError s
      go _ (MetaSomeError s) = MetaSomeError s
      go (MetaFieldError k e) _ = MetaFieldError k e
      go _ (MetaFieldError k e) = MetaFieldError k e
      go MetaNullError MetaNullError = MetaNullError

instance Monoid MetaError where
  mempty = MetaNullError

-- | Show the branch of the argument. Used for type errors.
showMetaValueType :: MetaValue -> String
showMetaValueType x = case x of
  MetaString _  -> "String"
  MetaBool _    -> "Bool"
  MetaMap _     -> "Map"
  MetaList _    -> "List"
  MetaInlines _ -> "Inlines"
  MetaBlocks _  -> "Blocks"

-- | Throw a simple type error, printing the branch of the @MetaValue@ as what was received.
--
-- > throwTypeError s (MetaString x) = throwExpectGot s "String"
throwTypeError :: MonadError MetaError m
               => String -- ^ What was expected.
               -> MetaValue -- ^ What was received
               -> m a
throwTypeError s = throwExpectGot s . Just . showMetaValueType

-- | Throw an expectation error.
throwExpectGot :: MonadError MetaError m
               => String -- ^ What was expected.
               -> Maybe String -- ^ What was received.
               -> m a
throwExpectGot e v = throwError $ MetaExpectGotError (expectationFromList [e]) v

-- | The result of a parse.
data Result a
  = Error MetaError
  | Success a
  deriving Show

-- | Combines two @Success@ values with @<>@
instance Semigroup a => Semigroup (Result a) where
  (<>) = liftA2 (<>)

-- | Lift the unit of @a@ to a @Result a@
instance Monoid a => Monoid (Result a) where
  mempty = pure mempty

instance Functor Result where
  fmap _ (Error e)   = Error e
  fmap f (Success x) = Success (f x)

instance Applicative Result where
  pure = Success
  Error e <*> _   = Error e
  Success f <*> g = fmap f g

instance Monad Result where
  return = pure
  Error e >>= _   = Error e
  Success f >>= k = k f

instance MonadError MetaError Result where
  throwError = Error
  catchError z@(Success _) _ = z
  catchError (Error e) f     = f e

instance Alternative Result where
  empty = throwError MetaNullError
  Success z <|> _ = Success z
  _ <|> Success z = Success z
  Error x <|> Error y = Error (x <> y)

instance MonadPlus Result where

-- | A parser for an input @i@.
newtype Parse i a = Parse
  { unParse :: ReaderT i Result a
  } deriving ( Functor, Applicative, Monad, Alternative, MonadPlus, MonadError MetaError)

-- | Inspect the input under consideration (usually a `MetaValue` or a `MetaObject`)
inspect :: Parse i i
inspect = Parse ask

-- | Combine two successful parse results with the @<>@ of @a@.
instance Semigroup a => Semigroup (Parse i a) where
  (<>) = liftA2 (<>)

-- | Lift the unit of @a@ to a @Parse i a@
instance Monoid a => Monoid (Parse i a) where
  mempty = pure mempty

-- | A parser for a @MetaValue@.
type ParseValue  = Parse MetaValue

-- | A parser for a @MetaObject@.
type ParseObject = Parse MetaObject

-- | A wrapper for the @MetaMap@ branch of @MetaValue@, treated as an @Object@ with the keys being fields.
newtype MetaObject = MetaObject
  { unObject :: Map String MetaValue
  } deriving (Eq, Ord, Show, Semigroup, Monoid)

-- Should export things like this if I ever write writers for @MetaValue@ and @MetaObject@.
lookupMetaObject :: String -> MetaObject -> Maybe MetaValue
lookupMetaObject k = Map.lookup k . unObject

-- | Run an explicit parser.
runParse :: Parse i a -> i -> Result a
runParse = runReaderT . unParse

-- | Convert a `MetaValue`, returning a `Result`.
runParseValue :: FromValue a => MetaValue -> Result a
runParseValue = runParse parseValue

-- | Convert a `MetaObject`, returning a `Result`.
runParseObject :: FromObject a => MetaObject -> Result a
runParseObject = runParse parseObject

-- | Read a value from `Meta` using its `FromObject` instance.
parseMeta :: FromObject a => Meta -> Result a
parseMeta = runParseObject . rewrap
  where
    rewrap (Meta m) = MetaObject m

-- | Use a `ParseObject` parser as a `Meta` parser.
parseMetaWith :: ParseObject a -> Meta -> Result a
parseMetaWith act = runParse act . rewrap
  where
    rewrap (Meta m) = MetaObject m

-- | Embed a `Result` in a `Parse` action.
embedResult :: Result a -> Parse i a
embedResult = Parse . ReaderT . const

-- | Use a function as a `Parse` action.
liftResult :: (i -> Result a) -> Parse i a
liftResult = Parse . ReaderT

infixl 2 <?>

-- | In @act \<?\> s@, modify errors thrown from @act@ by replacing the
-- expectation string in an error with @e@, keeping the received string. In detail,
-- we have
--
-- > throwError (MetaExpectGotError _ y) <?> x = throwError (MetaExpectGotError x y)
-- > throwError e <?> x = throwError e
--
-- This should work out how you would expect when used with @\<|\>@ and functions like `.?` and `.!`, since we have
--
-- > "key" .! p <?> "expect" = "key" .! (p <?> "expect")
--
-- and
--
-- > p <|> q <?> "expect" = (p <|> q) <?> "expect"
--
-- so key errors will be preserved and all errors from chained `MetaValue` parsers will be overwritten.
(<?>) :: ParseValue a -> String -> ParseValue a
(<?>) = flip expect

-- | Flipped `<?>`.
expect :: String -> ParseValue a -> ParseValue a
expect e = flip catchError go
  where
    go (MetaExpectGotError _ g) = throwError $ MetaExpectGotError (expectationFromList [e]) g
    go x                        = throwError x

-- | Succeeds on @MetaString [x]@ and @MetaInlines [Str x]@.
charLike :: ParseValue Char
charLike = (symbolLike >>= go) <?> "Char"
  where
    go [x] = pure x
    go _   = empty

-- | Succeeds on @MetaString s@ and @MetaInlines [Str s]@. Used in `symbol` and `symbolFrom`.
symbolLike :: ParseValue String
symbolLike = liftResult go
  where
    go x = case x of
      MetaString s        -> pure s
      MetaInlines [Str s] -> pure s
      z                   -> throwTypeError "symbol" z

-- | Parse a "symbol", a string representing an element of a Haskell type @a@.
-- The parser @symbol sym x@ succeeds and returns @x@ if `symbolLike` returns @sym@.
--
-- We use `symbolLike` and not the @String@ instance of @parseValue@ because
-- Pandoc will parse bare text as @[Inline]@ and we would like to allow users to
-- write symbols without enclosing them in quotation marks if possible. Notably,
-- symbols containing whitespace or markdown characters will still need to be
-- written between quotation marks, otherwise Pandoc would return @MetaInlines@
-- containing more than just a @Str@.
symbol :: String -> a -> ParseValue a
symbol sym a = symbolFrom [(sym, a)]

-- | Parse a symbol from a given table. The parser @symbolFrom tbl@ succeeds if `symbolLike` returns @sym@ and @sym@ is found in the table.
symbolFrom :: [(String, a)] -> ParseValue a
symbolFrom tbl = symbolLike >>= lookStr
  where
    err = intercalate ", " . fmap fst $ tbl
    lookStr x = case lookup x tbl of
      Just a  -> pure a
      Nothing -> throwExpectGot err (Just x)

-- | Succeeds on `MetaMap`. You most likely want to use functions like `object`, `fromObject`, and `.!` instead of this function.
metaMap :: ParseValue (Map String MetaValue)
metaMap = liftResult go
  where
    go x = case x of
      MetaMap m -> pure m
      z         -> throwTypeError "Map" z

-- | Parse something other than a list or map and `stringify` it.
stringified :: ParseValue String
stringified = liftResult go
  where
    go x = case x of
      MetaString s  -> pure s
      MetaInlines s -> pure $ stringify s
      MetaBlocks s  -> pure $ stringify s
      MetaBool b    -> pure $ if b then "true" else "false"
      z             -> throwTypeError "String-like " z

-- | A @String@ parser that succeeds on @MetaString@ and on a @MetaList@ whose
-- entries are of the form @MetaString [x]@.
weakString :: ParseValue String
weakString = liftResult go
  where
    go x = case x of
      MetaString s -> pure s
      MetaList l   -> traverse runParseValue l
      z            -> throwTypeError "String" z

-- | An @[Inline]@ parser that succeeds on @MetaInlines@ and on a @MetaList@
-- whose entries are of the form @MetaInlines [x]@.
weakInlines :: ParseValue [Inline]
weakInlines = liftResult go
  where
    go x = case x of
      MetaInlines s -> pure s
      MetaList l    -> traverse runParseValue l
      z             -> throwTypeError "Inlines" z


-- | A @[Block]@ parser that succeeds on @MetaBlocks@ and on a @MetaList@ whose
-- entries are of the form @MetaBlocks [x]@.
weakBlocks :: ParseValue [Block]
weakBlocks = liftResult go
  where
    go x = case x of
      MetaBlocks s -> pure s
      MetaList l   -> traverse runParseValue l
      z            -> throwTypeError "Blocks" z

-- | Run a @MetaObject@ parser as a @MetaValue@ parser, throwing an error if the
-- @MetaValue@ is not a @MetaMap@.
object :: ParseObject a -> ParseValue a
object act = metaMap >>= embedResult . runParse act . MetaObject

-- | Expect a @MetaMap@, parsing it as @MetaObject@.
fromObject :: FromObject a => ParseValue a
fromObject = object parseObject

-- | Check that an object has only (but not necessarily all of) the listed
-- fields, proceeding with parsing if so, throwing a @MetaFieldUnknown@ error
-- with one of the unknown fields otherwise.
onlyFields :: [String] -> ParseObject a -> ParseObject a
onlyFields = (>>) . go . Set.fromList
  where
    go ks = do
      fs <- Map.keysSet . unObject <$> inspect
      case Set.toList (fs `Set.difference` ks) of
        []    -> pure ()
        (x:_) -> throwError (MetaFieldUnknown x)

-- | Throw a @MetaFieldUnknown@ error if the given field is present.
guardNoField :: String -> ParseObject ()
guardNoField k = inspect >>= go . lookupMetaObject k
  where
    go Nothing  = pure ()
    go (Just _) = throwError (MetaFieldUnknown k)

-- $field
-- Note that all of these functions wrap thrown errors during the parsing of a
-- field @k@ in @MetaErrorField k@. The ones that expect a field to be present
-- throw a @MetaErrorField k MetaFieldNotPresent@ error if it is not.

infixr 1 .?

-- | Run a @MetaValue@ parser on a field if it is present, returning @Just@ the
-- result, and returning @Nothing@ if it is not present.
(.?) :: String -> ParseValue a -> ParseObject (Maybe a)
k .? act = (inspect >>= go . lookupMetaObject k) `catchError` wraperr
  where
    go Nothing  = pure Nothing
    go (Just x) = fmap Just . embedResult $ runParse act x
    wraperr e = throwError $ MetaFieldError k e

infixr 1 .!

-- | Run a @MetaValue@ parser on a field, throwing an error if it is not
-- present.
(.!) :: String -> ParseValue a -> ParseObject a
k .! act = (k .? act) >>= go
  where
    go Nothing  = throwError $ MetaExpectGotError (expectationFromList ["present field " <> k]) Nothing
    go (Just a) = pure a

-- | Parse the value of a field, throwing an error if it is not present.
--
-- > field k = k .! parseValue
field :: FromValue a => String -> ParseObject a
field = flip (.!) parseValue

-- | Parse the value of a field if it is present, returning @Just@ the result,
-- and returning @Nothing@ if it is not present.
--
-- > maybeField k = k .? parseValue
maybeField :: FromValue a => String -> ParseObject (Maybe a)
maybeField = flip (.?) parseValue

-- | Parse the value of a field if it is present, returning @Just@ the result,
-- and returning the supplied default if it is not present.
(.?!) :: FromValue a => String -> a -> ParseObject a
k .?! x = maybeF x $ maybeField k

-- | Give a default value to a parser returning @Maybe a@.
maybeF :: Functor f => a -> f (Maybe a) -> f a
maybeF = fmap . fromMaybe

-- | Things that can be read from a `MetaObject`. Given for writing your own
-- instances. You can `inspect` the underlying `MetaObject`, but
-- the provided combinators like `.!` and `field` should be more convenient.
class FromObject a where
  parseObject :: ParseObject a

-- | Things that can be converted from a @MetaValue@.
--
-- We include @parseListValue@ so we can have sensible results when parsing
-- @String@, @MetaInlines@, and @MetaBlocks@.
--
-- Note that we parse a @MetaValue@ as a @String@, @[Inline]@, or @[Block]@
-- /only when/ it is a @MetaString@, @MetaInlines@, or @MetaBlocks@,
-- respectively. This remark applies to the parsing of @Inlines@, @Blocks@, and
-- @Text@.If you also want a @MetaList@ to parse as one of these if all of its
-- entries parse as either @Char@, @Inline@, or @Block@, then use `weakString`,
-- `weakInlines`, or `weakBlocks`.
--
-- No @Map String MetaValue@ or @MetaObject@ instance is given. Use functions
-- like `object`, `fromObject`, and `field` instead. Use `metaMap` if you are
-- sure you need to parse a @Map String MetaValue@.
class FromValue a where
  parseValue :: ParseValue a
  parseListValue :: ParseValue [a]

  parseListValue = liftResult go >>= traverse fromValue
    where
      go x = case x of
        MetaList s -> pure s
        z          -> throwTypeError "List" z

-- | Parse a @MetaValue@ in the @Parse i@ monad.
fromValue :: FromValue a => MetaValue -> Parse i a
fromValue = embedResult . runParseValue

instance FromValue a => FromValue [a] where
  parseValue = parseListValue

instance FromValue MetaValue where
  parseValue = inspect

instance FromValue Bool where
  parseValue = liftResult go
    where
      go x = case x of
        MetaBool b -> pure b
        z          -> throwTypeError "Bool" z

-- | Parse @MetaString@ as @Text@.
instance FromValue Text where
  parseValue = Text.pack <$> parseValue <?> "text"

-- | Parse @MetaString [x]@ as @Char@ and @MetaString s@ as @String@
instance FromValue Char where
  parseValue = liftResult go
    where
      go (MetaString [x]) = pure x
      go (MetaString s)   = throwExpectGot "Char" (Just s)
      go z                = throwTypeError "Char" z
  parseListValue = liftResult go
    where
      go x = case x of
        MetaString s -> pure s
        z            -> throwTypeError "String" z

-- | Parse @MetaInlines [x]@ as @Inline@ and @MetaInlines s@ as @[Inline]@
instance FromValue Inline where
  parseValue = liftResult go
    where
      go x = case x of
        MetaInlines [y] -> pure y
        MetaInlines _   -> throwExpectGot "Inlines of length 1" Nothing
        z               -> throwTypeError "Inline" z
  parseListValue = liftResult go
    where
      go x = case x of
        MetaInlines y -> pure y
        z             -> throwTypeError "Inlines" z

-- | Parse @MetaBlocks [x]@ as @Block@ and @MetaBlocks s@ as @[Block]@
instance FromValue Block where
  parseValue = liftResult go
   where
     go x = case x of
       MetaBlocks [y] -> pure y
       MetaBlocks _   -> throwExpectGot "Blocks of length 1" Nothing
       z              -> throwTypeError "Block" z
  parseListValue = liftResult go
    where
      go x = case x of
        MetaBlocks y -> pure y
        z            -> throwTypeError "Blocks" z

-- | Parse @MetaInlines@ as @Inlines@
instance FromValue Inlines where
  parseValue = fromList <$> parseValue

-- | Parse @MetaBlocks@ as @Blocks@
instance FromValue Blocks where
  parseValue = fromList <$> parseValue
