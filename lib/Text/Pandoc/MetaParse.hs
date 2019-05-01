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

  -- * Parse and result types
    Parse
  , Result(..)
  , MetaObject(..)
  , ParseValue
  , ParseObject

  -- ** Running parsers
  , runParse
  , runParseMeta
  , fromMeta
  , fromValue
  , fromObject
  , fromMetaField

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
  , withObjectName

  -- *** Guarding fields
  , onlyFields
  , guardNoField

  -- ** @MetaValue@ parsers

  , FromValue(..)
  , object
  , objectNamed
  , objectValue
  , symbol
  , symbolFrom
  , charlike
  , symbollike
  , parseString
  , parseInlines
  , parseBlocks
  , parseMetaMap
  -- ** Lower-level inspection, embedding a @Result@ in a parser
  , inspect
  , embedResult
  , liftResult
  , parseOfValue
  -- * Errors
  , MetaError(..)
  , simpleErrorShow
  , Expectation(..)
  , expectationFromList
  , expectationToList
  -- ** Modifying thrown errors
  , (<?>)
  , expect
  , expectFrom
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
import           Text.Pandoc.Definition
import           Text.Pandoc.Builder  (Blocks, Inlines, fromList)

-- TODO: Go through documentation and fix it up! Make sure the examples make sense, esp. with the new error system!
-- TODO: Fix up below with markup in the code blocks!

-- $use
-- Suppose you expect the @contributors@ field of your document's `Meta` to
-- consist of a list of @{ name: [Inline]; location: [Inline]; title: (no-title
-- | [Inline]) }@ entries, with the @title@ field having the default @no-title@
-- if it is not present. You can create the types
--
-- > data ContributorTitle = NoTitle | HasTitle Inlines
-- >
-- > data Contributor = Contributor
-- >   { name     :: Inlines
-- >   , location :: [Inline] -- Not Inlines, to show off .! and fromInlines
-- >   , title    :: ContributorTitle
-- >   }
--
-- and write the instances
--
-- > instance FromValue ContributorTitle where
-- >   parseValue = symbol "no-title" NoTitle <|>
-- >                HasTitle <$> parseValue <?> "inline title"
-- >
-- > instance FromValue Contributor where
-- >   parseValue = objectNamed "contributor" $
-- >     Contributor <$> field "name" <*> ("location" .! fromInlines) <*> "title" .?! NoTitle
--
-- Then @`runParseMeta` (`field` "contributors")@ will give a function @`Meta`
-- -> `Result` [Contributor]@. Thrown errors contain basic positional
-- information and an `Error` `MetaError` result can be printed with
-- `simpleErrorShow`.
--
-- There are other possibilities: you can write `FromObject` instances like
--
-- > instance FromObject Author where
-- >   parseObject = withObjectName "contributor" $
-- >     Author <$> field "name" <*> "location" .! fromInlines <*> "title" .?! NoTitle
-- >     -- assuming that the ContributorTitle instance is still around
-- >
-- > instance FromObject [Contributor] where
-- >   parseObject = field "contributors"
--
-- and use `fromMeta` for a @`Meta` -> `Result` [Contributor]@ function. Parsers can be
-- written with varying levels of `FromValue` and `FromObject` use.

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
--   * In the other direction, we have the functions `object` and `objectValue` that parse the input `MetaValue` using `ParseObject` parsers.

-- | Expected values when parsing.
newtype Expectation = Expectation
  { unExpectation :: Set.Set String
  } deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | Convert `Expectation` to a list of expectations.
expectationToList :: Expectation -> [String]
expectationToList = Set.toList . unExpectation

-- | Convert a list of expectations to `Expectation`.
expectationFromList :: [String] -> Expectation
expectationFromList = Expectation . Set.fromList

-- | Possible errors during parsing. The `Semigroup` instance determines how the
-- errors are combined with `<|>`. Briefly, we prefer to keep values with
-- constructors that appear earlier in the declaration, and that are leftmost if
-- the constructors are the same. The exception is that two `MetaExpectGotError`
-- values will have their `Expectation` fields merged together, keeping the
-- leftmost @Just s@.
data MetaError
  = MetaSomeError String             -- ^ Some error, printed verbatim by `simpleErrorShow`
  | MetaExpectGotError Expectation (Maybe String) -- ^ Expected: @x@, got: @Just s@ if something was present, otherwise @Nothing@.
  | MetaFieldUnknown String          -- ^ Unknown field: @k@.
  | MetaWhenParseError String MetaError  -- ^ Got error @e@ when parsing @k@
  | MetaNullError                    -- ^ Unknown error (thrown by `empty`)
  deriving (Eq, Ord, Show)

instance Semigroup MetaError where
  (<>) = go
    where
      mmerge (Just x) _ = Just x
      mmerge Nothing  x = x

      go (MetaSomeError s) _ = MetaSomeError s
      go _ (MetaSomeError s) = MetaSomeError s
      go (MetaExpectGotError e ms) (MetaExpectGotError e' ms') = MetaExpectGotError (e <> e') (mmerge ms ms')
      go (MetaExpectGotError e s) _ = MetaExpectGotError e s
      go _ (MetaExpectGotError e s) = MetaExpectGotError e s
      go (MetaFieldUnknown s) _ = MetaFieldUnknown s
      go _ (MetaFieldUnknown s) = MetaFieldUnknown s
      go (MetaWhenParseError k e) _ = MetaWhenParseError k e
      go _ (MetaWhenParseError k e) = MetaWhenParseError k e
      go MetaNullError MetaNullError = MetaNullError

instance Monoid MetaError where
  mempty = MetaNullError

-- | Show the branch of the argument, without the @Meta@ part, except that @MetaString@ is shown as @Text@. Used for simple type errors.
showMetaValueType :: MetaValue -> String
showMetaValueType x = case x of
  MetaString _  -> "String"
  MetaBool _    -> "Bool"
  MetaMap _     -> "Map"
  MetaList _    -> "List"
  MetaInlines _ -> "Inlines"
  MetaBlocks _  -> "Blocks"

-- | Display errors roughly as described in `MetaError`.
--
--   * For `MetaExpectGotError`, the `Expectation` will be a comma-separated list after a printed @Expected: @ and the @got: s@ will be printed on a new line if the result is present.
--   * For `MetaWhenParsingError e f`, we first display `simpleErrorShow e`, then print @when parsing: f@ on a new line.
simpleErrorShow :: MetaError -> String
simpleErrorShow x = case x of
  MetaExpectGotError e m -> "Expected: " <> showExp e <> "\n" <> fromM m
  MetaFieldUnknown f     -> "Unknown field: " <> f
  MetaSomeError e        -> e
  MetaWhenParseError k e     -> simpleErrorShow e <> "\nwhen parsing: " <> k
  MetaNullError          -> "Unknown error"
  where
    showExp = intercalate ", " . expectationToList
    fromM Nothing  = ""
    fromM (Just s) = "got: " <> s

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
  deriving (Eq, Ord, Show)

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

-- | Use a `ParseObject` parser as a `Meta` parser.
runParseMeta :: ParseObject a -> Meta -> Result a
runParseMeta act = runParse act . rewrap
  where
    rewrap (Meta m) = MetaObject m

-- | Convert a `MetaValue`, returning a `Result`.
fromValue :: FromValue a => MetaValue -> Result a
fromValue = runParse parseValue

-- | Convert a `MetaObject`, returning a `Result`.
fromObject :: FromObject a => MetaObject -> Result a
fromObject = runParse parseObject

-- | Read a value from `Meta` using its `FromObject` instance.
fromMeta :: FromObject a => Meta -> Result a
fromMeta = fromObject . rewrap
  where
    rewrap (Meta m) = MetaObject m

-- | Read a field of `Meta` using its `FromValue` instance.
fromMetaField :: FromValue a => String -> Meta -> Result a
fromMetaField = runParseMeta . field

-- | Embed a `Result` in a `Parse` action.
embedResult :: Result a -> Parse i a
embedResult = Parse . ReaderT . const

-- | Use a function as a `Parse` action.
liftResult :: (i -> Result a) -> Parse i a
liftResult = Parse . ReaderT

infixl 4 <?>

-- | In @act \<?\> s@, modify errors thrown from @act@ by replacing the
-- `Expectation` in an error with @e@, keeping the received string if it exists.
-- In detail, we have
--
-- > throwError (MetaExpectGotError _ y) <?> x = throwExpectGot x y
-- > throwError e <?> x = throwError e
--
-- Use `<?>` and `expect` as locally as possible in your parsers. In particular,
-- use them on the individual branches of parsers made with `<|>`, not on the
-- resulting parser, since the `Expectation` fields of errors are merged when
-- using `<|>` whenever possible.
(<?>) :: ParseValue a -> String -> ParseValue a
(<?>) = flip expect

-- | Flipped `<?>`
expect :: String -> ParseValue a -> ParseValue a
expect = expectFrom . (:[])

-- | Like `<?>`, but allowing multiple expected values.
expectFrom :: [String] -> ParseValue a -> ParseValue a
expectFrom e = flip catchError go
  where
    go (MetaExpectGotError _ g) = throwError $ MetaExpectGotError (expectationFromList e) g
    go x                        = throwError x

-- | Wraps the errors thrown by the parser in a `MetaWhenParseError` using the
-- supplied string. Used in `objectNamed` and useful when writing `FromObject`
-- instances that might benefit from an annotated error.
withObjectName :: String -> ParseObject a -> ParseObject a
withObjectName s = flip catchError go
  where
    go = throwError . MetaWhenParseError s

-- | Succeeds on @MetaString [x]@ and @MetaInlines [Str x]@. Can be used like `symbollike` is used in `symbol`.
charlike :: ParseValue Char
charlike = (symbollike >>= go) <?> "Char"
  where
    go [x] = pure x
    go _   = empty

-- | Succeeds on @MetaString s@ and @MetaInlines [Str s]@. Used in `symbol` and `symbolFrom`.
symbollike :: ParseValue String
symbollike = liftResult go
  where
    go x = case x of
      MetaString s        -> pure s
      MetaInlines [Str s] -> pure s
      z                   -> throwTypeError "symbol" z

-- | Parse a "symbol", a string representing an element of a Haskell type @a@.
-- The parser @symbol sym x@ succeeds and returns @x@ if `symbollike` returns @sym@.
--
-- We use `symbollike` and not the @String@ instance of @parseValue@ because
-- Pandoc will parse bare text as @[Inline]@ and we would like to allow users to
-- write symbols without enclosing them in quotation marks if possible. Notably,
-- symbols containing whitespace or markdown characters will still need to be
-- written between quotation marks, otherwise Pandoc would return @MetaInlines@
-- containing more than just a @Str@.
symbol :: String -> a -> ParseValue a
symbol sym a = symbolFrom [(sym, a)]

-- | Parse a symbol from a given table. The parser @symbolFrom tbl@ succeeds if `symbollike` returns @sym@ and @sym@ is found in the table.
symbolFrom :: [(String, a)] -> ParseValue a
symbolFrom tbl = symbollike >>= lookStr
  where
    err = intercalate ", " . fmap fst $ tbl
    lookStr x = case lookup x tbl of
      Just a  -> pure a
      Nothing -> throwExpectGot err (Just x)

-- | Succeeds on `MetaMap`. You most likely want to use functions like `object`, `objectValue`, and `.!` instead of this function.
parseMetaMap :: ParseValue (Map String MetaValue)
parseMetaMap = liftResult go
  where
    go x = case x of
      MetaMap m -> pure m
      z         -> throwTypeError "Map" z

-- | Succeeds on @MetaString@.
parseString :: ParseValue String
parseString = liftResult go
  where
    go x = case x of
      MetaString s -> pure s
      z            -> throwTypeError "String" z

-- | Succeeds on @MetaInlines@.
parseInlines :: ParseValue [Inline]
parseInlines = liftResult go
  where
    go x = case x of
      MetaInlines s -> pure s
      z             -> throwTypeError "Inlines" z

-- | Succeeds on @MetaBlocks@.
parseBlocks :: ParseValue [Block]
parseBlocks = liftResult go
  where
    go x = case x of
      MetaBlocks s -> pure s
      z            -> throwTypeError "Blocks" z

-- | Run a @MetaObject@ parser as a @MetaValue@ parser, throwing an error if the
-- @MetaValue@ is not a @MetaMap@.
object :: ParseObject a -> ParseValue a
object act = parseMetaMap >>= embedResult . runParse act . MetaObject

-- | Run a @MetaObject@ parser as a @MetaValue@ parser, wrapping thrown errors
-- in a @MetaWhenParseError@ using the supplied string, and throwing a type
-- error using the supplied string if the @MetaValue@ is not a `MetaMap`. If
-- your `ParseObject` parser already uses `withObjectName` then this function
-- may result in redundant information in errors.
objectNamed :: String -> ParseObject a -> ParseValue a
objectNamed n = object . withObjectName n

-- | Expect a @MetaMap@, parsing it as a @MetaObject@.
objectValue :: FromObject a => ParseValue a
objectValue = object parseObject

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

-- | Run a @MetaValue@ parser on a field if it is present, returning @Just@ the
-- result, and returning @Nothing@ if it is not present.
(.?) :: String -> ParseValue a -> ParseObject (Maybe a)
k .? act = (inspect >>= go . lookupMetaObject k) `catchError` wraperr
  where
    go Nothing  = pure Nothing
    go (Just x) = fmap Just . embedResult $ runParse act x
    wraperr e = throwError $ MetaWhenParseError ("field " <> k) e

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
-- You can use the `parseString`, `parseInlines`, and `parseBlocks` functions if
-- you want to match a `MetaString`, `MetaInlines`, or `MetaBlocks` result and do
-- not want to use the `Text`, `Inlines`, or `Blocks` instances provided.
--
-- No @Map String MetaValue@ or @MetaObject@ instance is given. Use functions
-- like `object`, `objectValue`, and `field` instead. Use `parseMetaMap` if you are
-- sure you need to parse a @Map String MetaValue@.
class FromValue a where
  parseValue :: ParseValue a

-- | Parse a @MetaValue@ in a @Parse i@ monad using its `FromValue` instance.
-- This is used in implementing things like the @FromValue a => FromValue [a]@
-- instance. This can be used, e.g., when you `inspect` a value, but the various
-- `FromValue` instances and parser combinators will be of more use to you than
-- this practice.
parseOfValue :: FromValue a => MetaValue -> Parse i a
parseOfValue = embedResult . fromValue

instance FromValue a => FromValue [a] where
  parseValue = liftResult go
    where
      go x = case x of
        MetaList l -> traverse fromValue l
        z          -> throwTypeError "List" z

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
  parseValue = liftResult go
    where
      go (MetaString s) = pure $ Text.pack s
      go z              = throwTypeError "Text" z

-- | Parse @MetaInlines@ as @Inlines@.
instance FromValue Inlines where
  parseValue = liftResult go
    where
      go x = case x of
        MetaInlines s -> pure $ fromList s
        z             -> throwTypeError "Inlines" z

-- | Parse @MetaBlocks@ as @Blocks@.
instance FromValue Blocks where
  parseValue = liftResult go
   where
     go x = case x of
       MetaBlocks s -> pure $ fromList s
       z            -> throwTypeError "Blocks" z
