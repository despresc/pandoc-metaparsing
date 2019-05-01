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

Simple parsing of Pandoc `Meta` metadata.
-}

module Text.Pandoc.MetaParse
  (
  -- * Using this library
  -- $use

  -- ** Parsing objects, values, and the current input
  -- $metaparse

  -- * Core parser and object types
    Parse
  , Result(..)
  , MetaObject(..)
  , Field
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
  , symbollike
  , charlike
  , parseString
  , parseInlines
  , parseBlocks
  , parseMetaMap
  -- * Errors
  -- ** Core error type
  , MetaError(..)
  , simpleErrorShow
  -- ** Setting expectations
  , (<?>)
  , expect
  , expectFrom
  -- ** Throwing errors
  , throwExpectMessage
  , throwExpectGot
  , throwTypeError
  -- ** Error components
  , Expectation(..)
  , expectationFromList
  , expectationToList
  , MetaErrorMessage(..)
  , toMetaErrorMessage
  , toMetaErrorValue
  , NonEmptyString
  , toNonEmptyString
  , fromNonEmptyString
  , showMetaValueType
  -- * Lower-level inspection and embedding
  , inspect
  , embedResult
  , liftResult
  , parseOfValue
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Fail     (MonadFail)
import qualified Control.Monad.Fail     as Fail
import           Control.Monad.Reader
import           Data.List              (intercalate)
import qualified Data.List.NonEmpty     as NE
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe, mapMaybe)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Text.Pandoc.Builder    (Blocks, Inlines, fromList)
import           Text.Pandoc.Definition
import Data.Semigroup

-- $use
-- Suppose you expect the @contributors@ field of your document's `Meta` to
-- consist of a list of @{ name: [Inline]; location: [Inline]; title: (no-title
-- | [Inline]) }@ entries, with the @title@ field having the default @no-title@
-- if it is not present. You can create the types
--
-- @
--   data ContributorTitle = NoTitle | HasTitle Inlines
--
--   data Contributor = Contributor
--     { name     :: Inlines
--     , location :: [Inline] -- Not Inlines, to show off `.!` and `parseInlines`
--     , title    :: ContributorTitle
--     }
-- @
--
-- and write the instances
--
-- @
--   instance `FromValue` ContributorTitle where
--     `parseValue` = `symbol` "no-title" NoTitle \<|\>
--                  HasTitle \<$\> `parseValue` `<?>` "inline title"
--
--   instance `FromValue` Contributor where
--     `parseValue` = `objectNamed` "contributor" $
--       Contributor \<$\> `field` "name" \<*\> "location" `.!` `parseInlines` \<*\> "title" `.?!` NoTitle
-- @
--
-- Then @`runParseMeta` (`field` "contributors")@ will give a function @`Meta`
-- -> `Result` [Contributor]@. Thrown errors contain basic positional
-- information and an `Error` `MetaError` result can be printed with
-- `simpleErrorShow`.
--
-- There are other possibilities: you can write `FromObject` instances like
--
-- @
--   instance `FromObject` Author where
--     `parseObject` = `withObjectName` "contributor" $
--       Author \<$\> `field` "name" \<*\> "location" `.!` `parseInlines` \<*\> "title" `.?!` NoTitle
--       -- assuming that the ContributorTitle instance is still around
--
--   instance `FromObject` [Contributor] where
--     `parseObject` = `field` "contributors"
-- @
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
-- A `MetaObject` (and so the document `Meta` itself) is a tree-like structure,
-- with branches named by its fields each leading to a `MetaValue`. Inside a
-- `ParseObject` parser we can use functions like `field` and `.!` to parse the
-- values of the fields of the current input with `ParseValue` parsers. In turn,
-- we can expect that the current input of a `ParseValue` parser is a
-- `MetaObject` with functions like `object` and `objectNamed`, which will then
-- be parsed with a `ParseObject` parser.
--
-- While there /is/ a current input @i@ in the @Parse i@ monad (which can be
-- even be `inspect`ed, though you should prefer other functions and combinators
-- instead), there /is not/ a notion of consuming the input. Since the input
-- does not have a specific forward direction we instead have to manually
-- advance deeper into the input using functions like `object` and `.!`. In this
-- way the @Parse i@ monad is more like a @Reader i@ than a typical parser with
-- a stateful input stream. In particular, all parsers fail or succeed without
-- consuming input.

-- | Expected values when parsing.
newtype Expectation = Expectation
  { unExpectation :: Set.Set NonEmptyString
  } deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | Convert `Expectation` to a list of expectations. All of the returned strings will be nonempty.
expectationToList :: Expectation -> [String]
expectationToList = fmap NE.toList . Set.toList . unExpectation

-- | Convert a list of expectations to `Expectation`. Filters empty strings.
expectationFromList :: [String] -> Expectation
expectationFromList = Expectation . Set.fromList . mapMaybe NE.nonEmpty

-- | The type of nonempty strings
type NonEmptyString = NE.NonEmpty Char

-- | Convert a nonempty @String@ to @Just@ a `NonEmptyString`
toNonEmptyString :: String -> Maybe NonEmptyString
toNonEmptyString = NE.nonEmpty

-- | Convert a @NonEmptyString@ to a @String@.
fromNonEmptyString :: NonEmptyString -> String
fromNonEmptyString = NE.toList

-- | The possibilities for a message in a `MetaExpectGotError`. Either we have a
-- `MetaErrorValue` representing a value that was encountered, or we have a
-- `MetaErrorMessage` containing some error message.
data MetaErrorMessage
  = MetaErrorMessage NonEmptyString
  | MetaErrorValue   NonEmptyString
  deriving (Eq, Ord, Show)

-- | Convert a nonempty string to @Just@ a `MetaErrorMessage`.
toMetaErrorMessage :: String -> Maybe MetaErrorMessage
toMetaErrorMessage = fmap MetaErrorMessage . NE.nonEmpty

-- | Convert a nonempty string to @Just@ a `MetaErrorValue`.
toMetaErrorValue :: String -> Maybe MetaErrorMessage
toMetaErrorValue = fmap MetaErrorValue . NE.nonEmpty

-- | `MetaErrorMessage` takes precedence over `MetaErrorValue`. Otherwise left-biased and idempotent.
instance Semigroup MetaErrorMessage where
  MetaErrorMessage x <> _ = MetaErrorMessage x
  _ <> MetaErrorMessage x = MetaErrorMessage x
  MetaErrorValue x <> _   = MetaErrorValue x
  stimes = stimesIdempotent

-- | Possible errors during parsing. The `Semigroup` instance (which is
-- idempotent) determines how the errors are combined with `<|>`. Briefly, we
-- prefer to keep values with constructors that appear earlier in the
-- declaration, and that are leftmost if the constructors are the same. The
-- exception is that two `MetaExpectGotError` values will have their
-- `Expectation` fields merged, using the `Semigroup` instances of `Maybe` and
-- `MetaErrorMessage` to decide what message is kept.
data MetaError
  = MetaExpectGotError Expectation (Maybe MetaErrorMessage) -- ^ @Expected: x, got value: s@ or @Expected: x, got error: s@.
  | MetaFieldUnknown Field -- ^ Unknown field: @k@.
  | MetaWhenParseError String MetaError  -- ^ Got error @e@ when parsing @k@
  | MetaNullError -- ^ Unknown error (thrown by `empty`)
  deriving (Eq, Ord, Show)

instance Semigroup MetaError where
  (<>) = go
    where
      go (MetaExpectGotError e ms) (MetaExpectGotError e' ms') = MetaExpectGotError (e <> e') (ms <> ms')
      go (MetaExpectGotError e s) _ = MetaExpectGotError e s
      go _ (MetaExpectGotError e s) = MetaExpectGotError e s
      go (MetaFieldUnknown s) _ = MetaFieldUnknown s
      go _ (MetaFieldUnknown s) = MetaFieldUnknown s
      go (MetaWhenParseError k e) _ = MetaWhenParseError k e
      go _ (MetaWhenParseError k e) = MetaWhenParseError k e
      go MetaNullError MetaNullError = MetaNullError
  stimes = stimesIdempotentMonoid

instance Monoid MetaError where
  mempty = MetaNullError

-- | Show the branch of the argument, without the @Meta@ part, except that
-- @MetaString@ is shown as @Text@ and @MetaMap@ is shown as @Object@. Used for
-- simple type errors.
showMetaValueType :: MetaValue -> NonEmptyString
showMetaValueType x = NE.fromList $ case x of
  MetaString _  -> "Text"
  MetaBool _    -> "Bool"
  MetaMap _     -> "Object"
  MetaList _    -> "List"
  MetaInlines _ -> "Inlines"
  MetaBlocks _  -> "Blocks"

-- | Display errors roughly as described in `MetaError`.
--
--   * If a `MetaExpectGotError` is entirely empty, we print @some error@. If the `Expectation` field is non-empty we print @expected: @ then a comma-separated list of the contents of `Expectation`. If the `MetaErrorMessage` is not `Nothing`, we print @got error: @ or @got value: @ followed by the message, depending on its contents. If both are present we print these on separate lines.
--   * For a @`MetaFieldUnknown` k@ error we print @unknown field: k@.
--   * For @`MetaWhenParseError` e f@, we first print @`simpleErrorShow` e@, then print @when parsing: f@ on a new line.
simpleErrorShow :: MetaError -> String
simpleErrorShow (MetaExpectGotError e m)
  | Set.null (unExpectation e) = maybe "some error" meshow m
  | otherwise                  = "expected: " <> showExp e <> maybe "" (("\n" <>) . meshow) m
  where
    meshow (MetaErrorMessage x) = "got error: " <> NE.toList x
    meshow (MetaErrorValue x)   = "got value: " <> NE.toList x
    showExp = intercalate ", " . expectationToList
simpleErrorShow (MetaFieldUnknown f) = "unknown field: " <> f
simpleErrorShow (MetaWhenParseError k e) = simpleErrorShow e <> "\nwhen parsing: " <> k
simpleErrorShow MetaNullError = "unknown error"

-- | Throw a simple type error, printing the branch of the @MetaValue@ as what was received.
--
-- > throwTypeError s (MetaString x) = throwExpectGot s "String"
throwTypeError :: MonadError MetaError m
               => String -- ^ What was expected.
               -> MetaValue -- ^ What was received
               -> m a
throwTypeError s = throwError . MetaExpectGotError (expectationFromList [s]) . Just . MetaErrorMessage . showMetaValueType

-- | Throw an expectation/value error (`MetaErrorValue` inside a `MetaExpectGotError`). Filters empty strings out of the error.
throwExpectGot :: MonadError MetaError m
               => [String] -- ^ What was expected.
               -> String   -- ^ The value that was received.
               -> m a
throwExpectGot e = throwError . MetaExpectGotError (expectationFromList e) . toMetaErrorValue

-- | Throw an expectation/message error (`MetaErrorMessage` inside a
-- `MetaExpectGotError`). Filters empty strings out of the error. Note that the
-- value in a `MetaErrorMessage` takes precedence over that of a `MetaErrorValue`.
throwExpectMessage :: MonadError MetaError m
                => [String] -- ^ What was expected
                -> String   -- ^ An error that occurred.
                -> m a
throwExpectMessage e = throwError . MetaExpectGotError (expectationFromList e) . toMetaErrorMessage

-- | The result of a parser.
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

instance Fail.MonadFail Result where
  fail = throwError . MetaExpectGotError mempty . toMetaErrorMessage

-- | A parser for an input @i@.
--
-- For error handling, note that the implementations of `empty` and `mzero`
-- throw the weakest `MetaNullError` (which will discarded in favour of other
-- errors or a successful result by `<|>` and `mplus`). Use `fail` to throw an
-- error message with no set `Expectation` and a particular `MetaErrorMessage`,
-- or use `throwExpectMessage` to do the same but with a given `Expectation` as
-- well.
newtype Parse i a = Parse
  { unParse :: ReaderT i Result a
  } deriving ( Functor, Applicative, Monad, Alternative, MonadPlus, MonadError MetaError, MonadFail)

-- | Inspect the input under consideration (usually a `MetaValue` or a `MetaObject`)
inspect :: Parse i i
inspect = Parse ask

-- | Combine two successful parse results with the @<>@ of @a@.
instance Semigroup a => Semigroup (Parse i a) where
  (<>) = liftA2 (<>)

-- | Succeed and return `mempty`
instance Monoid a => Monoid (Parse i a) where
  mempty = pure mempty

-- | A parser for a `MetaValue`.
type ParseValue  = Parse MetaValue

-- | A parser for a `MetaObject`.
type ParseObject = Parse MetaObject

-- | The name of a field in an object.
type Field = String

-- | A wrapper for the @MetaMap@ branch of @MetaValue@, treated as an @Object@ with the keys being fields.
newtype MetaObject = MetaObject
  { unObject :: Map Field MetaValue
  } deriving (Eq, Ord, Show, Semigroup, Monoid)

-- Should export things like this if I ever write writers for @MetaValue@ and @MetaObject@.
lookupMetaObject :: Field -> MetaObject -> Maybe MetaValue
lookupMetaObject k = Map.lookup k . unObject

-- | Run an explicit parser.
runParse :: Parse i a -> i -> Result a
runParse = runReaderT . unParse

-- | Parse `Meta` by treating it as a `MetaObject` and running the given
-- `ParseObject` parser. Useful if you have a `MetaObject` parser that parses
-- various fields and returns a result without restricting what fields may be
-- present (so using this and `onlyFields` will most likely not work if the
-- `Meta` comes from a raw document). See also `fromMeta`.
runParseMeta :: ParseObject a -> Meta -> Result a
runParseMeta act = runParse act . rewrap
  where
    rewrap (Meta m) = MetaObject m

-- | Parse a `MetaValue` with a `FromValue` instance, returning a `Result`.
fromValue :: FromValue a => MetaValue -> Result a
fromValue = runParse parseValue

-- | Parse a `MetaObject` with a `FromObject` instance, returning a `Result`.
fromObject :: FromObject a => MetaObject -> Result a
fromObject = runParse parseObject

-- | Parse `Meta` by treating it as a `MetaObject` and using a `FromObject`
-- instance, returning a `Result`. The remarks about `runParseMeta` apply here
-- as well.
fromMeta :: FromObject a => Meta -> Result a
fromMeta = fromObject . rewrap
  where
    rewrap (Meta m) = MetaObject m

-- | Read a field of `Meta` using its `FromValue` instance.
fromMetaField :: FromValue a => Field -> Meta -> Result a
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
-- This behaves like so:
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

-- | Wrap the errors thrown by the given parser in a `MetaWhenParseError` using
-- the given string. Used in `objectNamed` and useful when writing `FromObject`
-- instances that might benefit from an annotated error.
withObjectName :: String -> ParseObject a -> ParseObject a
withObjectName s = flip catchError go
  where
    go = throwError . MetaWhenParseError s

-- | Succeeds on @`MetaString` [x]@ and @`MetaInlines` [`Str` [x]]@. Can be used like `symbollike` is used in `symbol`.
charlike :: ParseValue Char
charlike = (symbollike >>= go) <?> "Char"
  where
    go [x] = pure x
    go _   = empty

-- | Succeeds on @`MetaString` s@ and @`MetaInlines` [`Str` s]@. Used in `symbol` and `symbolFrom`.
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
-- We use `symbollike` and not the `parseString` or the `Text` instance of
-- `parseValue` because Pandoc will parse bare text as @[Inline]@ and we would
-- like to allow users to write symbols without enclosing them in quotation
-- marks if possible. Notably, symbols containing whitespace or markdown
-- characters will still need to be written between quotation marks, otherwise
-- Pandoc would return @MetaInlines@ containing more than just a @Str@.
symbol :: String -> a -> ParseValue a
symbol sym a = symbolFrom [(sym, a)]

-- | Parse a symbol from a given table. The parser @symbolFrom tbl@ succeeds if
-- `symbollike` returns @sym@ and @sym@ is found in the table. If @sym@ is not,
-- we throw an error with an @unexpected symbol sym@ message in a
-- `MetaErrorMessage`, using the symbols themselves as the `Expectation`.
symbolFrom :: [(String, a)] -> ParseValue a
symbolFrom tbl = symbollike >>= lookStr
  where
    expects = fst <$> tbl
    lookStr x = case lookup x tbl of
      Just a  -> pure a
      Nothing -> throwExpectMessage expects ("unexpected symbol " <> x)

-- | Succeeds on `MetaMap`. You most likely want to use functions like `object`,
-- `objectValue`, and `.!` instead of this function.
parseMetaMap :: ParseValue (Map String MetaValue)
parseMetaMap = liftResult go
  where
    go x = case x of
      MetaMap m -> pure m
      z         -> throwTypeError "Object" z

-- | Succeeds on `MetaString`.
parseString :: ParseValue String
parseString = liftResult go
  where
    go x = case x of
      MetaString s -> pure s
      z            -> throwTypeError "String" z

-- | Succeeds on `MetaInlines`.
parseInlines :: ParseValue [Inline]
parseInlines = liftResult go
  where
    go x = case x of
      MetaInlines s -> pure s
      z             -> throwTypeError "Inlines" z

-- | Succeeds on `MetaBlocks`.
parseBlocks :: ParseValue [Block]
parseBlocks = liftResult go
  where
    go x = case x of
      MetaBlocks s -> pure s
      z            -> throwTypeError "Blocks" z

-- | Expect that the `MetaValue` input is a @MetaMap@ and parse it as a
-- `MetaObject` using the supplied parser, otherwise throwing a type error.
object :: ParseObject a -> ParseValue a
object act = parseMetaMap >>= embedResult . runParse act . MetaObject

-- | Construct a parser as with `object`, except that any errors thrown by the
-- supplied parser will be wrapped in a `MetaWhenParseError` using the supplied
-- string. If your `ParseObject` parser already uses `withObjectName` then this
-- function may result in redundant information in errors.
--
-- > objectNamed n = object . withObjectName n
objectNamed :: String -> ParseObject a -> ParseValue a
objectNamed n = object . withObjectName n

-- | Use `parseObject` as the parser for `object`.
objectValue :: FromObject a => ParseValue a
objectValue = object parseObject

-- | Check that an object has only (but not necessarily all of) the listed
-- fields, proceeding with parsing if so, throwing a `MetaFieldUnknown` error
-- with the name of one of the unknown fields otherwise.
onlyFields :: [Field] -> ParseObject a -> ParseObject a
onlyFields = (>>) . go . Set.fromList
  where
    go ks = do
      fs <- Map.keysSet . unObject <$> inspect
      case Set.toList (fs `Set.difference` ks) of
        []    -> pure ()
        (x:_) -> throwError (MetaFieldUnknown x)

-- | Throw a `MetaFieldUnknown` error if the given field is present.
guardNoField :: Field -> ParseObject ()
guardNoField k = inspect >>= go . lookupMetaObject k
  where
    go Nothing  = pure ()
    go (Just _) = throwError (MetaFieldUnknown k)

-- $field
-- Note that all of these functions wrap thrown errors during the parsing of a
-- field @k@ in a `MetaWhenParseError` using the field name. The ones that
-- expect a field to be present throw a `MetaExpectGotError` if it is not, again
-- using the field name.

-- | If the given field is present in the input, parse its value using the given
-- `ParseValue` parser and return @Just@ the result. Otherwise, return
-- @Nothing@. See also `maybeField`.
(.?) :: Field -> ParseValue a -> ParseObject (Maybe a)
k .? act = (inspect >>= go . lookupMetaObject k) `catchError` wraperr
  where
    go Nothing  = pure Nothing
    go (Just x) = fmap Just . embedResult $ runParse act x
    wraperr e = throwError $ MetaWhenParseError ("field " <> k) e

-- | If the given field is present in the input, parse its value using the given
-- `ParseValue` parser and return @Just@ the result. Otherwise, throw an error.
-- See also `field`.
(.!) :: Field -> ParseValue a -> ParseObject a
k .! act = (k .? act) >>= go
  where
    go Nothing  = throwError $ MetaExpectGotError (expectationFromList ["present field " <> k]) Nothing
    go (Just a) = pure a

-- | Use `parseValue` as the parser in `.!`.
--
-- > field k = k .! parseValue
field :: FromValue a => Field -> ParseObject a
field = flip (.!) parseValue

-- | Use `parseValue` as the parser in `.?`.
--
-- > maybeField k = k .? parseValue
maybeField :: FromValue a => Field -> ParseObject (Maybe a)
maybeField = flip (.?) parseValue

-- | Use `parseValue` as the parser in `.?`, returning the supplied
-- default if the field is not present.
(.?!) :: FromValue a => Field -> a -> ParseObject a
k .?! x = maybeF x $ maybeField k

-- | Give a default value to a parser returning @Maybe a@.
maybeF :: Functor f => a -> f (Maybe a) -> f a
maybeF = fmap . fromMaybe

-- | Things that can be read from a `MetaObject`. Given for writing your own
-- instances. You can `inspect` the underlying `MetaObject`, but the provided
-- combinators like `.!` and `field` should be more convenient.
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
