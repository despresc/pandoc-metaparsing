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

Simple parsing of Pandoc `Meta` metadata in the style of the Aeson library.
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
  , runParse
  , runParseValue
  , runParseObject
  , parseMeta
  , parseMetaWith
  -- * Parsing @MetaObject@ and @MetaValue@ values
  -- ** @MetaObject@ parsers
  , FromObject(..)
  , fieldWith
  , maybeFieldWith
  , field
  , maybeField
  , (.!=)
  -- ** @MetaValue@ parsers
  , FromValue(..)
  , object
  , objectWith
  , symbol
  , symbolFrom
  , symbolLike
  , stringified
  , metaMap
  , weakString
  , weakInlines
  , weakBlocks
  -- * Errors
  , MetaError(..)
  -- ** Modifying thrown errors
  , (<?>)
  , expect
  -- ** Throwing errors
  , throwTypeError
  , throwExpectGot
  , throwExpect
  ) where

import           Control.Applicative
import Data.Maybe (fromMaybe)
import           Control.Monad.Except
import qualified Control.Monad.Fail   as Fail
import           Control.Monad.Reader
import           Data.List            (intercalate)
import           Data.Map             (Map)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Text.Pandoc
import           Text.Pandoc.Builder  (Blocks, Inlines, fromList)
import           Text.Pandoc.Shared   (stringify)
import qualified Data.Map as Map

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
-- >   parseValue = objectWith $ Author <$> field "name" <*> field "location"
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
-- to reflect this.
--
-- The `ParseObject` and `ParseValue` monads are linked by two families of functions.
--
--   * In one direction, we have functions like `field` and `fieldWith` that parse the fields of the input `MetaObject` using `ParseValue` parsers.
--
--   * In the other direction, we have the functions `object` and `objectWith` that parse the input `MetaValue` using `ParseObject` parsers.

-- | Possible errors during parsing
data MetaError
  = MetaExpectGotError String String -- ^ Expected @x@, got @y@
  | MetaExpectError String           -- ^ Expected @x@
  | MetaOtherError String            -- ^ Other errors
  deriving (Eq, Ord, Show)

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
throwTypeError s = throwError . MetaExpectGotError s . showMetaValueType

-- | Throw an expectation error if we can show what we received.
throwExpectGot :: MonadError MetaError m
               => String -- ^ What was expected.
               -> String -- ^ What was received.
               -> m a
throwExpectGot e = throwError . MetaExpectGotError e

-- | Throw an expectation error if there is not a good way of showing what we received.
throwExpect :: MonadError MetaError m
            => String -- ^ What was expected.
            -> m a
throwExpect = throwError . MetaExpectError

-- | The result of a parse.
data Result a
  = Error MetaError
  | Success a
  deriving Show

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

instance Fail.MonadFail Result where
  fail = throwError . MetaOtherError

instance Alternative Result where
  empty = Fail.fail "empty"
  z@(Success _) <|> _ = z
  Error _       <|> x = x

instance MonadPlus Result where

-- | A parser for an input @i@.
newtype Parse i a = Parse
  { unParse :: ReaderT i Result a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadReader i, MonadError MetaError)

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

infix 0 <?>

-- | With @act \<?\> e@, catch a @MetaError@ from @act@, replacing a
-- @MetaExpectGotError _ g@ with @MetaExpectGotError e g@ and replacing any other
-- error with @MetaExpectError e@.
(<?>) :: MonadError MetaError m => m a -> String -> m a
(<?>) = flip expect

-- | Flipped `<?>`.
expect :: MonadError MetaError m => String -> m a -> m a
expect e = flip catchError go
  where
    go (MetaExpectGotError _ g) = throwExpectGot e g
    go _                        = throwExpect e

-- | Succeeds on @MetaString s@ and @MetaInlines [Str s]@. Used in `symbol` and `symbolFrom`.
symbolLike :: ParseValue String
symbolLike = liftResult go
  where
    go x = case x of
      MetaString s -> pure s
      MetaInlines [Str s] -> pure s
      z -> throwTypeError "symbol" z

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
      Nothing -> throwExpectGot err x

-- | Parse a @MetaMap@. You most likely want to use functions like `object`, `objectWith`, and `field` instead of this function.
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
-- entries parse as @Char@.
weakString :: ParseValue String
weakString = liftResult go
  where
    go x = case x of
      MetaString s -> pure s
      MetaList l   -> traverse runParseValue l
      z            -> throwTypeError "String" z

-- | An @[Inline]@ parser that succeeds on @MetaInlines@ and on a @MetaList@
-- whose entries parse as @Inline@.
weakInlines :: ParseValue [Inline]
weakInlines = liftResult go
  where
    go x = case x of
      MetaInlines s -> pure s
      MetaList l    -> traverse runParseValue l
      z             -> throwTypeError "Inlines" z


-- | A @[Block]@ parser that succeeds on @MetaBlocks@ and on a @MetaList@ whose
-- entries parse as @Block@.
weakBlocks :: ParseValue [Block]
weakBlocks = liftResult go
  where
    go x = case x of
      MetaBlocks s -> pure s
      MetaList l   -> traverse runParseValue l
      z            -> throwTypeError "Blocks" z

-- | Run a @MetaObject@ parser as a @MetaValue@ parser, throwing an error if the
-- @MetaValue@ is not a @MetaMap@. One main link between the `ParseObject` and
-- `ParseValue` parsers (the others being the various @field@ functions).
objectWith :: ParseObject a -> ParseValue a
objectWith act = metaMap >>= embedResult . runParse act . MetaObject

-- | Expect a @MetaMap@, parsing it as @MetaObject@.
object :: FromObject a => ParseValue a
object = objectWith parseObject

-- | Run a @MetaValue@ parser on the value of a key if it is set, returning
-- @Just a@ if the key was set and @Nothing@ if it was not.
maybeFieldWith :: String -> ParseValue a -> ParseObject (Maybe a)
maybeFieldWith k act = ask >>= go . lookupMetaObject k
  where
    go Nothing  = pure Nothing
    go (Just x) = fmap Just . embedResult $ runParse act x

-- | Run a @MetaValue@ parser on a key, throwing an error if the key is not set.
fieldWith :: String -> ParseValue a -> ParseObject a
fieldWith k act = maybeFieldWith k act >>= go
  where
    go Nothing  = throwExpect $ "the key " <> k <> " to be set"
    go (Just a) = pure a

-- | Parse the value of a field, throwing an error if the key is not set.
--
-- > field k = fieldWith k parseValue
field :: FromValue a => String -> ParseObject a
field = flip fieldWith parseValue

-- | Parse the value of a field if it is set and return @Just@ the result.
-- Otherwise return @Nothing@.
--
-- > maybeField k = maybeFieldWith k parseValue
maybeField :: FromValue a => String -> ParseObject (Maybe a)
maybeField = flip maybeFieldWith parseValue

infix 1 .!=

-- | Give a default value to a parser returning @Maybe a@. Useful for parsing
-- optional fields.
(.!=) :: Parse i (Maybe a) -> a -> Parse i a
(.!=) = flip $ fmap . fromMaybe

-- | Things that can be read from a @MetaObject@.
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
-- like `object`, `objectWith`, and `field` instead. Use `metaMap` if you are
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
  parseValue = ask

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
      go (MetaString _)   = throwExpect "String of length 1"
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
        MetaInlines _   -> throwExpect "Inlines of length 1"
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
       MetaBlocks _   -> throwExpect "Blocks of length 1"
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
