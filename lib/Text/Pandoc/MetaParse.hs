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
  ( Parse
  , Result(..)
  -- * Things that can be parsed from a @MetaValue@
  , FromMetaValue(..)
  -- ** Functions for writing @MetaValue@ parsers
  , symbol
  , symbolFrom
  , stringlike
  , weakString
  , weakInlines
  , weakBlocks
  -- ** Functions for writing @Meta@ parsers
  , onMeta
  , key
  , keyMaybe
  -- * Errors
  -- ** Type of errors
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

-- | A parser for @Meta@ and @MetaValue@.
newtype Parse i a = Parse
  { unParse :: ReaderT i Result a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadReader i, MonadError MetaError)

-- | Run an explicit parser.
runParse :: Parse i a -> i -> Result a
runParse = runReaderT . unParse

-- | Convert a `MetaValue`, returning a `Result`.
runParseValue :: FromMetaValue a => MetaValue -> Result a
runParseValue = runParse parseValue

-- | Embed a `Result` in a `Parse` action.
embedResult :: Result a -> Parse i a
embedResult = Parse . ReaderT . const

-- | Use a function as a `Parse` action.
liftResult :: (i -> Result a) -> Parse i a
liftResult = Parse . ReaderT

infix 0 <?>

-- | With @act \<?\> e@, catch a @MetaError@ from @act@, replacing a @MetaExpectGotError _ g@ with @MetaExpectGotError e g@ and replacing any other error with @MetaExpectError e@.
(<?>) :: MonadError MetaError m => m a -> String -> m a
(<?>) = flip expect

-- | Flipped `<?>`.
expect :: MonadError MetaError m => String -> m a -> m a
expect e = flip catchError go
  where
    go (MetaExpectGotError _ g) = throwExpectGot e g
    go _                        = throwExpect e

-- | Expect a "symbol", a string representing an element of a Haskell type @a@. The parser @symbol sym x@ succeeds and returns @x@ if `stringlike` returns @sym@.
symbol :: String -> a -> Parse MetaValue a
symbol sym a = symbolFrom [(sym, a)]

-- | Expect a symbol from a given table.
symbolFrom :: [(String, a)] -> Parse MetaValue a
symbolFrom tbl = parseValue >>= go
  where
    err = intercalate ", " . fmap fst $ tbl
    lookStr x = case lookup x tbl of
      Just a  -> pure a
      Nothing -> throwExpectGot err x
    go x = case x of
      MetaString s  -> lookStr s
      MetaInlines s -> lookStr (stringify s)
      MetaBlocks s  -> lookStr (stringify s)
      z             -> throwTypeError err z

-- | Parse something other than a list or map and stringify it.
stringlike :: Parse MetaValue String
stringlike = liftResult go
  where
    go x = case x of
      MetaString s  -> pure s
      MetaInlines s -> pure $ stringify s
      MetaBlocks s  -> pure $ stringify s
      MetaBool b    -> pure $ if b then "true" else "false"
      z             -> throwTypeError "String-like " z

-- | A @String@ parser that only succeeds on @MetaString@.
weakString :: Parse MetaValue String
weakString = liftResult go
  where
    go x = case x of
      MetaString s -> pure s
      MetaList l   -> traverse runParseValue l
      z            -> throwTypeError "String" z

-- | An @[Inline]@ parser that only succeeds on @MetaInlines@
weakInlines :: Parse MetaValue [Inline]
weakInlines = liftResult go
  where
    go x = case x of
      MetaInlines s -> pure s
      MetaList l   -> traverse runParseValue l
      z             -> throwTypeError "Inlines" z


-- | A @[Block]@ parser that only succeeds on @MetaBlocks@.
weakBlocks :: Parse MetaValue [Block]
weakBlocks = liftResult go
  where
    go x = case x of
      MetaBlocks s -> pure s
      MetaList l   -> traverse runParseValue l
      z            -> throwTypeError "Blocks" z

-- | Run a @Meta@ parser as a @MetaValue@ parser, expecting a @MetaMap@. Used for interacting with @MetaMap@.
onMeta :: Parse Meta a -> Parse MetaValue a
onMeta act = parseValue >>= embedResult . runParse act . Meta

-- | Read a value from a key, throwing an error if the key is not set.
key :: FromMetaValue a => String -> Parse Meta a
key k = keyMaybe k >>= go
  where
    go Nothing  = throwExpect $ "the key " <> k <> " to be set"
    go (Just a) = pure a

-- | Read a value from a key, returning `Just a` if the key is set and `Nothing` if the key is not set. A lifted version of @lookupMeta@ that parses its result.
keyMaybe :: FromMetaValue a => String -> Parse Meta (Maybe a)
keyMaybe k = ask >>= traverse fromValue . lookupMeta k

-- | Things that can be read from a @MetaValue@.
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
class FromMetaValue a where
  parseValue :: Parse MetaValue a
  parseListValue :: Parse MetaValue [a]

  parseListValue = liftResult go >>= traverse fromValue
    where
      go x = case x of
        MetaList s -> pure s
        z          -> throwTypeError "List" z

-- | Parse a meta value.
fromValue :: FromMetaValue a => MetaValue -> Parse i a
fromValue = embedResult . runParseValue

instance FromMetaValue a => FromMetaValue [a] where
  parseValue = parseListValue

instance FromMetaValue MetaValue where
  parseValue = ask

instance FromMetaValue Bool where
  parseValue = liftResult go
    where
      go x = case x of
        MetaBool b -> pure b
        z          -> throwTypeError "Bool" z

-- | Expect a @MetaString@, parse it as @Text@.
instance FromMetaValue Text where
  parseValue = Text.pack <$> parseValue <?> "text"

-- | Parse @MetaString [x]@ as @Char@ and @MetaString s@ as @String@
instance FromMetaValue Char where
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

-- | Wrap a @MetaMap@ in @Meta@. Try using `onMeta` and functions like `key` before using this.
instance FromMetaValue Meta where
  parseValue = onMeta ask

-- | Try using `onMeta` and functions like `key` before using this.
instance FromMetaValue (Map String MetaValue) where
  parseValue = liftResult go
    where
      go x = case x of
        MetaMap m -> pure m
        z         -> throwTypeError "Map" z

-- | Parse @MetaInlines [x]@ as @Inline@ and @MetaInlines s@ as @[Inline]@
instance FromMetaValue Inline where
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
instance FromMetaValue Block where
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
instance FromMetaValue Inlines where
  parseValue = fromList <$> parseValue

-- | Parse @MetaBlocks@ as @Blocks@
instance FromMetaValue Blocks where
  parseValue = fromList <$> parseValue
