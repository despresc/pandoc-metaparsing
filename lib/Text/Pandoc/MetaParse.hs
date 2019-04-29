{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Text.Pandoc.MetaParse where

import           Control.Applicative
import           Control.Monad.Except
import qualified Control.Monad.Fail   as Fail
import           Control.Monad.Reader
import           Data.List            (intercalate)
import           Data.Map             (Map)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Text.Pandoc
import           Text.Pandoc.Shared   (stringify)

-- * Simple parsing of Pandoc `Meta` metadata.

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

-- | Throw a type error.
throwTypeError :: MonadError MetaError m => String -> MetaValue -> m a
throwTypeError s v = throwError $ MetaExpectGotError s (showMetaValueType v)

-- | Throw an expectation error if we know what we got.
throwExpectGot :: MonadError MetaError m => String -> String -> m a
throwExpectGot e = throwError . MetaExpectGotError e

-- | Throw an expectation error if we don't know what we got.
throwExpect :: MonadError MetaError m => String -> m a
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

runParse :: Parse i a -> i -> Result a
runParse = runReaderT . unParse

runParseValue :: FromMetaValue a => MetaValue -> Result a
runParseValue = runParse parseValue

embedResult :: Result a -> Parse i a
embedResult = Parse . ReaderT . const

liftResult :: (i -> Result a) -> Parse i a
liftResult = Parse . ReaderT

-- | With @expect e@, catch a @MetaError@, replacing the expected string with @e@.
expect :: MonadError MetaError m => String -> m a -> m a
expect e = flip catchError go
  where
    go (MetaExpectGotError _ g) = throwExpectGot e g
    go _                        = throwExpect e

infix 0 <?>

-- | Flipped `expect` of low fixity.
(<?>) :: MonadError MetaError m => m a -> String -> m a
(<?>) = flip expect

-- | Expect a "symbol" - a string representing some internal type. The error handling could be better.
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

-- | Expect something other than a list or map and stringify it.
stringlike :: Parse MetaValue String
stringlike = liftResult go
  where
    go x = case x of
      MetaString s  -> pure s
      MetaInlines s -> pure $ stringify s
      MetaBlocks s  -> pure $ stringify s
      MetaBool b    -> pure $ if b then "true" else "false"
      z             -> throwTypeError "String-like " z

-- | Expect a `MetaString`. A type-restricted version of `parseValue`.
string :: Parse MetaValue String
string = liftResult go
  where
    go x = case x of
      MetaString s -> pure s
      z            -> throwTypeError "String" z

-- | Expect a `MetaList`. A type-restricted version of `parseValue`.
list :: Parse MetaValue [MetaValue]
list = liftResult go
  where
    go x = case x of
      MetaList s -> pure s
      z          -> throwTypeError "List" z

-- | Expect a `MetaMap`. A type-restricted version of `parseValue`. Try using @onMeta@ and functions like @key@ before using this.
metaMap :: Parse MetaValue (Map String MetaValue)
metaMap = liftResult go
  where
    go x = case x of
      MetaMap m -> pure m
      z         -> throwTypeError "Map" z

-- | Expect a `MetaBool`. A type-restricted version of `parseValue`.
bool :: Parse MetaValue Bool
bool = liftResult go
  where
    go x = case x of
      MetaBool b -> pure b
      z          -> throwTypeError "Bool" z

-- | Run a @ParseMeta@, expecting a @MetaMap@. Used for interacting with @MetaMap@.
onMeta :: Parse Meta a -> Parse MetaValue a
onMeta act = metaMap >>= embedResult . runParse act . Meta

-- | Read a value from a key, throwing an error if the key is not set.
key :: FromMetaValue a => String -> Parse Meta a
key k = keyMaybe k >>= go
  where
    go Nothing  = throwExpect $ "the key " <> k <> " to be set"
    go (Just a) = pure a

-- | Read a value from a key, returning `Just a` if the key is set and `Nothing` if the key is not set. A lifted version of @lookupMeta@ that parses its result.
keyMaybe :: FromMetaValue a => String -> Parse Meta (Maybe a)
keyMaybe k = ask >>= traverse fromValue . lookupMeta k

-- | Things that can be read from meta values.
class FromMetaValue a where
  parseValue :: Parse MetaValue a
  parseListValue :: Parse MetaValue [a] -- ^ So we can have a sensible result when parsing a String

  parseListValue = list >>= traverse fromValue

-- | Parse a meta value.
fromValue :: FromMetaValue a => MetaValue -> Parse i a
fromValue = embedResult . runParseValue

instance FromMetaValue a => FromMetaValue [a] where
  parseValue = parseListValue

instance FromMetaValue MetaValue where
  parseValue = ask

instance FromMetaValue Bool where
  parseValue = bool

-- | Expect a string, parse it as @Text@.
instance FromMetaValue Text where
  parseValue = Text.pack <$> parseValue <?> "text"

instance FromMetaValue Char where
  parseValue = liftResult go
    where
      go (MetaString [x]) = pure x
      go (MetaString _)   = throwExpect "a string of length 1"
      go z                = throwTypeError "a character" z
  parseListValue = string -- ^ Important: We don't consider a MetaList of singleton MetaStrings to be a @String@.

instance FromMetaValue Meta where
  parseValue = onMeta ask
