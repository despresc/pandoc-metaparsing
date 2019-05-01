# Pandoc Meta Parsing

Library for parsing [Pandoc](https://github.com/jgm/pandoc) `Meta` metadata. The
design is similar in some respects to the JSON parsing facilities of
[aeson](http://hackage.haskell.org/package/aeson), but reflects the different
nature of the Pandoc `MetaValue` type and some different design decisions. In
particular, the error type is more complex (using a mechanism of "expected
values" similar to megaparsec), and the parsers in the library have a notion of
"current input", in contrast to the JSON `Object` parsers of aeson. However,
unlike normal parsers the input is a _value_ and not a _stream_ (since objects
with fields don't have a defined forward direction), and so the current input
has to be advanced deeper into composite objects manually; these parsers do not
consume their input. The `FromValue` and `FromObject` classes and various
predefined parsers and combinators make this process relatively painless.

## Example

If you expect the metadata of your document to have the field `contributors`
with the scheme

```yaml
contributors:
- name: [Inline]
  location: [Inline]
  title: (no-title | [Inline])
```

you can write Haskell types

```haskell
data ContributorTitle = NoTitle | HasTitle Inlines

data Contributor = Contributor
  { name :: Inlines
  , location :: [Inline] -- Not Inlines, to show off .! and parseInlines
  , title :: ContributorTitle
  }

type Contributors = [Contributor]
```

and write instances of `FromValue`

```haskell
instance FromValue ContributorTitle where
  parseValue = symbol "no-title"  NoTitle  <|>
               ContributorTitle <$> parseValue <?> "inline title"

instance FromValue Contributor where
  parseValue = objectNamed "contributor" $
    Contributor <$> field "name" <*> "location" .! parseInlines <*> "title" .?! NoTitle
```

so that `fromMetaField "contributors"` will return either `Error MetaError` or
`Success Contributors`. We've used `.!?` to parse the possibly-not-present
`title` field by giving it the default value `NoTitle`, and `.!` to parse the
field of an object with an explicit parser `parseInlines`.

## Meta parser reference

A typical parser will consist of `ParseValue a = Parse MetaValue a` and
`ParseObject a = Parse MetaObject a` parsers, with functions like `.!`, `field`,
and `object` linking the two together. Note that `MetaObject` is a newtype
wrapper over `Map String MetaValue`, since we think of `MetaMap` as being a YAML
object, its keys being fields.

A sample of commonly used parsers and combinators from the library is given below.

```haskell
class FromValue a where
  parseValue :: ParseValue a
  -- Parse the input MetaValue with the given instance, returning a type a.
  parseListValue :: ParseValue [a]
  -- By default, expect a MetaList of values that parse as the type a with the
  -- parseValue instance. Used for the String, Inline, and Block instances. See
  -- the note on list instances below.

object p :: ParseValue a
-- Expect the input to be a MetaMap, parsing it with the given p :: MetaObject
-- parser.

p <?> s :: ParseValue a
-- Replace what was expected from the ParseValue parser p with the string s.

f .! p :: ParseObject a
-- Expect the field f of the input MetaObject to be set, parsing its value with
-- the p :: ParseValue a parser

f .? p :: ParseObject (Maybe a)
-- Parse the field f with p :: ParseValue a if it is present, returning Just the
-- result. Otherwise return Nothing.

field f :: ParseObject a
-- Expect the field f to be set, parsing its value with parseValue

onlyFields fs p :: ParseObject a
-- Require before executing p :: ParseObject a that the object only have fields
-- that are listed in fs :: [String]. Useful for detecting user error when
-- fields can have default values (otherwise a misspelled field name might be
-- silently ignored)

f .?! a :: ParseObject a
-- Parse the value of the field f with parseValue if it is present. Otherwise return a.

symbollike :: ParseValue String
-- Expect the input to be MetaString s (returning s), or MetaInlines [Str s]
-- (returning s). Useful when the input is a symbol standing for an option, like
-- the no-title above, and you do not want to require the user to enclose the
-- option in quotation marks. Used in symbol and symbolFrom for that reason.
```

## Errors

The basic error returned by most parsers is a `MetaExpectGotError e ms`,
consisting of a set `e :: Expectation` of expected values and an `ms :: Maybe
MetaErrorMessage` representing what was received. The `Monoid` instance of
`MetaError` and the `Alternative` instance of `Parse i` are written so that
`Expectation` fields of errors are combined when possible.

The field parsing functions like `field k` and `(k .!)` automatically catch
errors thrown by their parsers and wrap them in `MetaWhenParseError ("field " <>
k)`. This adds simple positional information to errors. There are also the
`objectNamed :: FromObject a => String -> ParseValue a` and `withObjectName ::
String -> ParseObject a -> ParseObject a` functions that take their string
argument `s` and wrap thrown errors from their parsers in `MetaWhenParseError
s`. Some care is required: if a `FromObject` parser already names its output
using something like `withObjectName`, then using `objectNamed` will most likely
add redundant information to errors.
