# Pandoc Meta Parsing

Library for parsing [Pandoc](https://github.com/jgm/pandoc) `Meta` metadata.

## Example

If you expect YAML metadata in your document with the scheme

```yaml
authors: [ { name: [Inline];
             location: [Inline];
             title : (no-title | [Inline]) } ]
```

you can write Haskell types

```haskell
data AuthorTitle = NoTitle | AuthorTitle [Inline]

data Author = Author
  { name :: [Inline]
  , location :: [Inline]
  , title :: AuthorTitle
  }

type Authors = [Author]
```

and write instances of `FromValue`

```haskell
instance FromValue AuthorTitle where
  parseValue = symbol "no-title"  NoTitle <|> AuthorTitle <$> parseValue <?> "no-title, inline title"

instance FromValue Author where
  parseValue = object $ Author <$> field "name" <*> field "location" <*> ("title" .!? NoTitle)
```

so that `parseMetaField "authors"` will return either `Error MetaError` or
`Success Authors`. We've used `.!?` to parse the possibly-not-present `title`
field by giving it the default value `NoTitle`.

## Quick reference

A typical parser will consist of `ParseValue a = Parse MetaValue a` and
`ParseObject a = Parse MetaObject a` parsers, with functions like `.!`, `field`,
and `object` linking the two together. Note that `MetaObject` is a newtype
wrapper over `Map String MetaValue`, since we think of `MetaMap` as being a YAML
object, its keys being fields.

```haskell
class FromValue a where
  parseValue :: ParseValue a
  -- Parse the input MetaValue with the given instance, returning a type a.
  parseListValue :: ParseValue [a]
  -- By default, expect a MetaList of values that parse as the type a with the parseValue instance.
  -- Used for the String, Inline, and Block instances. See the note on list instances below.

object p :: ParseValue a
-- Expect the input to be a MetaMap, parsing it with the given p :: MetaObject parser.

f .! p :: ParseObject a
-- Expect the field f of the input MetaObject to be set, parsing its value with the p :: ParseValue a parser

f .? p :: ParseObject (Maybe a)
-- Parse the field f with p :: ParseValue a if it is present, returning Just the result. Otherwise return Nothing.

field f :: ParseObject a
-- Expect the field f to be set, parsing its value with parseValue

f .?! a :: ParseObject a
-- Parse the value of the field f with parseValue if it is present. Otherwise return a.

symbolLike :: ParseValue String
-- Expect the input to be MetaString s (returning s), or MetaInlines [Str s] (returning s).
-- Useful when the input is a symbol standing for an option, like the no-title above, 
-- and you do not want to require the user to enclose the option in quotation marks.
-- Used in symbol and symbolFrom for that reason.
```

## Note on Char, String, and other list instances

In the instance for `Char` we parse exactly the input `MetaString [x]` as `x ::
Char` for `parseValue`, and we parse exactly the input `MetaString s` as `s ::
String` for `parseListValue`. Since the instance for `FromValue a => FromValue
[a]` uses `parseValue = parseListValue` this gives us a `String` instance that
succeeds exactly on a `MetaString` input. However, this decision does mean that
a `MetaList` of entries of the form `MetaString [x]` will _not_ parse as a
`String`. This is most likely what you want, but there is a `weakString` parser
given that also succeeds in this case.

Similar remarks apply to the `Inline` and `Block` instances.
