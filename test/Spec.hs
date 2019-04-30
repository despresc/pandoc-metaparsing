import           Control.Applicative
import           Data.Map              (Map, insert)
import           Test.Hspec
import           Text.Pandoc.Builder
import           Text.Pandoc.Definition

import           Text.Pandoc.MetaParse

resultExplode :: Result a -> IO a
resultExplode (Error e)   = error $ simpleErrorShow e
resultExplode (Success a) = pure a

-- Make sure that the example used in the README and the haddock introduction works.

data ContributorTitle = NoTitle | HasTitle Inlines
  deriving (Eq, Ord, Show)

data Contributor = Contributor
  { authorName     :: Inlines
  , authorLocation :: [Inline]
  , authorTitle    :: ContributorTitle
  } deriving (Eq, Ord, Show)

type Contributors = [Contributor]

instance FromValue ContributorTitle where
  parseValue = symbol "no-title" NoTitle <|> HasTitle <$> parseValue <?> "inline title"

instance FromValue Contributor where
  parseValue = object $ Contributor <$> field "name" <*> "location" .! parseInlines <*> "title" .?! NoTitle

main :: IO ()
main = hspec $ do
  describe "README example" $ do
    let nameS      = text "Sherlock Holmes"
        locationS  = text "221B Baker Street"
        nameW      = text "John Watson"
        locationW  = text "221B Baker Street"
        titleW     = text "Dr."
        expectS    = Contributor nameS (toList locationS) NoTitle
        expectW    = Contributor nameW (toList locationW) (HasTitle titleW)
        toSource x y z = insert "name" x . insert "location" y . maybe id (insert "title") z $ mempty
        sourceMeta = setMeta "contributors" [ toSource nameS locationS Nothing
                                            , toSource nameW locationW (Just titleW) ] nullMeta
        invalidTitle1 = MetaString "foo"
        invalidMeta = setMeta "contributors" [ toSource (toMetaValue nameS) (toMetaValue locationS) (Just invalidTitle1)] nullMeta
        expectedError = MetaWhenParseError "field contributors" . MetaWhenParseError "field title" . MetaExpectGotError (expectationFromList [ "no-title", "inline title"])
    it "parses valid contributors" $
      fromMetaField "contributors" sourceMeta `shouldBe` Success [expectS, expectW]
    it "throws correctly on one invalid title field" $
      (fromMetaField "contributors" invalidMeta :: Result Contributors) `shouldBe` Error (expectedError (Just "foo"))
