---
author: Alex Kalderimis
date: October 2015
title: A Journey into the Centre of Citeproc
description: A Tech-Talk about the citeproc processor.
...

# Down the Rabbit-Hole

---------------------------

```
POST /format/apa

{"references": [{...
```

----------------------------

```json
{
  "references":[{
    "refId":"0",
    "content": 
      "Weir, A. (2014) <i>The Martian</i>. United Kingdom: Del Rey"
  }],
  "meta":{},
  "citations":["(Weir, 2014)"]
}
```

# In the beginning there was a citation...

--------------------------

```JavaScript
{
    "references":[{
        "type":"book",
        "id":"0",
        "title":"The Martian",
        "author":[{"family":"Weir","given":"Andy"}],
        "issued":{"day":28,"month":8,"year":"2014"},
        "ISBN":"9780091956141",
        "publisher":"Del Rey",
        "publisher-place":"United Kingdom",
        "annote":"this is my note about this book"
    }],
    "citations":{
        "citationItems":[{"id":"0","locator":"100-200", "label": "page"}],
        "properties":{}
    }
}
```

## Luckily JSON is easy to parse...

----------------------------------

```haskell
data RequestData = FormattingRequest
    { references :: [Reference]
    , citations  :: CitationData
    } deriving (Show, Eq)
```

## Aeson $$\neq$$ JSON

---------------------------------------

```haskell
data IntermediateInput = II
    { refs               :: [Json.Value]
    , cites              :: [Json.Value]
    } deriving (Show, Eq)
```

## This we can parse

```haskell
instance Json.FromJSON IntermediateInput where
    parseJSON (Json.Object v) = II <$> rs <*> cs
        where rs = v .: "references"
              cs = v .: "citations" >>= (.: "citationItems")
    parseJSON _ = mzero
```

## And then transform

```haskell
refsFromAST :: [Json.Value] -> Either L.Text [Reference]
refsFromAST = first L.pack .
              parseJsonInputString .
              UC.toString .
              encode
```

## Haskell can feel like a RTL language

--------------

## Building larger functions from smaller ones.

```haskell
(f . g) x = f (g x)
```

So

```haskell
doThisThenDoThat = that . this
```

## Composition hides intermediate values

```haskell
f x = let iv = foo x
          iv' = bar iv
      in wibble iv'

g = wibble . bar . foo
```

This is a key element of "point-free" style.

## Breaking that transformation down:

```
first L.pack .         * Either String a -> Either Text a
parseJsonInputString . | String          -> Either String [Reference]
UC.toString .          | ByteString      -> String
encode                 ^ ToJSON a => a   -> ByteString
```

## Some obvious wins here:

```haskell
parseJsonInputString :: String -> Either String [Reference]
parseReferences      :: ByteString -> Either Text [Reference]

parseReferences = first pack . parseJsonInputString . toString
fromAST         = parseReferences . encode
```

## Or just use Aeson

```haskell
instance FromJSON Reference where
    parseJSON (Object v) = Reference <$> parseType v <*>
                                         parseID v   <*>
                                         ...
```

# Now we need a Style

------------------------

```
POST /format/[apa]
```

## A style is a bit like a template

```xml
<style>

  <layout>
    <text variable="author"/>
    <text variable="foo" suffix=", "/>
    <text variable="bar" suffix=". "/>
  </layout>

</style>
```

## With some abstraction

```xml
<style>

  <macro name="foo-bar">
    <text variable="foo" suffix=", "/>
    <text variable="bar" suffix=". "/>
  </macro>

  <layout>
    <text variable="author"/>
    <text macro="foo-bar"/>
  </layout>

</style>
```

## It's ~~Alive~~ a Language!

Macros & layouts $$\approx$$ functions & procedures

## It closely resembles the Lambda Calculus

## Parsing the CSL

```
[REDACTED]
```

## OK, you asked for it:

```haskell
xpWrap ( \(((p,s,ff),(fs,fv,fw)),(td,va,tc,d),(q,sp))
            -> Formatting p s ff fs fv fw td va tc d
            (if q then NativeQuote else NoQuote) sp False False
       , \(Formatting p s ff fs fv fw td va tc d _ sp _ _)
            -> (((p,s,ff),(fs,fv,fw)),(td,va,tc,d),(False,sp))) $
    xpTriple (xpPair (xpTriple (xpAttrText' "prefix"      )
                            (xpAttrText' "suffix"      )
                            (xpAttrText' "font-family" ))
                (xpTriple (xpAttrText' "font-style"  )
                            (xpAttrText' "font-variant")
                            (xpAttrText' "font-weight" )))
        (xp4Tuple (xpAttrText' "text-decoration")
                    (xpAttrText' "vertical-align" )
                    (xpAttrText' "text-case"      )
                    (xpAttrText' "display"        ))
        (xpPair   (xpAttrWithDefault False "quotes"        getPickler)
                    (xpAttrWithDefault False "strip-periods" getPickler))
```

## Parsing the CSL

```haskell
layout = do
    tagName "layout"
    body <- many (variable <|> macroCall <|> number <|> constant)
    return (Layout body)

variable = tagName "text" (requireAttr "variable") $ \var ->
    return (Variable var)

macroCall = tagName "text" (requireAttr "macro") $ \name ->
    return (MacroCall name)

...
```

## Parsing Operator Soup

Haskell makes frequent use of symbolic names for some functions. Some examples:

```haskell
f $ x = f x

class Monad m where
    return :: x -> m x
    (>>=)  :: m x -> (x -> m y) -> m y

(<$>) :: (x -> y) -> m x -> m y

(>>) :: m a -> m b -> m b

(<|>) :: m a -> m a -> m a
```

## `do` notation

Generalised de-nesting.

```haskell
foo this that = do
    x <- this
    y <- that
    return (x + y)

foo this that = this >>= $ \x -> that >>= $ \y -> return $ x + y
foo this that = this >>= ( \x -> that >>= ( \y -> return (x + y)))
```

## A more realistic example:

```haskell
-- | With the variable name and the variable value search for an
-- abbreviation or return an empty string.
getAbbreviation :: [Abbrev] -> String -> String -> String
getAbbreviation as varName varValue = fromMaybe "" $ do
    let k = if isNumericVar varName then "number" else varName
    abbrevSets <- lookup "default" as
    mapping <- lookup k abbrevSets
    M.lookup varValue mapping
```

# Entering `citeproc`

------------------------------------

```haskell
-- The result of rendering a bibliography into a style.
data RenderedBibliography = RenderedBibliography
      { citations    :: [FormattedItem]
      , bibliography :: [(Reference, FormattedItem)]
      } deriving ( Show )

-- The processing routine
citeproc :: Style -> [Reference] -> [[Cite]] -> RenderedBibliography
```

-------------------------------------

```haskell
citeproc :: ProcOpts -> Style -> [Reference] -> Citations -> RenderedBibliography
citeproc ops s rs cs = RenderedBibliography citsOutput (zip sorted biblioOutput)
    where
      citsOutput   = map (formatCitLayout s) . collapseCitGroups s $ citG
      biblioOutput = map formatOutputList $ disambiguate' $ procBiblio (bibOpts ops) s sorted
      sorted       = sortRefs s . map (getReference rs) . nubOn citeId . concat $ cs
      disambiguate' = if disambiguateYear `elem` getCitDisambOptions s
                        then map $ proc (updateYearSuffixes yearS) . map addYearSuffix
                        else id
      (yearS,citG) = disambCitations s sorted cs $ map (procGroup s) citsAndRefs
      citsAndRefs  = processCites sorted cs
```

<div class="notes">
Point-free style has its limits, and indeed can be horribly abused.
Haskell is an enormously dense language, so needs careful management of its
complexity.

Where clauses are great way of doing this. Unlike lets, which lead you down a
garden path not knowing how things will turn out (a story), wheres give you the
headline, and let you drill down as you need (news-report). This top-down
approach to writing code makes things much more readable, because by the time
you encounter a variable or term you understand why it's there and what it's
meant to do.
</div>

--------------------------------------------------------

```haskell
data RenderedBibliography = RenderedBibliography
      { citations    :: [FormattedItem]
      , bibliography :: [(Reference, FormattedItem)]
      } deriving ( Show )

RenderedBibliography citsOutput (zip sorted biblioOutput)
```

--------------------------------------------------------

```haskell
citsOutput   = map (formatCitLayout s) . collapseCitGroups s $ citG
```

<div class="notes">
Note the use of single character variable names. This both reminds us of
Haskell's academic and mathematical heritage, but it is also motivated by the
need to pass references around, and hence a need for brevity. Also it is enabled
by a system where types can be more informative than even good variable names.
</div>

----------------------------------------------------------

```haskell
(yearS,citG) = disambCitations s sorted cs $ map (procGroup s) citsAndRefs
```

<div class="notes">
Note here the use of destructuring to capture the elements of a data-structure
(here a tuple). This works for all data-structures you can construct, including
any you define yourself.
</div>

-----------------------------------------------------------

```haskell
sorted = sortRefs s . map (getReference rs) . nubOn citeId . concat $ cs

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy $ \x y -> f x == f y

nubBy :: (a -> a -> Bool) :: [a] -> [a]
```

<div class="notes">
The ability to easily abstract patterns of usage: would have been nice to have a
a nubOn in Data.List, but defining it is a one-liner.
The use of type-classes.
Pipelines as a pattern
bad things: O(n) getReference [shudder]
</div>

-------------------------------------------------------------

```haskell
disambiguate' = if disambiguateYear `elem` getCitDisambOptions s
                then map $ proc (updateYearSuffixes yearS) . map addYearSuffix
                else id
```

-------------------------------------------------------------

## Case study - suffixes

```haskell
suffixes :: [String]
suffixes = suffixes' [""]
    where suffixes' curr = let next = [x <> y | x <- curr, y <- alphabet]
                           in next ++ suffixes next
          alphabet = [[c] | c <- ['a' .. 'z']]
```

<div class="notes">
a text-bookish way to generate an infinite list of suffixes.

Comprehensions (just a specialised species of do-notation) are used to generate
lists.
</div>

------------------------------------------------------------

## The problem with Strings

Language   Bytes              UTF8
--------   ----------------  --------
Java       int[], short[]     String
Python2    str                unicode
Python3    bytes              str
JS         Buffer             String
Ruby       Array              String
Haskell    [Word8], [Int]     [Char]
-------    ---------------- ---------

------------------------------------------------------------

## Enter `Data.Text`

```haskell
import Data.Text

suffixes :: [Text]
suffixes = suffixes' [""]
    where suffixes' curr = let next = [x <> y | x <- curr, y <- alphabet]
                           in next ++ suffixes next
          alphabet = [singleton c | c <- ['a' .. 'z']]
```

<div class="notes">
Strings vs Text vs ByteStrings
</div>

------------------------------------------------------------

```haskell
suffixes :: [Text.Text]
suffixes = suffixes' [""]
    where suffixes' curr = let next = Text.append <$> curr <*> alphabet
                           in next ++ suffixes' next
          alphabet = map Text.singleton ['a' .. 'z']
```

------------------------------------------------------------

```haskell
suffixes :: [Text.Text]
suffixes = suffixes' [""]
    where suffixes' curr = let next = curr <<>> alphabet
                           in next ++ suffixes' next
          alphabet = map Text.singleton ['a' .. 'z']
          (<<>>) = liftA2 mappend
```

# Rendering

## In summary:

```

+--------+    +---------+    +---------------+    +--------+    +------+
| Data   |--->| Output  |--->| FormattedItem |--->| Inline |--->| Text |
+--------+    +---------+    +---------------+    +--------+    +------+

```

## $\rightarrow$ `Pandoc`

- Parse strings into sentences
- Taking quotes into account
- Perform formatting (italics, title-casing, etc.)
- Format punctuation according to the locale
- Collapse whitespace
- Avoid punctuation collisions
- Avoid emitting empty elements

## The benefits of Pandoc

> - A single, widely supported format
> - An existing ecosystem
> - Very simple to render

----------------------------------

```haskell
render :: Inline -> Text
render (Emph a) = tag "i" a
render (Strong a) = tag "b" a
render (Strikeout a) = tag "strike" a
render (Superscript a) = tag "sup" a
render (Subscript a) = tag "sub" a
render (SmallCaps a) = tag "smallcaps" a
render (Quoted SingleQuote a) = wrap "‘" "’" a
render (Quoted DoubleQuote a) = wrap "“" "”" a
render (Str a) = L.pack . escape $ a
render Space = " "
render LineBreak = "<br/>"
render (Link a _) = toHTML a
render (Note blks) = L.unlines $ map renderBlock blks
```

------------------------------------

```haskell
wrap :: Text -> Text -> [Inline] -> Text
wrap a b x = mconcat [a, toHTML x, b]

tag :: Text -> [Inline] -> Text
tag a = wrap opening closing
    where coda    = a    <> ">"
          opening = "<"  <> coda
          closing = "</" <> coda
```

# Nearly done...

--------------------------------------

```haskell
formatRequest :: I.RequestData -> Style -> ActionM ()
formatRequest req s = do
    let l = I.localeName req `orElse` styleDefaultLocale s
        rs = I.references req
        cs = map return $ I.items . I.citations $ req
    loc <- getLocale l
    json $ processBib s loc rs cs
```

--------------------------------------

```haskell
data FormatResponse = FormatResponse { references :: ![FormattedReference]
                                     , citations  :: ![FormattedCitation]
                                     , meta       :: !ResponseMeta
                                     } deriving (Show, Eq)

data FormattedReference = FormattedReference { refId :: !Text
                                             , content :: !Text
                                             } deriving (Show, Eq) 
```

--------------------------------------

```haskell
instance ToJSON FormatResponse where
    toJSON (FormatResponse {..}) = object ["references" .= references
                                          ,"citations" .= citations
                                          ,"meta" .= meta
                                          ]

instance ToJSON FormattedReference where
    toJSON (FormattedReference {..}) = object ["refId" .= refId
                                              ,"content" .= content
                                              ]
```

# Next steps

## Optimisations:

- Tree shaking (both of styles and of references)
- Text vs Strings
- Look at sorting and disambiguation (eliminating $O(n)$ structures)

## Tooling:

- IDE
- Static analysis
- REPL
- CSLUnit

<div class="notes">
We take this for granted - why not them!

Testing on the macro level
Automatic refactoring
Instant feedback
</div>

## Sugar

```ruby
style do |csl|
  csl.name = 'University of Somewhere'
  csl.authors = ['Joe Bloggs', 'Jane Bloggs']
  italics = {font_variant: 'italic'}

  csl.macro 'fancy-author' do |m|
    m.variable 'author', formatting: italics
  end

  csl.bibliography do |b|
    b.layout delimiter: ' ' do |l|
      s.fancy_author()
      s.variable 'publisher', prefix: '(', suffix: ')'
    end
  end
end
```

