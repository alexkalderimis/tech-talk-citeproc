---
author: Alex Kalderimis
date: January 2016
title: XML Parsing
description: A guided tour of XML parsing in refme-citeproc
...

# What have the Monads ever done for us?

--------------------------------------------------------

> Apart from the roads, the sanitation, the legal system, the logging,
> the error handling, the http servers, the databases, the file-system,
> the parsers, the serializers, ...

# Why Was This Necessary?

## Code must be correct, clean and _comprehensible_

* Maintainability
* Adding features
* Error reporting
* Grokkability

## Things Haskell is not so great at

Or what Java gets right:

* Any Java developer can generally work on any code-base
* Java has a mature, standardised and well-tested tool-set
* It is clear what to use: all standard needs have mature solutions,
  some in the JDK (Collections, XML),
  some in standard libaries (log4j, Guava, Jackson)
* Core language features are optimised for performance.

<div class="notes">
Java achieves this *deliberately*. There is a massive emphasis on keeping
the language small, coherent and tractable.

The quality of engineering in the Java world is usually really high.

re. performance, consider Strings, ArrayList, AtomicInteger, ...
</div>

## Whereas in Haskell-land:

* Libraries frequently overlap and are not-interoperable
* It can be difficult to ascertain which solutions are standard;
  there have been a _lot_ of fads.
* Developers will often re-implement functionality.
* The language design is layered - modern Haskell can be intimidating
* Core language features are motivated my theoretical purity

<div class="notes">
Overlapping libraries: (Set vs HashSet, Seq vs Vector vs List, String vs Text vs ByteString, ...)

Language extensions can be horribly abused; at the very least they mean you can
easily run into syntax you have never seen before.

Type level extensions are particularly mind-bending, and very difficult to
understand if you don't immediately see the motivation.
</div>

# In our case: XML parsing

## How it was done

* re-implementation of Text.XML.HXT.Arrow.Pickle.Xml (Andrew Kennedy's pickle
  combinators: <http://research.microsoft.com/~akenn/fun/picklercombinators.pdf>
* no error reporting mechanism
* Wasted the whole point of the system (isomorphism)
* Does not (*cannot*) implement `Monad` or `Functor`

## And it looked like this:

```haskell
instance Picklable Formatting where
    getPickler = xpWrap ( \(((p,s,ff),(fs,fv,fw)),(td,va,tc,d),(q,sp))
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

<div class="notes">
* Things to note: not bijective - fields are ignored in serialisation
* If the parser fails, it will throw an error
* in order to parse more than 3 things at a time, combinators are needed to
  group them into pairs, triples and quads
</div>

# Why does that matter?

## A Skippable section on Type-classes

This section just makes sure everyone is up-to-speed with what Type Classes are
in general, what monads are in particular, and why we use them.

## Type Classes are very much like interfaces

They allow use to write extensible code - i.e. they are a solution to the
[Expression Problem](https://en.wikipedia.org/wiki/Expression_problem):

```haskell
specific :: [Int] -> [Int]
specific = map (+ 1)

containerAgnostic :: Functor f => f Int -> f Int
containerAgnostic = fmap (+ 1)

generalised :: (Functor f, Enum e) => f e -> f e
generalised = fmap succ
```

Anyone can come by later and add data-types that will work with these
definitions.

## Some classes are more important than others

The analogue to the Java collection hierarchy or Ruby's Enumerable module is the
type-class hierarchy:

```
Functor < Applicative < Monad
```

## There are others

such as standard one:

* `Monoid`
* `Semigroup`
* `Foldable`
* `Alternative`
* `Cofunctor`
* `Arrow`

## And more the specialised (but incredibly useful)

* `IsString`
* `Num`
* `Default`
* `Enum`
* `Ord`
* `Show`
* `Read`

[Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)

## What is a Monad?

Only *much* more general than either. The following things are monads:

* collections
* nullability (Maybe)
* Error states (Either)
* functions
* asynchronous results (futures)
* http requests
* http servers
* ...

------------------------------------------------------------

* logging
* dependency-injection
* databases
* random test set generators
* test specs
* IO (the Real World)
* parsers
* ...

## Showerthoughts

It should be noted this is not people forcing the world to fit their view; the
most mind-blowing thing about Monads is that these things are Monads whether we
want them to be or not: all we have done is *discover* their monadic nature.

We have monads in other code, we just don't call them that.

------------------------------------------------

A Functor lets you apply transformations.

```haskell
fmap transmogrify things
```

------------------------------------------------

An Applicative lets you treat things that are in a context as if they are not.
(also, inherently parallel).

```haskell
combine a b
```

------------------------------------------------

```haskell
pure combine <*> ma <*> mb
```

------------------------------------------------

```haskell
combine <$> ma <*> mb
```

------------------------------------------------

A Monad lets you focus on the data, not the structure (also inherently
sequential)

```haskell
let join mma = do
    ma <- mma
    a <- ma
    return a
```

------------------------------------------------

Now we can use `join` for loads of things, including some we might not have
originally considered

```
join ["abc", "def"]    `shouldBe` "abcdef"
join (Just (Just 'x')) `shouldBe` Just 'x'
join (Just Nothing)    `shouldBe` Nothing
join (*) 10            `shouldBe` 100
```

## Monadic code is easier to deal with

You don't have to learn a dozen specific APIs - just learn how to use `do`
notation, and apply it everywhere.

## An example: QuickCheck generators:

Say we want to test a function that told me if a triangle was right-angled:

```haskell
data Triangle = Triangle Double Double Double

isRightAngled (Triangle x y z) = (x ^ 2) + (y ^ 2) == (z ^ 2)
```

------------------------------------------------

We want to generate a lot of triangles to test it against. We could write this
ourselves, but there is a better way: QuickCheck!

```haskell
positiveDoubles :: Gen Double
positiveDoubles = arbitrary `suchThat` (> 0)

equilateralTriangles :: Gen Triangle
equilateralTriangles = do
    x <- positiveDoubles
    return (Triangle x x x)

rightAngledTriangles :: Gen Triangle
rightAngledTriangles = do
    x <- positiveDoubles
    y <- positiveDoubles
    let y = sqrt $ (x ^ 2) + (y ^ 2)
    return (Triangle x y z)
```

------------------------------------------------

And we can test that our function works:

```haskell
triangleSpec :: Spec
triangleSpec = describe "Triangle" $ do
    it "knows what right-angled triangles are" $
        forAll rightAngledTriangles isRightAngled
    it "knows that equilateral triangles are not right angled" $
        forAll equilateralTriangles (not . isRightAngled)
```

## This is the way things are done

Most useful things implement standard type classes from Typeclassopedia; code to
manipulate them can then be written in a standard way, using all the tools
that work elsewhere.

## A note about functions and methods

`(<>)` vs `(++)`, `map` vs `fmap`

# How does that apply to parsers?

## Monadic parser combinators are widely used

Monadic parsing reads almost like a definition of the problem

----------------------------------------------------

Consider (a subset of) BNF notation for JSON: <http://rfc7159.net/rfc7159>

```
JSON-text       = ws value ws
begin-array     = ws %x5B ws  ; [ left square bracket
begin-object    = ws %x7B ws  ; { left curly bracket
end-array       = ws %x5D ws  ; ] right square bracket
end-object      = ws %x7D ws  ; } right curly bracket
name-separator  = ws %x3A ws  ; : colon
value-separator = ws %x2C ws  ; , comma
ws = *(%x20 / %x09 / %x0A / %x0D) ; Space, tab, LF, CR
value = false / null / true / object / array / number / string
false = %x66.61.6c.73.65   ; false
null  = %x6e.75.6c.6c      ; null
true  = %x74.72.75.65      ; true
object = begin-object [ member *( value-separator member ) ] end-object
member = string name-separator value
...
```

----------------------------------------------------

Using Attoparsec, it would look like this:

```haskell
jsonText = do
    skipSpace
    v <- value
    skipSpace
    return v

jsonText = ws *> value <* ws
```

----------------------------------------------------

```haskell
jsonText       = ws *> value <* ws
beginArray     = ws *> char '[' <* ws
beginObject    = ws *> char '{' <* ws
endArray       = ws *> char ']' <* ws
endObject      = ws *> char '}' <* ws
nameSeparator  = ws *> char ':' <* ws
valueSeparator = ws *> char ',' <* ws
ws             = skip (`elem` [' ', '\n', '\t', '\r'])
value          = choice [false, null, true, object, array, number, jsonString]
false          = string "false"
null           = string "null"
true           = string "true"
object         = beginObject *> (member `sepBy` valueSeparator) <* endObject
member = do key <- jsonString
            skip nameSeparator 
            value <- value
            return (key, value)
...
```

<div class="notes">
Note that this maps almost 1-1 with the formal definition
</div>

-------------------------------------------------------

Which means our parser can look like this:

```haskell
generalAttributes :: AttrParser Formatting
generalAttributes = do
  f <- textAppearance
  dv <- displayAttr
  nc <- noCaseAttr
  nd <- noDecorAttr
  ff <- fontFamilyAttr
  return $! f & F.display .~ dv
              & F.noCase .~ nc
              & F.noDecor .~ nd
              & F.fontFamily .~ ff
```

--------------------------------------------------------

where:

```haskell
textAppearance :: AttrParser Formatting
textAppearance = do
  fs <- fontStyleAttr
  fv <- fontVariantAttr
  fw <- fontWeightAttr
  td <- textDecorationAttr
  va <- verticalAlignAttr
  return $! def & F.fontStyle .~ fs
                & F.fontVariant .~ fv
                & F.fontWeight .~ fw
                & F.textDecoration .~ td
                & F.verticalAlign .~ va
```

<div class="notes">
Side notes about strictness and `(&)`.
</div>

--------------------------------------------------------

where:

```haskell
fontStyleAttr :: AttrParser F.FontStyle
fontStyleAttr = enumAttribute "font-style"
```

--------------------------------------------------------

where:

```haskell
enumAttribute :: (Enum a, Bounded a, Show a) => XML.Name -> AttrParser a
enumAttribute name = maybe (return defaultValue) (readValue . unpack) (attr name)
  where defaultValue = minBound
        validValues = map (snakeCase . show) [defaultValue ..]
        readValue "normal" = return defaultValue
        readValue v = either err return (enumFromName v)
        err         = let n = unpack (XML.nameLocalName name)
                          msg = v ++ " is not a valid " ++ n
                                ++ ". It must be one of: " ++ show validValues
                      in raise msg
```
