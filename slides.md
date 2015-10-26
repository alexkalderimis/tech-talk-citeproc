---
author: Alex Kalderimis
date: October 2015
title: A Journey into the Centre of Citeproc
description: A Tech-Talk about the citeproc processor.
...

# In the beginning there was a citation...

## Description of a Citeproc bibliography

# Description of the Citation Processing Language

# Parsing a Bibliography

# Parsing the CSL

# The L in CSL: The evaluation model

----------------------

Remarks on layouts/macros and s-exprs/functions in lisp/lambda-calculus

# Descending into <code>citeproc</code>

## Sorting

## Disambiguation

## Processing

Points to note:

- the parsing of fields, including inline elements.
- the use (and consumption) of variables
- substitutions

# Rendering

----------------------

Note on the different intermediate formats, and the benefits of pandoc.

# Summary

----------------------

[FLOW DIAGRAM]

# Next steps

## Optimisations:

- Tree shaking (both of styles and of references)
- Text vs Strings (discussion of data types)
- Look at sorting and disambiguation (eliminating $$O(n)$$ structures)
- Memoisation (turn citeproc into a state)

## Tooling:

- IDE
- Static analysis
- REPL

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

## Example code:

```javascript
var foo = 123;
```
