# [heidi][]

[![Build Status](https://travis-ci.com/ocramz/heidi.png?branch=master)](https://travis-ci.com/ocramz/heidi?branch=master)


[heidi]: https://github.com/ocramz/heidi


![alt text](https://github.com/ocramz/heidi/raw/master/img/heidi.jpg "Heidi")

`heidi` : tidy data in Haskell

This library aims to bridge the gap between Haskell's precise but inflexible type discipline and the dynamic world of dataframes.

If this sounds interesting, read on!


## Introduction

A "dataframe" is conceptually a table of data that can be manipulated with a computer program; it potentially contains numbers, text and anything else that can be rendered as a string of text.

In scientific practice, a "tidy" dataframe is a specific way of arranging the data in which each row represents a distinct observation ("data point") and each column a "feature" (i.e. some observable aspect) of the data. 

Nowadays, data science is a very established practice and many software libraries offer excellent functionality for working with such dataframes. `R` has `tidyverse` , Python has `pandas`, and so on.

What about Haskell?

The `Frames` [1] library offers rigorous type safety and good runtime performance, at the cost of some setup overhead. `Heidi`'s  main design goal instead is to have minimal overhead and possibly very low cognitive load to data science practitioners, at the cost of some type safety.

## TL;DR

```
{-# language DeriveGenerics, DeriveAnyClass #-}
module MyDataScienceTask where

import Heidi

data Sales = Row String Int deriving (Eq, Show, Generic, Heidi)
```

and off you go.

## Rationale


Out of the box, Haskell offers record types, e.g.

```
data Row a = MkRow { column1 :: Int, column2 :: String } deriving (Eq, Show)
```

which is handy because in one declaration you get a constructor method `MkRow` and accessors `column1`, `column2`, so a simple "data table" could be constructed as a list of such records, simply enough.

One thing that the language doesn't natively support is lookup by accessor name. For example `column1 :: Row -> Int` can only access a value of type `Row`, since the `column1` name is globally unique (for a discussion on modern techniques to deal with this, see the Advanced section below).

In addition to lookup, many data tasks require relational operations across pairs of data tables; algorithmically, these require lookups both across rows and columns, and there's nothing in Haskell's implementation of records that supports this.

There are a number of additional tasks that are routine in data analysis such as plotting, rendering the dataset to various tabular formats (CSV, database ...), and this library aims to support those too with a convenient syntax.


## Advanced


Haskell offers a number of advanced workarounds for manipulating types, such as generic traversals, lookups, etc. A brief list of keywords is given in the following, for those inclined to dive into the rabbit hole.

### Row polymorphism

Elm, Purescript etc.

### OverloadedRecordFields

[1]

### Row types

As you might know, the "row types" problem is well understood and has been explored in practice; discussing the various tradeoffs between approaches would be lengthy and quite technical (and your humble author is not too qualified to do full justice to the topic either).

In Haskell , the Frames [2] library and related ecosystem stands out as a full-featured dataframe implementation that does not compromise on type safety. 

Heidi instead offers generic transformations from the source datatypes to uni-typed values (conceptually, each row is a `Map String T` where `data T = TInt Int | TChar Char` etc.), a domain in which it's convenient to perform lookups and similar operations.

Exploring further : vinyl [3], heterogeneous lists, sums-of-products ...




## References

[1] OverloadedRecordFields : https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#record-field-selector-polymorphism

[2] Frames : https://hackage.haskell.org/package/Frames

[3] vinyl : https://hackage.haskell.org/package/vinyl 

[4] generics-sop : https://hackage.haskell.org/package/generics-sop
