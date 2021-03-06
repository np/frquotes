Once installed you can use put this line at the top of your Haskell files:

    {-# OPTIONS_GHC -F -pgmF frquotes #-}


Syntax
======

There is a new form of literals delimited by `«` and `»`.

    expression ::= ...
                 | '«' fr-elem* '»'

    fr-elem ::= any non '«', '»', '{', '}' character    -- plain character
              | '{' expression '}'                      -- hole/antiquotation
              | '{' name '|' quasi-quote-character '|}' -- quasi-quotation
              | '{«}'                                   -- escaped '«'
              | '{»}'                                   -- escaped '»'
              | '«{»'                                   -- escaped '{'
              | '«}»'                                   -- escaped '}'

Default Quotations
==================

If no name is given to a quotation, the quotation defaultQQ is
assumed.

Examples
========

    «some simple french quote»
    «some simple french quote {with a hole} inside of it»
    {- «not a french quote» and an «unclosed non french quote here too -} "a literal string"
    "just «a literal» string"
    "another string with an \"es«cape"
    «some {«}nested{»} french quote»
    «some {«}nested{»} french {hole} quote»
    «some {«}nested with {hole}{»} french {hole} quote»
    «some {«}nested with {hole «that reuses quotes»}{»} french {hole} quote»
    «some {«}nested with {hole «that reuses quotes» and {braces} also}{»} french {hole} quote»
    «bla {foo|bar|} baz»
    «bla {|foo|} bar» ≈ «bla {defaultQQ|foo|} bar»
