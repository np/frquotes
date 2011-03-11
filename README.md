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
              | '{' '{'                                 -- escaped '{'
              | '}' '}'                                 -- escaped '}'
              | '«' fr-elem* '»'


Examples
========

    «some simple french quote»
    «some simple french quote {with a hole} inside of it»
    {- «not a french quote» and an «unclosed non french quote here too -} "a literal string"
    "just «a literal» string"
    "another string with an \"es«cape"
    «some «nested» french quote»
    «some «nested» french {hole} quote»
    «some «nested with {hole}» french {hole} quote»
    «some «nested with {hole «that reuse quotes»}» french {hole} quote»
    «some «nested with {hole «that reuse quotes» and {braces} also}» french {hole} quote»
    «{start} with {a} hole»
    «stop {with} a {hole}»
    «bla {foo|bar|} baz»
