{-# OPTIONS_GHC -F -pgmF ./frquotes #-}
x1 = «some simple french quote»
x2 = «some simple french quote {with a hole} inside of it»
x3 = {- «not a french quote» and an «unclosed non french quote here too -} "a literal string"
x4 = "just «a literal» string" -- here yi is badely coloring it ;(
x5 = "another string with an \"es«cape" -- here for yi »
x6 = «some «nested» french quote»
x7 = «some «nested» french {hole} quote»
x8 = «some «nested with {hole}» french {hole} quote»
x8 = «some «nested with {hole «that reuse quotes»}» french {hole} quote»
x8 = «some «nested with {hole «that reuse quotes» and {braces} also}» french {hole} quote»
x9 = «{start} with {a} hole»
x10 = «stop {with} a {hole}»
