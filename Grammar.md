> module Grammar where
>
> import Data.Text (Text)
> import qualified Data.Text as Strict
> import Data.Map (Map)
> import qualified Data.Map as Map
>

# Grammar.

## What do I understand as such?

A formal definition from a book would be like this:

$$ (Vn, Vr, R, S) $$

I cannot define a grammar before I have defined a sequence, so suppose that is had. I may now
propose that a grammar is defined by a collection of substitution rules from a sequence to
another. Would any such collection serve me well? And is it enough? That is, can the start symbol
and the list of terminals be derived?

* All the rules must be connected. That is, the left side of a rule must reside within the right
  hand side of some other, otherwise this left side is the start symbol. So, the same rule may
  start from any number of right hand sides of any number of rules, giving dissimilar results.
  This makes me question whether a grammar may be represented as a graph at all, or only a
  hypergraph of some sort _(even if that)_.

* There must be a way to locate some terminal collection. A question arises as to whether a
  terminal can further have a substitution performed upon, that is, whether there are rules that
  have only terminal symbols on the left. If not, then it is quite easy to find the terminal
  collection: those symbols that only inhabit the right side. Or rather, are never seen on the
  left side by themselves?

  This gives me some sort of a classification of grammars, from more well-behaving to more
  pathological.

  - The nicest case is when all the rules only feature a single symbol on the left, and some
    symbols never even occur on the left at all. Then:

    + Those symbols never seen on the left are the terminals.
    * Those symbols that never occur on the right are the start symbols.

    _(Can I prove these statements?)_

    + I can find out if there are cycles.

  - An insidious pathological case would be when two grammar rules together give a sequence that
    can be matched by the third rule, taking the ending of the first and the beginning of the
    second together. Such a grammar can lead to substitution sequences that do not admit tree
    representation. I may still handle this situation, taking advantage of that only contiguous
    sequences can be matched. But an ordinary AST does not even know of contiguity! What I am
    dealing with here is an order-tracking directed graph.

I want to run some experiments already.

* Generate random grammars.
* Detect loops, start and terminal symbols, contracting rules...
* Generate random sentences from a grammar, and actually draw the graph of substitutions.

> data Grammar = Grammar { meta :: (), rules :: Map Text Text }
>
> createGrammar :: Grammar
> createRule :: (Text, Text)

How can I generate a grammar? Above all, I must generate some rules.

