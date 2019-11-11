## Introduction

A collection of modules implementing various aspects of the framework for
presupposition based on graded monads from the dissertation [Scope-taking and
presupposition satisfaction](https://semanticsarchive.net/Archive/TRmOTkzM/grove-scope-presupposition-dissertation.pdf);
in particular, chapters 2 and 3. These are:

P.hs: implements a static framework for presupposition, in which all
presuppositions project. Terms with presuppositions inhabit types `P e a`, where
`e` is an effect encoding the types of any anaphoric dependencies, and `a` is
the type of the value. This module provides an implementation of chapter 2.

D.hs: implements a dynamic framework for presupposition, in which
presuppositions may be filtered and cancelled. Terms with presuppositions
inhabit types `D e a`. This module provides an implementation of chapter 3 (the
first part).

K.hs: continuizes `D` to provide a framework with presupposition filtering and
cancellation, but with added functionality to analyze the semantic contribution
of indefinites. Terms with presuppositions now inhabit types `K e f a`. This
module provides an implementation of chapter 3 (the second part).

Q.hs: continuizes `K` to provide a framework with presupposition filtering and
cancellation, functionality to analyze the semantic contribution of indefinites,
and functionality to analyze the semantic contribution of other quantifiers that
lack exceptionally scoping behavior (e.g., *every*). Terms with presuppositions
now inhabit types `Q h e f a`. The overall strategy is borrowed from
[Charlow 2014](https://semanticsarchive.net/Archive/2JmMWRjY/charlow-semantics-exceptional-scope-diss.pdf),
which introduces the notion of an "underlying monad" of a continuation monad to
differentiate between the two kinds of quantifiers.

`P` and `D` are made instances of the *Effect* class of Orchard, Petricek, and
Mycroft for graded monads. `K` and `Q` are made instances of their *PMonad* class
for parameterized monads.

## Examples

Note the dependencies effect monad
([https://github.com/dorchard/effect-monad](https://github.com/dorchard/effect-monad))
and type-level-sets
([https://github.com/dorchard/type-level-sets](https://github.com/dorchard/type-level-sets)).
To run the following, load up Q.hs.

### Part 1

First sentence: "An octopus ate a cannoli."

  `sentence1 = monadicLower $ liftOpK ba (fa an (dyn octopus)) (liftOpK fa (upK (dyn ate)) (fa a (dyn cannoli)))`

Second sentence: "The mollusk was thirsty."

  `sentence2 = liftOp ba (fa the (dyn mollusk)) (upD (dyn (was thirsty)))`

Make 'the mollusk' anaphoric to 'an octopus':

  `sentence2resolved = anaph SZero (\g -> g !! 0) sentence2`

Third sentence: "The dessert was stale."

  `sentence3 = liftOp ba (fa the (dyn dessert)) (upD ((dyn (was stale))))`

Make 'the dessert' anaphoric to 'a cannoli':

  `sentence3resolved = anaph SZero (\g -> g !! 1) sentence3`

Combine the first two sentences into a discourse:

  `discourse1 = sentence1 >@ sentence2resolved`

Combine the discourse and the third sentence into a new discourse:

  `discourse2 = discourse1 >@ sentence3resolved`

Now, check each piece for truth, falsity, or failure:

  `checkForTruth sentence1`

  `checkForTruth discourse1`

  `checkForTruth discourse2`

### Part 2

First sentence: "Every octopus ate a cannoli."

  `sentence4 = monadicLower $ lowerQ $ liftOpQ ba (fa every (dyn octopus)) (liftOpQ fa (upQ (dyn ate)) (liftK (fa a (dyn cannoli))))`

Second sentence: "An octopus ate every cannoli."

  `sentence5 = monadicLower $ lowerQ $ liftOpQ ba (liftK (fa an (dyn octopus))) (liftOpQ fa (upQ (dyn ate)) (fa every (dyn cannoli)))`

Third sentence: "Every octopus is a mollusk."

  `sentence6 = monadicLower $ lowerQ $ liftOpQ ba (fa every (dyn octopus)) (upQ (dyn mollusk))`

Now, check each sentence for truth, falsity, or failure:

  `checkForTruth sentence4`
  
  `checkForTruth sentence5`

  `checkForTruth sentence6`