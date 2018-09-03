A collection of modules implementing various aspects of the framework for
presupposition based on graded monads from the dissertation 'Scope-taking and
presupposition satisfaction'; in particular, chapters 2 and 3. These are:

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
of indefinites. Terms with presuppositions now inhabit types `K e e a`. This
module provides an implementation of chapter 3 (the second part).

`P` and `D` are made instances of the Effect class of Orchard, Petricek, and
Mycroft for graded monads. `K` is made an instance of their PMonad class for
parameterized monads.