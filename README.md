slogger is a fast structured logger.

Design:

* Each log message is suffixed with [slog=(1,2,3)] where the tuple is
  (nodeID,parentID,byteOffset)

* The binary serialized data is [(LogSpan, (TypeRep, ByteString))].
  In other words, it's a list of annotated spans.

* Log functions are variadic, concatenating some arguments into the
  log message, using others as semantic tags.


Consider: Should this be unified with the GHC pretty print modifications?

TODO: Current format doesn't look too good - consider revision

Misc things to look at:

  * Use precise clock
    https://code.google.com/p/greg/source/browse/client/haskell/System/Log/PreciseClock.hs

  * http://hackage.haskell.org/package/hslogstash

  * http://hackage.haskell.org/package/raven-haskell

  * http://hackage.haskell.org/package/rotating-log

  * http://hackage.haskell.org/package/binary-typed

Selector design
===============

In order for hierarchical logs to be useful, a query language is
needed.  Here are some desired features:

* Substring match on logs.  "This is the current syntax"

* Regex match on logs.  /Maybe this syntax/

* Match on tags:  .tagIdSyntax

* Match on source levels:  .LevelWarn

* Run haskell predicates on the data in matched logs.  [predicate arg1]

* Various selectors borrowed from CSS
