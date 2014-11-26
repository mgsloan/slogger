NOTE: WIP
=========

This project isn't yet ready for primetime, and is currently quite a
mess.  I'm just uploading it to github for a bit to access it from
elsewhere / share it with others.

Slogger
=======

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

Misc TODO
=========

* sforkUnmasked

* Log masking state when it's something other than "Unmasked"?  (Control.Exception.getMaskingState)

* Consider having the option of logging interesting other thread-global stuff.  One possible API for this would look like:

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

* Extensions beyond CSS - what do we do about OR? is it nestable?

Selector usage
==============

Selectors can be used for a wide variety of purposes:

1) Filtering emitted log messages at runtime

2) Filtering emitted log messages at compiletime.  This will work by
the user declaring instances of some typeclass(es), where the argument
is a type level string of the selector.  The TH code will pick these
up by using reify, and then apply them to the known portion of the
log.

-- If there are no LogWhiteList instances, then "*" is assumed.

class LogWhiteList (x :: String)

class LogBlackList (x :: String)

A further interesting optimization might be to have the TH splices
output specialized code for pruning the currently active selectors.
In other words, it's possible to precompute if a given log line
definitely does or definitely does not match a component of a
selector.

3) Analyzing logs

I'd like the semantics of these to be identical no matter the phase.
In other words, if you pre-emptively know your analysis, you can stick
it in at a different phase than log analysis.

Selector meaning
================

There needs to be some additional syntax in order to differentiate
between the different effects of the selectors.

* Blacklist vs whitelist

* Whole path vs just selected element

This will need a bit more thought - will it be possible in the UI to
view children without their parents?
