slogger is a fast structured logger.

Design:

* Each log message is suffixed with [slog=(1,2,3)] where the tuple is
  (nodeID,parentID,byteOffset)

* The binary serialized data is [(LogSpan, (TypeRep, ByteString))].
  In other words, it's a list of annotated spans.

Consider: Should this be unified with the GHC pretty print modifications?

TODO: Current format doesn't look too good - consider revision

Misc things to look at:

  * Use precise clock
    https://code.google.com/p/greg/source/browse/client/haskell/System/Log/PreciseClock.hs

  * http://hackage.haskell.org/package/hslogstash

  * http://hackage.haskell.org/package/raven-haskell

  * http://hackage.haskell.org/package/rotating-log

  * http://hackage.haskell.org/package/binary-typed
