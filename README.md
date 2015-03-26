I haven't worked on this project in a while.  I'm going to get back
into it.  Here's a braindump of the planned flexibility of slogger,
along with a plan of attack that initially discards all that scope
creep, and then lets it creep back in in stages ;)

Interface options
=================

* Optional TH

* Non tree structured logging atop MonadLogger instances

* Tree structured logging based on thread id atop MonadLoggerIO instances

* Tree structured logging based on MonadSlogger

  - Required for having dump files

  - Required for merging distributed logs - need a way to store which
    instance it came from (which will also specify the dump file)

Serialization format options
============================

I'd like to have all of the following options:

* JSON log content

* JSON dump, alongside textual logs

* Binary dump, alongside textual logs

The inline JSON datatype would include the constructor for referring
to the dump and the "inline" constructor.

Development plan
================

Milestone 1: Basic structured logging
-------------------------------------

Plain logging function which outputs inline JSON

(no special monad class, no TH, no polyvariadic stuff, probably not
very convenient)

Milestone 2: Log viewer with GHCI
---------------------------------

Log viewer which integrates with GHCI to automatically bring the
structured data into scope

Milestone 3: Tree structure
---------------------------

Add support for tree structure to the library and viewer.  This means
adding a 'MonadSlogger' class and SloggerT transformer.

Milestone 4: Polyvariadic TH
----------------------------

Add support for polyvariadic TH log statements

Milestone 6: Dump files
-----------------------

Add support for dumping data rather than inlining it

Milestone 7: Distributed logs
-----------------------------

Add support to the library for tagging logs with the instance they
came from.  Name dump files after this instance.  This way, logging
that merges together logs from multiple servers can still be viewed.

Support multiple dump files per instance?  (so that files don't get
too huge + can just delete old ones)

Milestone 8: Misc hacks
-----------------------

Use some global state to support structured logging without having a
special monad.  Helpful for quick printf-ey logging

Even allow non-IO functions to log.  E.g. you could log in a view
pattern.
