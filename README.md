conceit
=======

A version of the
[Concurrently](http://hackage.haskell.org/package/async-2.0.2/docs/Control-Concurrent-Async.html#t:Concurrently)
Applicative from Simon Marlow's
[async](http://hackage.haskell.org/package/async) package, with the difference
that the concurrent computation stops if any of the actions returns a Left
value, not only in the case of exceptions.

The internals have been copied wholesale from Concurrently, with modifications
to support the new behaviour.

Includes a useful Bifunctor instance.
