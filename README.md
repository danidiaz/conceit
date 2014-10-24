conceit
=======

A version of the Concurrently applicative from Simon Marlow's async package,
with the difference that the concurrent computation stops if any of the actions
returns a Left value, not only in the case of exceptions.

The internals have been copied wholesale from Concurrently, with modifications
to support the new behaviour.

Includes a useful Bifunctor instance.
