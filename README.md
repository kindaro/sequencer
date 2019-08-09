# sequencer

## Design decisions:

There are several dimensions of type variability:

* The logger. It could be either `WriterT`, `IO` or any action whatsoever.
* The output container. Does it make sense for it to be anything but a list? Well, kinda, yes.
* The exception type. Usually I will want it to be `SomeException`, but maybe it can be something
  else, what do I know..
* The monad is probably best left variable, but I may theoretically specialize it to IO.

It makes sense to create a most general form and some robustly useful one alongside?

On the other hand, it makes sense to provide any kind of an initial object. For instance, if it
has to be a free monoid, let it just be list.
