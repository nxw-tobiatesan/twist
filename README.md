# Twist

## What is Twist?

It is an embryo of a multi-agent draughts program, for starters, that
I initially began with the aim of learning Scala and experimenting
with AI.

Since in its current state it eschews all imaginable sorts of
optimizations and the architecture is trivially simple on purpose, it
also makes for a neat playground for experimenting with AI yourself.

The architecture is extensible and modular enough (see `Game.scala`)
that you can recycle it to implement your own two-player, perfect
information game with a minimum of effort; an example can be found in
`extras/StupidGame.scala`.

Conversely, you can easily add an engine by implementing an `AI[G:<
Game[G]` or even an evaluation function for the existing engines
(e.g. by extending `trait MinimaxEvaluation`) with ease.

The engines (currently found under `ai/`) currently consist of a
simple textbook implementation of Minimax with Alpha-Beta pruning and
MTCS.

Scala is especially handy in that it allows the programmer to mix pure
functional programming with structured programming, thus allowing for
copying and pasting algorithms found in literature verbatim.

**Nota bene: at the moment the gaming board comes with one
simplification wrt the [WDCF rules](http://www.wcdf.net/): a draw is
had after `n` moves, with `n` set to `100` by default.**


## How do I run it?

`sbt runMain "HumanVsMTCS"`

or

`sbt runMain "HumanVsMinimax"`

to play a game vs the MTCS resp. Minimax engine with the default
parameters, in which you play Max, i.e. White.

There is also a handful of _horribly_ quick&dirty benchmarks under
`benchmarks/`.

## FAQ
### Is it production ready?
No.
### Is it in the slightest way optimized for performance?
Not at all.
### Does it have a nice UI?
Not yet.
### Do you accept contributions?
A resounding yes.
### I want to extend it and incorporate it into my project, can I?
Yes, as long as you comply with the GPLv3.
### What is Minimax?
https://en.wikipedia.org/wiki/Minimax
### What is MTCS?
https://en.wikipedia.org/wiki/Monte_Carlo_tree_search

## Authors

* **Tobia Tesan** - [tobiatesan](https://github.com/tobiatesan)

## License

This project is licensed under the GPLv3 License - see the [LICENSE.md](LICENSE.md) file for details
