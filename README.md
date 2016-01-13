Linear Type System
----

Implementation based on *Advanced Topics in Types and Programming Languages*

### Important properties

1. Linear variables are used exactly once along every control-flow path
2. Unrestricted data structures may not contain linear data structures.


### Problems

* Why some term is qualified while some not?
    + In this type system, boolean, pair construct and abstraction are qualified.
    + It seems like only "terminal" will be qualified. The `if then else` construct is recursive instead of terminal. Whether this construct is linear or unrestricted depends on its branch, but not itself.
* What does $\Gamma_{in} \vdash t : T; \Gamma_{out}$ means?
    + The effect is explicitly documented by *in* and *out*. <mark>FIXME</mark>: Although I feel that there is something more.
* How to understand the different between the axiomatic and algorithmic styles of typing rules?
* How to understand the `ctxdiff` function?





