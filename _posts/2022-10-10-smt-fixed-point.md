---
title: "Understanding Z3 fixed-point result"
date: 2022-10-10 10:00:00 +0200
categories: smt 
tags: fixed-point z3
math: true
---

When learning the usage of fixed-point in Z3Py form [this tutorial](https://ericpony.github.io/z3py-tutorial/fixpoint-examples.htm), I was not clear about the relations between using fixed-point engine such as Datalog and using quantifiers, then I found [this StackOverflow post](https://stackoverflow.com/questions/39403644/%E2%88%83-queries-and-%E2%88%80-queries-with-z3-fixedpoint-engine) which also asks the similar question. 

It turns out that using Datalog engine is a Z3 extension, but the same logic can also be realized by normal SMT-LIB syntax with quantifiers.
The Datalog rules are just implicaitons with quantifiers in normal syntax, and the query in Datalog extension can be translated as the assertion of query in normal syntax.

For instance, in that post, `query fail` asks if `fail` can be derived (`fail` stands for an existence predicate), if the answer output by the Datalog engine is `sat`, one will also get `unsat` in the normal SMT-LIB constraints with the assertion `assert (not query)`. 

The rules and corresponding assertions involving `fail` are `rule (=> (and (f n m) (= m 0)) fail)` and `assert (forall ((n Int) (m Int)) (=> (and (f n m) (= m 0))))`.
If one first looks at the assertion version, to guarantee `not fail` holds, it requires the expression `f(n, m) && m == 0` is **always false** for every $$m$$ and $$n$$, otherwise the entire implication becomes false.
If `m != 0`, the expression is false trivially, if `m == 0`, one next considers `n == 0` and `n >= 0`, if `n < 0`, we look at another assertion `forall n, n < 0 => f(n, 0)`, these 2 assertions cannot hold together, hence the entire thing is unsat in the normal SMT-LIB syntax.

Since it is `unsat` for the normal SMT formulation, it indicates `fail` is an invariant/fact, hence the Datalog engine produces `sat`.
Which indicates the predicates: the original function accepts negative integers.
Note the original function is not `f`, here `f` is a relation, which returns true if the 2 arguments of `f` input and output exists.
