---
title: "Resources for learning SMT"
date: 2022-10-04 10:00:00 +0200
categories: smt
tags: smt-resources
math: true
---

## Markded/Unchecked resources for learning SMT

### Online resources

- [Decision procedures](http://www.decision-procedures.org/)

This book and associated slides teach how the decision procedure works in the SMT solver for different theories such as bit-vectors or linear real arthimetic, which is helpful for understanding the solver statistics.

- [Z3 online tutorial](https://microsoft.github.io/z3guide/docs/logic/basiccommands/)

This website gives an online tutorial of Z3. It seems to cover the same topics as the Python version [Programming Z3](https://theory.stanford.edu/~nikolaj/programmingz3.html#sec-transitive-closure), which I am currently looking at, but instead this website uses SMT-LIBv2 language.

- [Z3Py Guide](https://ericpony.github.io/z3py-tutorial/guide-examples.htm)

This website gives more examples on how to use z3 python API to do different things

### Papers

- [Cube and Conquer](https://www.cs.utexas.edu/~marijn/publications/cube.pdf)

This paper introduces the algorithm of "cube and conquer", which is a technique used in Z3 to increase the scalability by partitioning the search space into sub-problems that can be solved in parallel.
