---
title: "Resources for learning SMT"
date: 2022-10-04 10:00:00 +0200
categories: smt
tags: smt-resources
math: true
---

## Online resources

- [Decision procedures](http://www.decision-procedures.org/)

This book and associated slides teach how the decision procedure works in the SMT solver for different theories such as bit-vectors or linear real arthimetic, which is helpful for understanding the solver statistics.

- [Z3 online tutorial](https://microsoft.github.io/z3guide/docs/logic/basiccommands/)

This website gives an online tutorial of Z3. It seems to cover the same topics as the Python version [Programming Z3](https://theory.stanford.edu/~nikolaj/programmingz3.html#sec-transitive-closure), which I am currently looking at, but instead this website uses SMT-LIBv2 language.

- [Z3Py Guide](https://ericpony.github.io/z3py-tutorial/guide-examples.htm)

This website gives more examples on how to use z3 python API to do different things

- [Berkeley's formal methods](https://people.eecs.berkeley.edu/~sseshia/219c/)

This course teaches formal methods of verifications and synthesis. The [instructor](https://people.eecs.berkeley.edu/~sseshia/#talks) is famous.

[This lecture slides](https://people.eecs.berkeley.edu/~sseshia/219c/lectures/Interpolation.pdf) introduces what is propositional interpolation.

- [What is first-order logic](https://www.cs.jhu.edu/~phi/ai/slides/lecture-first-order-logic.pdf)

This slide from JHU gives intresting examples of first-order logic.

- Z3Py relared course: [1](https://www.cs.tau.ac.il/~msagiv/courses/software-productivity19.html), [2](https://www.cs.tau.ac.il/~msagiv/courses/asv/)

This course covers many topics related to Z3, such as bounded model checking. The [instructor](https://www.cs.tau.ac.il/~msagiv/#courses) also teaches other related courses.

- [This instructor](https://www.cs.tsukuba.ac.jp/~uhiro/) has research on functional program verification



## Papers

The following papers are introduced by Programming Z3.

- [Cube and Conquer](https://www.cs.utexas.edu/~marijn/publications/cube.pdf)

This paper introduces the algorithm of "cube and conquer", which is a technique used in Z3 to increase the scalability by partitioning the search space into sub-problems that can be solved in parallel.

- Get MSS and MUS: [1](https://www.ijcai.org/proceedings/2018/0188.pdf), [2](https://link.springer.com/content/pdf/10.1007/978-3-642-38171-3_11.pdf)

The above papers introduce algorithms to enumeraring all maximal satisfying subsets and minimal unsat cores together.

- [Bounded Model Checking](https://ebooks.iospress.nl/publication/4999)

Thid book chapter introduces bounded model checking, which is a technique to check reachability problems between two states. 

