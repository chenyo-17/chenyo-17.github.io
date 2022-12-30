---
title: "Search techniques"
date: 2022-12-30 22:00:00 +0200
categories: lecture
tags: synthesis
math: true
---

### Explicit enumeration/search

The idea of explicit enumeration is to consider different **explicit/concrete** programs constructed from the given DSL until a desired program is found.
There are 2 categories: **top down** or **bottom up**.
In a bottom up enumeration, the idea is to first discover low-level components and then **assemble** them together to a larger function and program.
In a top down enumeration, the idea is to first discover the high-level structure/**sketches** of the program and them enumerate low-level fragments.
In both case, a program (partial) AST is constructed after each search.

### Symbolic search

In explicit search, the synthesizer maintains *one or more* partially contructed programs.
In symbolic search, the synthesizer maintians a symbolic reperesentation of the space of **all** programs that are considered valid.
E.g., to search for an integer `n` satisfying the constraint `4 * n = 28`, an enumerative search would (randomly) try values until `7` is considered.
In symbolic search, we may *deduce* that `n = 28/4 = 7`.

However, algebraic manipulation can be complicated and sometimes an enumatation combined with smart search strategies (e.g., binary search, random sample) may be more efficient.

### How to define program space

One key design decisions before applying search techniques is the program space.
Defining the space in DSL has the following advantages:

- easy to enumerate all programs in a DSL
    
- the *type system* provided by the lanauges can help prune illegal programs

- a good choice of language can make the desired program popped up early

Another way of defining program space is called **parametric representation**, where the valid program is encoded *parametrically* (these programs are called *generative models*).
This representation is more helpful when **probability** of different programs are known.

A good program space representation should **not** have many **symmetries** (i.e., there are many different ways of represent the same program) so that the search can have more efficiency (but it depends on the search techniques in use).

