---
title: "Introduction to program synthesis"
date: 2022-12-30 15:00:00 +0200
categories: lecture
tags: synthesis
math: true
---

### Program synthesis, Compiler, logical programming and machine learning

- Common: program generation from high-level description of its behavior

- Synthesis: **search** the program space for the program that satisfies the stated requirements

    - Machine learning: a special case of synthesis where the specification comes from **data** and the space of functions is *tightly prescribed* (e.g., linear classifiers, decision trees)

- Compiler: apply **transform rules** according to *pre-defined* schedule on the input description

- Logical programming: express the requirments in a **logical form** and rely on **generic algorithm** to find an output satifying the logical constraints

### What is program synthesis

> Program synthesis correspond to a class of techniques that are able to generate a program from a collections of artifacts that establish semantic and syntactic requirements for the generated code.

The above definition emphasizes that program synthesis can provide **control** over **program space**, not just their intended behavior.
In fact, the biggest successes of synthesis have been in **specialized domains** where constrints have been baked in the synthesis system.
The system also has significant **flexibility** in how the space is defined.

### Today's program synthesis

- conferences
  
  - programming languages: PLDI, POPL, OOPSLA
  
  - formal methods: CAV, TACAS
  
  - machine learning: NeurIPS, ICLR, ICML
  
- research topics

    - bit-vector maniulations (fairly advanced)
    
    - data wrangling
    
    - specification inference
    
        - [verified lifting](https://people.eecs.berkeley.edu/~akcheung/papers/pldi16.pdf): discover a high-level representation that is provably equivalent to an implementation and can be used to generate a more efficient code version

    - reactive learning
    
### Challenges

- Intention: how do the users tell you their goals, what is the goal is *under-specified*

- Invention: how to discover the program that will satisfy the requirements 

- Adaptation: incremental synthesis vs clean-slate synthesis

