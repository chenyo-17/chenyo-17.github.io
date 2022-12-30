---
title: "Inductive synthesis"
date: 2022-12-30 12:00:00 +0200
categories: lecture
tags: synthesis
math: true
---

### Programming by Example (PBE) and Programming by Demonstration (PBD)

The goal of PBE is to synthesize a function given a set of inputs and outputs, while in PBD uers also provide a a **trace** of how the the output is computed.
PBD contains more information than PBE as a PBE problem can be highly *under-specified* (i.e., an enormous number of functions satisfy the input and output mappings).
However, in general there is more progress in PBE (e.g., machine learning) than in PBD.

There are 2 core **challenges** in PBE/PBD:

1. How to find a program that matches the examples/traces
2. How to know the program is correct

In traditional machine learning, the focus is on the *second* challenge as there are already systematic ways to pick program space (e.g., neural networks, SVM), but there is usually under-fitting or over-fitting issues.
In modern PBE, the focus is more on the *first* challenge: how to restrict **program space**.
PBE starts by carefully *control* the program space (e.g, *pruning* undesirable programs) and then **rank** the remaining reasonable programs.

In summary, PBE can reason about **arbitrarily large** program space without losing the benefit of list-of-program approaches.

### Inductive synthesis

The goal of inductive synthesis is to generate a function that matches a given set of **input/output** examples.
Inductive synthesis is one of the simplest interfaces for program synthesis.
