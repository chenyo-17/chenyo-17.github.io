---
title: "BDD applications"
date: 2022-10-28 10:00:00 +0200
categories: tool
tags: BDD synthesis
---

This post will collect applications of Binary Decision Diagram (BDD) in different fields.
I only read briefly, so it could not be correct.

- digital circuit mapping:
  
  [This paper](https://www.sciencedirect.com/science/article/pii/S0167926019301646) and [this paper](https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=7038601) use BDD to compactly represent a boolean function, then it traverses bottom-up (i.e. from True) to map nodes in each level into a circuit (e.g., each node stands for a 2-1 MUX). BDD nicely shows how much parallelism can be achieved (e.g., all MUX in the same level can run in parallel as there is no dependency among them). However, this BDD-based synthesis is not uncommon in EDA, this paper combines BDD with other design automation tools to improve the mapping.
  
