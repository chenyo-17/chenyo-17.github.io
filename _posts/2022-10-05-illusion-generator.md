---
title: "Illusion generation with Bayesian models"
date: 2022-10-05 13:00:00 +0200
categories: paper, machine learning
tags: bayesian inference, probabilistic model
math: true
---

This post is a note of [this paper](https://dl.acm.org/doi/10.1145/3528233.3530715).

### Why using probabilistic model to model human visions?

Define *scene* to be the real environment (e.g., a picture), and define *image* to be the content percepted by human.
Then multiple images can be mapped to the same scene because the image can be percepted from different light conditions, observation positions, or even different people.
Therefore, people uses probabilistic models to infer the scene from the given images.

### What is an adversary example

An *adversary example* refers to an illusion picture such as the famous dress (is it black/blue or yellow/white), or a face expression where from different angles people interpret different emotions of the face.

### What is Bayesian inference

The *Bayesian inference* computes the *posterior belief* of a scene given an image by combining the *prior belief* and the *observation* which encodes the distributions of different images given the scene.
For example, to compute the Bayesian model of *color constancy* (i.e., a class of illusions where people see different colors of the same scene), one can get the following 2 distributions:

1. prior = $$p$$(light, color): the prior beliefs over scenes, e.g., the daylight is more likely than green light;
2. observation = $$p$$(image \| light, color): how scenes are rendered into different images with any uncertainty (e.g., noise) during the process.

Then, the posterior belief $$p$$(light, color \| image) $$\propto$$ $$p$$(light, color) $$\cdot$$ $$p$$(image \| light, color) can be computed and one could reverse engineering the possible scenes given the image. 

It is said that Bayesian model is suitable to perform as a perceptual model.

### How to find adversary examples

On a high-level, given a proper Bayesian model, the goal is to find images such that the output posterior belief is *confidently incorrect*.
