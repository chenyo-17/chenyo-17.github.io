---
title: "Decision tree synthesis for ML"
date: 2022-10-27 12:00:00 +0200
categories: paper, machine-learning, synthesis
tags: decision-tree 
---

- [paper link](https://sites.cs.ucsb.edu/~arpitgupta/pdfs/trustee.pdf)

#### Paper summary 

- This paper presents a framework / tool TRUSTEE that takes as input: 1. the ML model and 2. training dataset, and outputs: 1. a high-fidelity (i.e., correct) while practical (i.e., easy to be interpreted) decision tree (DT) and 2. associated trust report which summarizes whether the model is underspecified.

- TRUSTEE can be used to anlyaze if the ML model suffers from underspecification (i.e., a model verifier that checks whether the model fails to specify adequate detail, although it has good performance on training set) such as:
  1. shortcut learning (e.g., few features of input are used to compute accurate outputs), 
  2. spurious correlations (e.g., the model incorrectly assigns a high weight to some features, while removing these features and retrain the model do not harm the accuracy), and 
  3. vulnerable to out-of-distribution samples (i.e., training and test data set have different distributions).

        > Inductive bias: a logical formula mapping unseen inputs to outputs based on the inadequate training data (e.g., partial encoding)

- the DT should satisfy: 1. model-agnostic, 2. high-fidelity (i.e., has the same metrics (e.g., F1-score) as the input model), 3. low-complexity and 4. stable (i.e., final DT decisions are robust to small detials). Here the low-complexity means: 1. small DT and 2. the main branches accurately describe how the model makes most of its decisions.

- TRUSTEE uses a two-layer iteration to train a DT that satisfy the four requirements above. In the inner iteration, it uses imitation learning (e.g., teacher-student) to increase the fidelity of DT to the input model. In the outer interation, it picks the DT with the highest fidelity learnt from the inner iteration and improves its complexity and stability. Compared with related work on white-box extractor, TRUSTEE achieves better requirements.

    > imitation learning: a machine learning framework to learn from demonstration.
    
    > teacher-student learning: a transfer learning approach where a student model is trained to have the same predictive behavior as the teacher model.

    > CART: classification and regression tree, a transparent, predicative algorithm which trains a DT as the model.

- The DT learnt from CART tend to suffer from overfitting problems (e.g., large DT), therefore a postprocessing on DT is required. Compared with CCP pruning which could reduce the interpretability. TRUSTEE applies a Top-k pruning approach, which remains the top k branches from the root node to the decision node, and discards the other later branches. The idea behind this pruning is that if the DT's branches are ranked by the number of input samples a branch classifies, then the top k branches classify the most samples that share some similarity, while the later branches tend to reflect overfitting decisions.

    > CCP: cost-complexity pruning: a post-pruning technique that balance the fidelity and the tree size of DT.

- While end users can play with different k to explore more branches in DT, each time requires a recomputation of DT, which leads to undeterministic model explanation, its implication is a future work.

- The generated trust report spots attention-worthy aspects of the pruned DT that could indicate underspecification.

#### Questions

- Is overfitting another type of underspecification? What is the inductive bias in a overfitting model?

- When the high-fidelity DT is learnt from inner loop, are the branches already ranked? For later branches, do they tend to do overfitting classification (e.g., each input goes to a different level in the last level, while in the first level there are only 2 classes)?
