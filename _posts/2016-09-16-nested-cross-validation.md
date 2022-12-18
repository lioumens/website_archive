---
layout: post
title: "Nested Cross Validation"
categories: [Machine Learning]
tags: [scikit-learn]
shortinfo: Nested Cross Validation and contribution to scikit-learn.
comments: true
---

This summer, I made my first contribution to a large open source repository called [scikit-learn](http://scikit-learn.org/stable/index.html). I contributed an [example](http://scikit-learn.org/stable/auto_examples/model_selection/plot_nested_cross_validation_iris.html) demonstrating the new `model_selection` module with nested cross validation. I was really happy with the end product that I contributed, and was gracious that the community of developers at scikit-learn was open and friendly toward new contributors. Here I walk you through my contribution, when nested cross-validation is necessary and why it generally a good practice, and some things I learned along the way.

## What is Cross Validation?

First to understand Nested cross validation (CV) we should first understand what non-nested CV does for us. In short, CV is essentially a procedure for splitting your data into a training set and testing set for your model that helps avoid overfitting and estimates the error rate of your fitted model. For an excellent short introductory video, Jeff Leek from Johns Hopkins Biostatistics department has one [here](https://www.youtube.com/watch?v=CmEqvD_ov2o).

## What is Nested Cross Validation?
Nested Cross Validation (CV) is quite simply a CV loop within each iteration of another CV loop. This is generally used when our learning model has hyperparameters that we do not know the optimal values for. For example, if we wish to fit a support vector classifier with a radial basis function (rbf) kernel, the classifier has two parameters to specify, `gamma` and `C`. ([A breakdown of how the parameters affect classification](http://scikit-learn.org/stable/auto_examples/svm/plot_rbf_parameters.html)). The outer CV loop thus splits the original dataset into a training set and *testing set*. In the inner CV loop, the training set from the outer loop is further split into another training set and a *validation set*. In this inner loop, different values are tried for the classifier parameters, and is scored using the validation set. The parameter values that give the best average scores on the validation set is then used on the outer *testing set*. This is good practice because without splitting the training set further, we would be using the same data to train the model as we do to validate the model, which ends up in overly optimistic generalization scores. Exactly how optimistic is dependent on the dataset, cross-validation strategy, and model. These topics are further explored by Cawley and Talbot in this [paper](http://jmlr.csail.mit.edu/papers/volume11/cawley10a/cawley10a.pdf).


## Nested Cross Validation Example

In my [example](http://scikit-learn.org/stable/auto_examples/model_selection/plot_nested_cross_validation_iris.html), I use a support vector classifier with rbf kernel on the classic [iris dataset](https://en.wikipedia.org/wiki/Iris_flower_data_set). The example uses K-fold validation for both the inner and outer loops, however this can be easily changed depending on your data. In order to simulate many trials, the seed for the random number generator is set to the iteration number, and a score is generated for both non-nested CV and nested CV. The first graph shows a comparison of scores for between the two types of CV, while the second graph shows the difference on each individual trial. As you can see, for this particular dataset and model choice, non-nested CV is only *marginally* more optimistic. However, this may not be the case for different datasets.

![](http://scikit-learn.org/stable/_images/sphx_glr_plot_nested_cross_validation_iris_001.png)

## Summary
I enjoyed my first experience contributing to [scikit-learn](http://scikit-learn.org/stable/index.html). If I have the time, and can find a proper sized issue I will definitely try to contribute again. Along the way, I learned lots about how the `model_selection` module is organized, how to apply general cross-validation concepts, the importance of continuous integration for large public projects, and how to write Sphinx documentation.
