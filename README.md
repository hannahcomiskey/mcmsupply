# mcmsupply

## Overview 
The 'mcmsupply' R package provides country-specific annual estimates for the proportions of modern contraceptive methods that are supplied by the public and private sectors for 30 countries participating in the [Family Planing 2030 initative](https://fp2030.org).

> 'mcmsupply' contains all the code required to reproduce the results discussed in the paper "Estimating the Proportion of Modern Contraceptives Supplied by the Public and Private Sectors using a Bayesian Hierarchical Penalized Spline Model", by Hannah Comiskey, Dr. Leontine Alkema & Dr. Niamh Cahill.


## Installation

The package can be installed by cloning this repository:
git clone https://github.com/hannahcomiskey/mcmsupply.git

Then running: 
devtools::install(). 

The source code for vignettes can be found in /vignettes/paper_vignettes. Below is a brief introduction.

## Background
Quantifying the public/private sector supply of contraceptive methods within countries is vital for effective and sustainable family planning (FP) delivery. In many low and middle-income countries (LMIC), measuring the contraceptive supply source often relies on Demographic Health Surveys (DHS). However, many of these countries carry out the DHS approximately every 3-5 years and do not have recent data beyond 2015/16. Our objective in estimating the set of related contraceptive supply-share outcomes (proportion of modern contraceptive methods supplied by the public/private sectors) is to take advantage of latent attributes present in dataset to produce annual, country-specific estimates and projections with uncertainty. We propose a Bayesian, hierarchical, penalized-spline model with multivariate-normal spline coefficients to capture cross-method correlations. Our approach offers an intuitive way to share information across countries and sub-continents, model the changes in the contraceptive supply share over time, account for survey observational errors and produce probabilistic estimates and projections that are informed by past changes in the contraceptive supply share as well as correlations between rates of change across different methods. These results will provide valuable information for evaluating FP program effectiveness. To the best of our knowledge, it is the first model of its kind to estimate these quantities. 

Keywords: Bayesian, family planning, splines, correlation, hierarchical, time-series

## Citation and link to the national contraceptive method supply share model paper 

Comiskey, Hannah, Alkema, Leontine, and Cahill, Niamh. "Estimating the proportion of modern contraceptives supplied by the public and private sectors using a Bayesian hierarchical penalized spline model." arXiv preprint arXiv:2212.03844 (2022).

https://arxiv.org/abs/2212.03844

