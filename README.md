# mcmsupply

## Overview 
The 'mcmsupply' R package provides country-specific annual estimates for the proportions of modern contraceptive methods that are supplied by the public and private sectors for 30 countries participating in the [Family Planing 2030 initative](https://fp2030.org).

> 'mcmsupply' contains all the code required to reproduce the results discussed in the paper "Estimating the Proportion of Modern Contraceptives Supplied by the Public and Private Sectors using a Bayesian Hierarchical Penalized Spline Model", by Hannah Comiskey, Dr. Leontine Alkema & Dr. Niamh Cahill.

Installation

The package can be installed by cloning and using devtools::install(). The source code for vignettes can be found in /vignettes. Below is a brief introduction.

## Background
Quantifying the public/private sector supply of contraceptive methods within countries is vital for effective and sustainable family planning (FP) delivery. In many low and middle-income countries (LMIC), measuring the contraceptive supply source often relies on Demographic Health Surveys (DHS). However, many of these countries carry out the DHS approximately every 3-5 years and do not have recent data beyond 2015/16. Our objective in estimating the set of related contraceptive supply-share outcomes (proportion of modern contraceptive methods supplied by the public/private sectors) is to take advantage of latent attributes present in dataset to produce annual, country-specific estimates and projections with uncertainty. We propose a Bayesian, hierarchical, penalized-spline model with multivariate-normal spline coefficients to capture cross-method correlations. Our approach offers an intuitive way to share information across countries and sub-continents, model the changes in the contraceptive supply share over time, account for survey observational errors and produce probabilistic estimates and projections that are informed by past changes in the contraceptive supply share as well as correlations between rates of change across different methods. These results will provide valuable information for evaluating FP program effectiveness. To the best of our knowledge, it is the first model of its kind to estimate these quantities. 

Keywords: Bayesian, family planning, splines, correlation, hierarchical, time-series

## Getting started
If you are just getting started with mcmsupply, we recommend starting with the tutorial vignette found on our package [webpage](https://rstudioserver.hamilton.ie/s/807ccd57f55a496e8de95/files/PhD/private_adjustment/mcmsupply/docs/index.html)

