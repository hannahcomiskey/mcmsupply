# mcmsupply

`mcmsupply` is a R package that enables users to estimate the public and private sector contributions to the modern contraceptive method market supplies over time in low- and middle-income countries. `mcmsupply` uses Bayesian hierarchical models, combined with cross-method correlations to maximise the precision of model estimates, even in data-sparse settings. This R package has functionality for users to estimate an individual country's market supply shares or run more complex multi-country models. The data and JAGS models are available at the national and subnational administration levels. Lastly, there is functionality available for users to supply custom datasets if they wish. 

## Zenodo link

[![DOI](https://zenodo.org/badge/473641889.svg)](https://zenodo.org/badge/latestdoi/473641889)

## Installation

The package can be installed by cloning this repository `git clone https://github.com/hannahcomiskey/mcmsupply.git` and then running  `devtools::install()`

or using the R command `install.packages("mcmsupply")`


## Motivation

Quantifying the public/private sector supply of contraceptive methods within countries is vital for effective and sustainable family planning (FP) delivery. In many low and middle-income countries (LMIC), measuring the contraceptive supply source often relies on Demographic Health Surveys (DHS). However, many of these countries carry out the DHS approximately every 3-5 years and do not have recent data beyond 2015/16. Our objective in estimating the set of related contraceptive supply-share outcomes (proportion of modern contraceptive methods supplied by the public/private sectors) is to take advantage of latent attributes present in dataset to produce annual, country-specific estimates and projections with uncertainty. We propose a Bayesian, hierarchical, penalized-spline model with multivariate-normal spline coefficients to capture cross-method correlations. Our approach offers an intuitive way to share information across countries and sub-continents, model the changes in the contraceptive supply share over time, account for survey observational errors and produce probabilistic estimates and projections that are informed by past changes in the contraceptive supply share as well as correlations between rates of change across different methods. These results will provide valuable information for evaluating FP program effectiveness. To the best of our knowledge, it is the first model of its kind to estimate these quantities. 

Keywords: Bayesian, family planning, splines, correlation, hierarchical, time-series

## Citation and link to the national contraceptive method supply share model paper 

Comiskey, Hannah, Alkema, Leontine, and Cahill, Niamh. "Estimating the proportion of modern contraceptives supplied by the public and private sectors using a Bayesian hierarchical penalized spline model." arXiv preprint arXiv:2212.03844 (2022).

https://arxiv.org/abs/2212.03844

## mcmsupply R package website
 
  <a href="http://hannahcomiskey.github.io/mcmsupply/"><img src="https://raw.githubusercontent.com/hannahcomiskey/mcmsupply/master/badge/mcmsupply_badge.png" height="300" align="centre" /></a>
  

## R-CMD-Check status

 <!-- badges: start -->
  [![R-CMD-check](https://github.com/hannahcomiskey/mcmsupply/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hannahcomiskey/mcmsupply/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
 
