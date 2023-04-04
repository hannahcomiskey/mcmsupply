mcmsupply
================

# Installation

### 1. Using termal GIT commands

This installation approach will give you access to the source code and data, downloading the folders and files as you see on this repo.
The package can be installed by cloning this repository using the terminal command: git clone
<https://github.com/hannahcomiskey/mcmsupply.git>

Then running the R command: devtools::install().

### 2. Using devtools
This approach does not let you see the files and folders as you see on github.

You can install the package using this R command:
devtools::install_github("hannahcomiskey/mcmsupply")


The source code for vignettes can be found in
/vignettes/paper_vignettes. Below is a brief introduction.

# Folders 

When you complete you installation, remember to create folders inside /visualisation and /results to store your subnational/national results. Keeping the results tidy and separate from one another will prevent any overwriting.

For example, I use the following pathways for national global and local runs: 
1. Results
      - Global: results/national/global
      - Local : results/national/local
2. Visualisations
      - Global: visualisation/national/global
      - Local : visualisation/national/local

# Zenodo link

[![DOI](https://zenodo.org/badge/473641889.svg)](https://zenodo.org/badge/latestdoi/473641889)

# Introduction

Quantifying the public/private sector supply of contraceptive methods
within countries is vital for effective and sustainable family planning
(FP) delivery. In many low and middle-income countries (LMIC), measuring
the contraceptive supply source often relies on Demographic Health
Surveys (DHS). However, many of these countries carry out the DHS
approximately every 3-5 years and do not have recent data beyond
2015/16. Our objective in estimating the set of related contraceptive
supply-share outcomes (proportion of modern contraceptive methods
supplied by the public/private sectors) is to take advantage of latent
attributes present in dataset to produce annual, country-specific
estimates and projections with uncertainty. We propose a Bayesian,
hierarchical, penalized-spline model with multivariate-normal spline
coefficients to capture cross-method correlations. Our approach offers
an intuitive way to share information across countries and
sub-continents, model the changes in the contraceptive supply share over
time, account for survey observational errors and produce probabilistic
estimates and projections that are informed by past changes in the
contraceptive supply share as well as correlations between rates of
change across different methods. These results will provide valuable
information for evaluating FP program effectiveness. To the best of our
knowledge, it is the first model of its kind to estimate these
quantities.

Keywords: Bayesian, family planning, splines, correlation, hierarchical,
time-series

# Citation and link to the national contraceptive method supply share model paper

Comiskey, Hannah, Alkema, Leontine, and Cahill, Niamh. “Estimating the
proportion of modern contraceptives supplied by the public and private
sectors using a Bayesian hierarchical penalized spline model.” arXiv
preprint arXiv:2212.03844 (2022).

<https://arxiv.org/abs/2212.03844>
