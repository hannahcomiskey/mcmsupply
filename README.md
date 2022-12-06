# mcmsupply

## Overview 
The 'mcmsupply' R package provides country-specific annual estimates for the proportions of modern contraceptive methods that are supplied by the public and private sectors for 30 countries participating in the Family Planing 2030 initative.
'mcmsupply' contains all the code required to reproduce the results discussed in the paper "Estimating the Proportion of Modern Contraceptives Supplied by the Public and Private Sectors using a Bayesian Hierarchical Penalized Spline Model", by Hannah Comiskey, Dr. Leontine Alkema & Dr. Niamh Cahill.


## Abstract
Family Planning programs and initiatives typically use nationally representative surveys to estimate key indicators of a country’s family planning progress. 
However, in recent years, routinely collected family planning services data (Service Statistics) have been used as a supplementary data source to bridge gaps in the surveys. 
The use of service statistics comes with the caveat that adjustments need to be made for missing private sector contributions to the contraceptive method supply chain. 
Evaluating the supply source of modern contraceptives often relies on Demographic Health Surveys (DHS), where many countries do not have recent data beyond 2015/16. 
Fortunately, in the absence of recent surveys we can rely on statistical model-based estimates and projections to fill the knowledge gap. 
We propose an approach for providing annual, country-specific estimates of a set of related contraceptive supply-share outcomes (proportion of modern contraceptive methods supplied by the public and private sectors), that captures changes in the supply share over time, as well as correlations between rates of change across different methods. The approach is implemented via a Bayesian, hierarchical, penalized-spline model with multivariate-normal spline coefficients to capture the cross-method correlations and is evaluated on 30 countries.

## How to Install and Run mcmsupply in R
devtools::install_github(" hannahcomiskey/mcmsupply")

## Getting started
If you are just getting started with mcmsupply, we recommend starting with the tutorial vignette found on our package webpage: 
https://rstudioserver.hamilton.ie/s/807ccd57f55a496e8de95/files/PhD/private_adjustment/mcmsupply/docs/index.html
