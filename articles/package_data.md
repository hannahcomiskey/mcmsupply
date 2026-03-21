# Package data

This vignette covers package data. Central to all vignettes are data
inputs in the form of, country classifications, estimated correlations,
estimated national and subnational model parameters for one-country
runs, and national and subnational family planning source data.

1.  [Family planning source data](#cu) `national_FPsource_data` and
    `subnat_FPsource_data`
2.  [Country and area classification](#cu)
    `Country_and_area_classification_inclFP2020`
3.  [Country names](#cu) `country_names`
4.  [Estimated national correlations](#cu)
    `national_estimated_correlations_logitnormal`
5.  [Estimated subnational correlations](#cu)
    `subnational_estimated_correlations`
6.  [Estimated model parameters for national one-country runs](#cu)
    `national_theta_rms_hat_logitnormal`,
    `national_tau_alpha_cms_hat_logitnormal` , and
    `national_sigma_delta_hat_logitnormal`,
7.  [Estimated model parameters for subnational one-country runs](#cu)
    `subnational_alpha_cms_hat`, `subnational_tau_alpha_pms_hat`, and
    `subnational_inv.sigma_delta_hat`.

## Load your library

``` r
library(mcmsupply)
```

## 

## Family planning source data

These are are two family planning commodity source datasets provided in
this package - one for the national level observations,
`national_FPsource_data` and one for the subnational level data
`subnat_FPsource_data`. For the national level data, there is a vignette
`calculate_FPsource_national_data_from_DHSmicrodata` in the
`inst/data-raw` folder that explains how the national level data was
calculated using the DHS micro-data. A similar approach was used for the
subnational data using IPUMS data.

``` r
head(national_FPsource_data)
```

    ## # A tibble: 6 × 21
    ## # Rowwise: 
    ##   Country     Region       Method average_year Commercial_medical   Other Public
    ##   <chr>       <chr>        <chr>         <dbl>              <dbl>   <dbl>  <dbl>
    ## 1 Afghanistan Southern As… Femal…        2016.           0.256    3.54e-2  0.709
    ## 2 Afghanistan Southern As… IUD           2016.           0.385    1.25e-2  0.602
    ## 3 Afghanistan Southern As… Injec…        2016.           0.364    1.05e-2  0.626
    ## 4 Afghanistan Southern As… OC Pi…        2016.           0.600    3.70e-2  0.363
    ## 5 Albania     Southern Eu… Femal…        2008.           0.000307 3.07e-4  0.999
    ## 6 Albania     Southern Eu… Femal…        2018.           0.0446   2.34e-2  0.932
    ## # ℹ 14 more variables: Commercial_medical.SE <dbl>, Other.SE <dbl>,
    ## #   Public.SE <dbl>, Other_n <dbl>, Public_n <dbl>, Commercial_medical_n <dbl>,
    ## #   check_sum <dbl>, count_SE.NA <dbl>, DEFT <dbl>, row_id <int>,
    ## #   index_country <int>, index_region <int>, index_method <int>,
    ## #   index_year <int>

``` r
head(subnat_FPsource_data)
```

    ##       Country     Region               Method average_year  sector_categories
    ## 1 Afghanistan Badakhshan Female Sterilization       2015.5 Commercial_medical
    ## 2 Afghanistan Badakhshan Female Sterilization       2015.5              Other
    ## 3 Afghanistan Badakhshan Female Sterilization       2015.5             Public
    ## 4 Afghanistan Badakhshan                  IUD       2015.5 Commercial_medical
    ## 5 Afghanistan Badakhshan                  IUD       2015.5              Other
    ## 6 Afghanistan Badakhshan                  IUD       2015.5             Public
    ##     proportion SE.proportion  n
    ## 1 5.757770e-11             0 NA
    ## 2 5.757770e-11             0 NA
    ## 3 1.000000e+00             0  5
    ## 4 5.181116e-11             0 NA
    ## 5 5.181116e-11             0 NA
    ## 6 1.000000e+00             0  3

## 

## Country and area classification

Country and area classification data is used as the a link between
low-level divisions (country) and higher-level divisions (sub-regions,
regions). After loading the package, enter
`Country and area classification` into the console to access this data.

``` r
Country_and_area_classification
```

    ## # A tibble: 231 × 8
    ##    `Country or area`   `ISO Code` `Major area`         Region `Developed region`
    ##    <chr>                    <dbl> <chr>                <chr>  <chr>             
    ##  1 Afghanistan                  4 Asia                 South… No                
    ##  2 Albania                      8 Europe               South… Yes               
    ##  3 Algeria                     12 Africa               North… No                
    ##  4 American Samoa              16 Oceania              Polyn… No                
    ##  5 Andorra                     20 Europe               South… Yes               
    ##  6 Angola                      24 Africa               Middl… No                
    ##  7 Anguilla                   660 Latin America and t… Carib… No                
    ##  8 Antigua and Barbuda         28 Latin America and t… Carib… No                
    ##  9 Argentina                   32 Latin America and t… South… No                
    ## 10 Armenia                     51 Asia                 Weste… No                
    ## # ℹ 221 more rows
    ## # ℹ 3 more variables: `Least developed country` <chr>,
    ## #   `Sub-Saharan Africa` <chr>, FP2020 <chr>

``` r
??Country_and_area_classification
```

## 

## Country names

Country names is to inform users of what countries are available at the
national and subnational administrative division in the preloaded data
of the mcmsupply package. After loading the package, enter
`country_names` into the console to access this data.

``` r
country_names
```

    ## # A tibble: 30 × 3
    ##    `Country names`              National level data ava…¹ Subnational level da…²
    ##    <chr>                        <chr>                     <chr>                 
    ##  1 Afghanistan                  Yes                       Yes                   
    ##  2 Benin                        Yes                       Yes                   
    ##  3 Burkina Faso                 Yes                       Yes                   
    ##  4 Cameroon                     Yes                       Yes                   
    ##  5 Congo                        Yes                       No                    
    ##  6 Democratic Republic of Congo Yes                       Yes                   
    ##  7 Cote d’Ivoire                Yes                       Yes                   
    ##  8 Ethiopia                     Yes                       Yes                   
    ##  9 Ghana                        Yes                       Yes                   
    ## 10 Guinea                       Yes                       Yes                   
    ## # ℹ 20 more rows
    ## # ℹ abbreviated names: ¹​`National level data available`,
    ## #   ²​`Subnational level data available`

``` r
??country_names
```

## 

## Estimated national correlations

This is the estimated correlations for the rates of change between
methods in the global national model. The approach for estimating
correlations at the national level is very similar to that at the
subnational level. For an example of how to calculate the subnational
correlations, please review the
`inst/data-raw/estimated_global_subnational_correlations.R` script.

``` r
mcmsupply::national_estimated_correlations_bivarlogitnormal
```

    ## # A tibble: 10 × 4
    ##    row         column               public_cor private_cor
    ##    <chr>       <chr>                     <dbl>       <dbl>
    ##  1 Implants    Female Sterilization        0           0  
    ##  2 Injectables Female Sterilization        0           0.1
    ##  3 IUD         Female Sterilization        0           0  
    ##  4 OC Pills    Female Sterilization        0           0.1
    ##  5 Injectables Implants                    0           0  
    ##  6 IUD         Implants                    0           0  
    ##  7 OC Pills    Implants                    0           0  
    ##  8 IUD         Injectables                 0           0  
    ##  9 OC Pills    Injectables                 0.2         0.1
    ## 10 OC Pills    IUD                         0           0

## 

## Estimated subnational correlations

This is the estimated correlations for the rates of change between
methods in the global national model. There is a vignette to describe
how we calculated these correlations at the subnational level, please
review the `inst/data-raw/estimated_global_subnational_correlations.R`
script.

``` r
subnational_estimated_correlations
```

    ## # A tibble: 10 × 4
    ##    row         column               public_cor private_cor
    ##    <chr>       <chr>                     <dbl>       <dbl>
    ##  1 Implants    Female Sterilization       -0.1         0.2
    ##  2 Injectables Female Sterilization        0.1         0.3
    ##  3 IUD         Female Sterilization        0.2        -0.1
    ##  4 OC Pills    Female Sterilization        0           0.5
    ##  5 Injectables Implants                    0.1         0.1
    ##  6 IUD         Implants                    0           0.1
    ##  7 OC Pills    Implants                    0           0.1
    ##  8 IUD         Injectables                 0.3         0  
    ##  9 OC Pills    Injectables                 0.3         0.6
    ## 10 OC Pills    IUD                         0           0

## 

## Estimated model parameters for national one-country runs

These are the estimated parameters used in a one-country national model
run. `national_theta_rms_hat_logitnormal` are the regional intercepts
used to inform the country-specific intercept of the model, the
`national_tau_alpha_cms_hat_logitnormal` are the associated variance
with these country-specific intercepts.
`national_sigma_delta_hat_logitnormal` is the variance-covariance matrix
used to inform the multivariate normal prior describing the first-order
differences of the spline coefficients ($\delta_{k}$).

``` r
national_theta_rms_hat_bivarlogitnorm
```

    ## , , 1
    ## 
    ##          [,1]     [,2]      [,3]      [,4]       [,5]
    ## [1,] 1.124690 1.879019 0.6123944 0.7029649 -0.3923574
    ## [2,] 4.925907 6.186027 3.3938891 6.1469855  2.1172546
    ## 
    ## , , 2
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 1.888573 2.519485 1.724739 1.702931 -0.1325017
    ## [2,] 5.123504 6.185247 3.381383 6.205612  2.1114136
    ## 
    ## , , 3
    ## 
    ##          [,1]      [,2]     [,3]      [,4]       [,5]
    ## [1,] 1.123264 0.9757107 1.403772 0.7431322 -0.6460643
    ## [2,] 5.081845 6.1901683 3.300275 6.1561532  2.0779721
    ## 
    ## , , 4
    ## 
    ##          [,1]     [,2]      [,3]      [,4]       [,5]
    ## [1,] 1.266304 1.579957 0.9993598 0.8297971 -0.5330631
    ## [2,] 5.102983 6.167419 3.3558101 6.1656484  2.0286927
    ## 
    ## , , 5
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.375822 2.057794 1.426649 1.157994 0.2966173
    ## [2,] 5.125126 6.187705 3.211676 6.203302 2.0952665
    ## 
    ## , , 6
    ## 
    ##          [,1]     [,2]     [,3]      [,4]      [,5]
    ## [1,] 1.103074 1.788250 1.904318 0.4001253 0.1891328
    ## [2,] 5.068090 6.166153 3.337933 6.1742769 2.1654082

``` r
national_tau_alpha_cms_hat_bivarlogitnorm
```

    ## [1] 1.5156756 0.4571958

``` r
national_inv_sigma_delta_hat_bivarlogitnorm
```

    ## , , 1
    ## 
    ##          [,1]     [,2]       [,3]     [,4]       [,5]
    ## [1,] 9.823609 0.000000  0.0000000 0.000000  0.0000000
    ## [2,] 0.000000 3.093259  0.0000000 0.000000  0.0000000
    ## [3,] 0.000000 0.000000  5.0010706 0.000000 -0.9655567
    ## [4,] 0.000000 0.000000  0.0000000 7.818821  0.0000000
    ## [5,] 0.000000 0.000000 -0.9655567 0.000000  4.7434306
    ## 
    ## , , 2
    ## 
    ##             [,1]     [,2]       [,3]     [,4]       [,5]
    ## [1,] 190.8352767    0.000 -0.9206719    0.000 -0.8301243
    ## [2,]   0.0000000 1338.708  0.0000000    0.000  0.0000000
    ## [3,]  -0.9206719    0.000  1.8037400    0.000 -0.1361239
    ## [4,]   0.0000000    0.000  0.0000000 1742.649  0.0000000
    ## [5,]  -0.8301243    0.000 -0.1361239    0.000  1.4333294

## 

## Estimated model parameters for subnational one-country runs

These are the estimated parameters used in a one-country subnational
model run. `subnational_alpha_cms_hat` are the country-specific
intercepts used to inform the subnational province-specific intercepts
of the model, the `subnational_tau_alpha_pms_hat` are the associated
variance with these province-specific intercepts.
`subnational_inv.sigma_delta_hat` is a precision of the
variance-covariance matrix used to inform the multi-variate normal prior
on first-order differences of the spline coefficients for the
one-country subnational model.

``` r
subnational_alpha_cms_hat
```

    ## , , Benin
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.490061 2.315202 1.468489 0.965467 -0.867741
    ## [2,] 3.709024 4.913419 3.559578 4.705832  1.417933
    ## 
    ## , , Burkina Faso
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 1.624638 4.401430 4.091642 1.856639 1.673468
    ## [2,] 3.627078 4.916308 3.542609 4.695024 1.332808
    ## 
    ## , , Cameroon
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 0.414227 1.851670 1.186021 1.618966 0.02014994
    ## [2,] 3.900294 4.974344 3.297583 4.689463 2.05336892
    ## 
    ## , , Congo Democratic Republic
    ## 
    ##          [,1]     [,2]      [,3]     [,4]      [,5]
    ## [1,] 2.492296 1.517491 0.5847138 1.595690 -1.365295
    ## [2,] 3.718868 4.924430 3.5579679 4.707582  3.153638
    ## 
    ## , , Cote d'Ivoire
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.593154 2.777196 1.708129 1.815302 -1.004267
    ## [2,] 3.634471 4.930963 3.507759 4.734519  1.576795
    ## 
    ## , , Ethiopia
    ## 
    ##          [,1]     [,2]     [,3]     [,4]         [,5]
    ## [1,] 1.747417 3.253701 1.232114 2.182444 -0.004640113
    ## [2,] 3.722831 5.066009 3.872909 4.693914  3.030750423
    ## 
    ## , , Ghana
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 2.024306 3.090449 2.375181 1.850325 -1.455411
    ## [2,] 3.723255 4.907766 3.500096 4.610203  3.382613
    ## 
    ## , , Guinea
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 1.569021 2.579824 1.529904 1.812646 -0.3854365
    ## [2,] 3.672309 4.972549 3.411387 4.726776  1.0253375
    ## 
    ## , , India
    ## 
    ##          [,1]     [,2]       [,3]      [,4]       [,5]
    ## [1,] 1.727972 2.319194 -0.6089675 0.9337197 -0.7976565
    ## [2,] 4.603336 4.930379  3.7498797 4.5598335  1.7521702
    ## 
    ## , , Kenya
    ## 
    ##          [,1]     [,2]      [,3]     [,4]       [,5]
    ## [1,] 1.380276 1.475382 0.6217132 1.003297 -0.1438469
    ## [2,] 4.009730 5.036432 4.1340056 4.739624  2.8696055
    ## 
    ## , , Liberia
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.584706 2.537414 1.092415 1.821980 0.7096074
    ## [2,] 3.664674 4.891488 3.318765 4.775019 1.9086049
    ## 
    ## , , Madagascar
    ## 
    ##          [,1]     [,2]     [,3]      [,4]      [,5]
    ## [1,] 1.452349 2.007100 1.926210 0.2240422 0.5095557
    ## [2,] 3.795371 5.001309 3.348336 4.7504325 1.4172713
    ## 
    ## , , Malawi
    ## 
    ##           [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 0.9599796 1.814213 1.873269 1.112080 1.331949
    ## [2,] 3.6146381 5.017234 2.575305 4.812123 2.198249
    ## 
    ## , , Mali
    ## 
    ##          [,1]     [,2]      [,3]     [,4]       [,5]
    ## [1,] 1.793507 2.378469 0.8042072 3.805264 -0.4974907
    ## [2,] 3.665678 4.949069 3.2119981 4.773084  1.8144717
    ## 
    ## , , Mozambique
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 1.835659 2.321865 3.195910 3.149991 1.197336
    ## [2,] 3.684628 4.949439 3.332752 4.652611 1.160038
    ## 
    ## , , Nepal
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.291945 2.086874 1.028583 1.066474 0.3402941
    ## [2,] 1.304060 4.968606 3.771652 4.698728 2.6481706
    ## 
    ## , , Niger
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.632563 3.088681 3.726582 1.548546 1.5793860
    ## [2,] 3.696786 5.006807 4.037084 4.761696 0.9871326
    ## 
    ## , , Pakistan
    ## 
    ##           [,1]     [,2]      [,3]     [,4]       [,5]
    ## [1,] 0.2688487 2.250390 0.6232042 1.526340 -0.3606401
    ## [2,] 4.0865275 4.985413 3.7905436 4.797376  1.7001215
    ## 
    ## , , Rwanda
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 2.193200 3.382272 3.251083 1.502788 2.272553
    ## [2,] 3.677578 4.903894 3.615488 4.691452 2.633087
    ## 
    ## , , Senegal
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 1.716900 4.204814 2.855192 2.388533 1.196907
    ## [2,] 3.668796 4.988311 3.516041 4.706682 2.730969
    ## 
    ## , , Tanzania
    ## 
    ##           [,1]     [,2]      [,3]     [,4]     [,5]
    ## [1,] 0.9650422 2.369480 1.4544593 1.442722 1.262963
    ## [2,] 3.1060103 4.697833 0.4194487 4.674467 1.199514
    ## 
    ## , , Uganda
    ## 
    ##          [,1]     [,2]      [,3]     [,4]       [,5]
    ## [1,] 1.769091 1.798986 0.5185503 1.264740 -0.7620542
    ## [2,] 3.831614 4.944204 4.0255467 4.697859  3.1368453
    ## 
    ## , , Zimbabwe
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.317705 1.758903 2.014544 1.552914 0.9099968
    ## [2,] 3.732717 4.927628 3.146861 4.686576 1.7962100

``` r
subnational_tau_alpha_pms_hat
```

    ## [1] 2.035571 1.507170

``` r
subnational_inv.sigma_delta_hat
```

    ## , , 1
    ## 
    ##            [,1]        [,2]        [,3]       [,4]       [,5]
    ## [1,]  6.1465996   -5.081301  -0.8678454  0.3733307  0.5105131
    ## [2,] -5.0813014 3095.437441 -19.9096569  8.0479535 12.1046314
    ## [3,] -0.8678454  -19.909657   2.7844919  0.0286086 -0.6903623
    ## [4,]  0.3733307    8.047954   0.0286086  4.5158420 -0.6462109
    ## [5,]  0.5105131   12.104631  -0.6903623 -0.6462109  2.8511850
    ## 
    ## , , 2
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 25527.97     0.00      0.0     0.00     0.00
    ## [2,]     0.00 49990.84      0.0     0.00     0.00
    ## [3,]     0.00     0.00 471914.4     0.00     0.00
    ## [4,]     0.00     0.00      0.0 20922.88     0.00
    ## [5,]     0.00     0.00      0.0     0.00 45692.51
