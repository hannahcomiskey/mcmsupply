Package data
================

This vignette covers package data. Central to all vignettes are data
inputs in the form of, country classifications, estimated correlations,
estimated national and subnational model parameters for one-country
runs, and national and subnational family planning source data.

1.  [Country and area classification](#div)
    `Country_and_area_classification_inclFP2020`
2.  [Estimated national correlations](#cu)
    `estimated_national_correlations`
3.  [Estimated subnational correlations](#cu)
    `estimated_global_subnational_correlations` and
    `estimated_global_spatial_subnational_correlations`
4.  [Estimated model parameters for national one-country runs](#pc)
    `median_alpha_region_intercepts`,
    `precision_alpha_country_intercepts` , and
    `Bspline_sigma_matrix_median`,
5.  [Estimated model parameters for subnational one-country runs](#pc)
    `median_alphacms`, `tau_alpha_pms_hat`, `sigma_delta_hat`,
    `spatial_sigma_delta_hat`, `global_provincial_neighbouradj`, and the
    country-specific neighbourhood adjacency matrices in the
    `data/local_neighbours` folder
6.  [Family planning source data](#pc) `national_FPsource_data` and
    `subnat_FPsource_data`

## <a name="div"></a>

## 1. Country and area classification

Country and area classification data is used as the a link between
low-level divisions (country) and higher-level divisions (sub-regions,
regions). After loading the package, enter
`Country and area classification` into the console to access this data.

``` r
library(mcmsupply)
Country_and_area_classification_inclFP2020
```

    ## # A tibble: 231 × 8
    ##    `Country or area`   `ISO Code` `Major area`    Region `Developed \r\nregion` Least developed\r\nc…¹ Sub-Saharan \r\nAfri…²
    ##    <chr>                    <dbl> <chr>           <chr>  <chr>                  <chr>                  <chr>                 
    ##  1 Afghanistan                  4 Asia            South… No                     Yes                    No                    
    ##  2 Albania                      8 Europe          South… Yes                    No                     No                    
    ##  3 Algeria                     12 Africa          North… No                     No                     No                    
    ##  4 American Samoa              16 Oceania         Polyn… No                     No                     No                    
    ##  5 Andorra                     20 Europe          South… Yes                    No                     No                    
    ##  6 Angola                      24 Africa          Middl… No                     Yes                    Yes                   
    ##  7 Anguilla                   660 Latin America … Carib… No                     No                     No                    
    ##  8 Antigua and Barbuda         28 Latin America … Carib… No                     No                     No                    
    ##  9 Argentina                   32 Latin America … South… No                     No                     No                    
    ## 10 Armenia                     51 Asia            Weste… No                     No                     No                    
    ## # ℹ 221 more rows
    ## # ℹ abbreviated names: ¹​`Least developed\r\ncountry`, ²​`Sub-Saharan \r\nAfrica`
    ## # ℹ 1 more variable: FP2020 <chr>

``` r
??Country_and_area_classification_inclFP2020
```

## <a name="cu"></a>

## 2. Estimated national correlations

This is the estimated correlations for the rates of change between
methods in the global national model. The approach for estimating
correlations at the national level is very similar to that at the
subnational level. For an example of how to calculate the subnational
correlations, please review the
`data-raw/estimated_global_subnational_correlations.R` script.

``` r
estimated_national_correlations
```

    ## # A tibble: 10 × 4
    ##    row         column               public_cor private_cor
    ##    <chr>       <chr>                     <dbl>       <dbl>
    ##  1 Implants    Female Sterilization        0           0  
    ##  2 Injectables Female Sterilization        0.2         0.7
    ##  3 IUD         Female Sterilization        0.1         0.2
    ##  4 OC Pills    Female Sterilization        0           0.7
    ##  5 Injectables Implants                    0.1         0.1
    ##  6 IUD         Implants                    0.2         0  
    ##  7 OC Pills    Implants                    0           0.1
    ##  8 IUD         Injectables                 0.2         0.1
    ##  9 OC Pills    Injectables                 0.5         0.8
    ## 10 OC Pills    IUD                         0.1         0.1

## <a name="cu"></a>

## 3. Estimated subnational correlations

This is the estimated correlations for the rates of change between
methods in the global national model. There is a vignette to describe
how we calculated these correlations at the subnational level, please
review the `data-raw/estimated_global_subnational_correlations.R`
script. The apporach is the same for both the spatial and non-spatial
model estimates.

``` r
estimated_global_subnational_correlations
```

    ## # A tibble: 10 × 4
    ##    row         column               public_cor private_cor
    ##    <chr>       <chr>                     <dbl>       <dbl>
    ##  1 Implants    Female Sterilization        0.1           0
    ##  2 Injectables Female Sterilization        0.2           0
    ##  3 IUD         Female Sterilization        0             0
    ##  4 OC Pills    Female Sterilization        0.2           0
    ##  5 Injectables Implants                    0             0
    ##  6 IUD         Implants                    0             0
    ##  7 OC Pills    Implants                    0             0
    ##  8 IUD         Injectables                 0.1           0
    ##  9 OC Pills    Injectables                 0.3           0
    ## 10 OC Pills    IUD                         0.2           0

``` r
estimated_global_spatial_subnational_correlations
```

    ## # A tibble: 10 × 4
    ##    row         column               public_cor private_cor
    ##    <chr>       <chr>                     <dbl>       <dbl>
    ##  1 Implants    Female Sterilization        0             0
    ##  2 Injectables Female Sterilization        0.2           0
    ##  3 IUD         Female Sterilization        0.1           0
    ##  4 OC Pills    Female Sterilization        0.2           0
    ##  5 Injectables Implants                    0.1           0
    ##  6 IUD         Implants                    0             0
    ##  7 OC Pills    Implants                    0             0
    ##  8 IUD         Injectables                 0.1           0
    ##  9 OC Pills    Injectables                 0.3           0
    ## 10 OC Pills    IUD                         0.2           0

## <a name="cu"></a>

## 4. Estimated model parameters for national one-country runs

These are the estimated parameters used in a one-country national model
run. `median_alpha_region_intercepts` are the regional intercepts used
to inform the country-specific intercept of the model, the
`precision_alpha_country_intercepts` are the associated variance with
these country-specific intercepts. `Bspline_sigma_matrix_median` is the
variance-covariance matrix used to inform the Wishart prior on the
first-order difference of the spline coefficients.

``` r
median_alpha_region_intercepts
```

    ## , , 1
    ## 
    ##          [,1]     [,2]      [,3]      [,4]       [,5]
    ## [1,] 1.301550 1.833818 0.5270685 0.7537049 -0.5164741
    ## [2,] 3.690803 4.764637 3.1455633 4.5460172  1.7931031
    ## 
    ## , , 2
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 2.583934 2.683237 1.794515 1.790627 -0.1707711
    ## [2,] 3.748790 4.751290 3.117329 4.546589  1.7916651
    ## 
    ## , , 3
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 1.675565 1.697223 1.029494 1.005109 -0.5180739
    ## [2,] 3.730166 4.760698 3.077658 4.553346  1.6835570
    ## 
    ## , , 4
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.735901 2.048935 1.242921 1.054806 0.1537607
    ## [2,] 3.688687 4.736354 2.988089 4.587235 1.8155029
    ## 
    ## , , 5
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 1.517680 1.195353 1.517616 1.065101 -0.7044244
    ## [2,] 3.727607 4.780151 3.036655 4.557675  1.6806916

``` r
precision_alpha_country_intercepts
```

    ## [1] 1.0338134 0.5713761

``` r
Bspline_sigma_matrix_median
```

    ## , , 1
    ## 
    ##            [,1]       [,2]       [,3]       [,4]       [,5]
    ## [1,] 0.17569510 0.00000000 0.04788189 0.02535736 0.00000000
    ## [2,] 0.00000000 0.59857633 0.04403929 0.09373054 0.00000000
    ## [3,] 0.04788189 0.04403929 0.32499002 0.06931805 0.16419734
    ## [4,] 0.02535736 0.09373054 0.06931805 0.37017274 0.03482467
    ## [5,] 0.00000000 0.00000000 0.16419734 0.03482467 0.32956170
    ## 
    ## , , 2
    ## 
    ##            [,1]        [,2]       [,3]       [,4]        [,5]
    ## [1,] 0.96824736 0.000000000 0.63234459 0.03290151 0.650254947
    ## [2,] 0.00000000 0.002161332 0.00418859 0.00000000 0.004367744
    ## [3,] 0.63234459 0.004188590 0.85508582 0.01539978 0.693431845
    ## [4,] 0.03290151 0.000000000 0.01539978 0.02854580 0.015767092
    ## [5,] 0.65025495 0.004367744 0.69343184 0.01576709 0.903939091

## <a name="cu"></a>

## 5. Estimated model parameters for subnational one-country runs

These are the estimated parameters used in a one-country national model
run. `median_alphacms` are the country-specific intercepts used to
inform the subnational province-specific intercepts of the model, the
`tau_alpha_pms_hat` are the associated variance with these
province-specific intercepts. `sigma_delta_hat` and
`spatial_sigma_delta_hat` are the variance-covariance matrices used to
inform the Wishart prior on the first-order difference of the spline
coefficients for the non-spatial and spatial subnational models
respectively. The `global_provincial_neighbouradj` is the global-level
neighbourhood adjanency matrix for all subnational provinces in the
complete dataset. The country-specific neighbourhood adjacency matrices
are found in the `data/local_neighbours` folder. These contain the
neighbourhood ajancy matrices for each country individually.

``` r
median_alphacms
```

    ## , , Afghanistan
    ## 
    ##           [,1]     [,2]      [,3]      [,4]        [,5]
    ## [1,] 0.9375425 2.279007 0.9614377 0.7875669 -0.06685011
    ## [2,] 3.5584977 4.994330 3.9379771 5.1905018  3.21232620
    ## 
    ## , , Benin
    ## 
    ##          [,1]     [,2]     [,3]      [,4]       [,5]
    ## [1,] 1.614509 2.278655 1.470588 0.6978635 -0.7425855
    ## [2,] 3.490346 5.012334 3.579064 5.1853863  1.4007400
    ## 
    ## , , Burkina Faso
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 1.541036 2.815530 3.204556 1.315246 1.669336
    ## [2,] 3.509578 5.000577 3.201150 5.140294 1.547652
    ## 
    ## , , Cameroon
    ## 
    ##           [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 0.3805835 1.926714 1.256077 1.139853 -0.3155968
    ## [2,] 3.6921759 4.962097 3.267732 5.101189  2.0331992
    ## 
    ## , , Congo Democratic Republic
    ## 
    ##          [,1]     [,2]      [,3]     [,4]      [,5]
    ## [1,] 2.054518 1.548893 0.6588179 1.141659 -1.656690
    ## [2,] 3.481285 4.997134 3.4840919 5.131586  3.157996
    ## 
    ## , , Cote d'Ivoire
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.538612 2.586998 1.664344 1.217848 -1.097675
    ## [2,] 3.458898 5.019847 3.468302 5.113079  1.566580
    ## 
    ## , , Ethiopia
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.234433 2.667722 1.251174 1.987835 0.1411864
    ## [2,] 3.463005 5.064522 3.883845 5.182309 3.1230510
    ## 
    ## , , Ghana
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.502208 3.309794 2.364002 1.222324 -1.575251
    ## [2,] 3.496789 4.955784 3.452444 5.151519  3.398406
    ## 
    ## , , Guinea
    ## 
    ##         [,1]     [,2]     [,3]    [,4]       [,5]
    ## [1,] 1.51673 2.645890 1.559947 1.02495 -0.4648663
    ## [2,] 3.44377 5.068471 3.412729 5.11952  1.2336673
    ## 
    ## , , India
    ## 
    ##          [,1]     [,2]       [,3]      [,4]       [,5]
    ## [1,] 1.730801 2.220600 -0.6513015 0.9208466 -0.8533873
    ## [2,] 4.556863 5.013087  3.8707762 5.1084206  1.8644321
    ## 
    ## , , Kenya
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 1.164999 1.626079 0.857908 1.126047 -0.0452362
    ## [2,] 3.728785 5.210681 4.355300 5.182808  3.5591716
    ## 
    ## , , Liberia
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 1.513915 2.326553 1.176981 1.198190 0.725333
    ## [2,] 3.431356 4.983296 3.335941 5.137783 1.959348
    ## 
    ## , , Madagascar
    ## 
    ##          [,1]     [,2]     [,3]      [,4]      [,5]
    ## [1,] 1.595684 1.751767 1.929113 0.0839175 0.5807928
    ## [2,] 3.508909 5.050697 3.437585 5.1815975 1.5837381
    ## 
    ## , , Malawi
    ## 
    ##           [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 0.9217411 1.740507 1.890808 1.058043 1.463779
    ## [2,] 2.9162896 5.088991 2.976915 5.104717 2.213030
    ## 
    ## , , Mali
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 1.515274 2.740144 1.027390 1.229250 -0.4811589
    ## [2,] 3.510741 5.021333 3.244636 5.178564  1.8553459
    ## 
    ## , , Mozambique
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 1.893525 2.174765 2.563972 1.111325 1.930336
    ## [2,] 3.448346 5.030801 3.055654 5.109403 1.306878
    ## 
    ## , , Nepal
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.290717 2.081615 1.019371 1.324007 0.3638721
    ## [2,] 1.417112 4.976004 3.804222 5.113763 2.7966377
    ## 
    ## , , Niger
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.502122 2.642241 2.789944 1.237055 1.6032611
    ## [2,] 3.521154 4.970975 3.344750 5.154410 0.7229305
    ## 
    ## , , Nigeria
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 1.788672 2.611483 1.219640 1.350439 -0.6213742
    ## [2,] 3.558362 4.992574 3.740164 5.191149  2.6168193
    ## 
    ## , , Pakistan
    ## 
    ##          [,1]     [,2]      [,3]     [,4]      [,5]
    ## [1,] 0.281763 2.336591 0.8681863 1.097996 -0.171294
    ## [2,] 3.890569 4.975643 3.5851548 5.100859  1.376465
    ## 
    ## , , Rwanda
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 1.686746 2.979814 3.376699 1.024101 2.326879
    ## [2,] 3.450294 4.972424 3.433565 5.181746 2.499687
    ## 
    ## , , Senegal
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 1.782367 3.505220 2.929911 2.006801 1.281271
    ## [2,] 3.461560 4.970257 3.483374 5.122069 2.733137
    ## 
    ## , , Tanzania
    ## 
    ##          [,1]     [,2]     [,3]     [,4]      [,5]
    ## [1,] 1.202425 2.509193 1.580458 1.358766 1.4954566
    ## [2,] 2.894547 4.865637 0.397618 5.056905 0.4148872
    ## 
    ## , , Uganda
    ## 
    ##          [,1]     [,2]     [,3]     [,4]       [,5]
    ## [1,] 1.820563 1.760954 0.553650 1.240779 -0.7461476
    ## [2,] 3.627767 5.113125 4.000446 5.156258  3.0800601
    ## 
    ## , , Zimbabwe
    ## 
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 1.283054 1.874936 2.181961 1.102948 1.017927
    ## [2,] 3.556670 5.059976 3.233320 5.161617 1.947115

``` r
tau_alpha_pms_hat
```

    ## [1] 1.386594 1.057686

``` r
sigma_delta_hat
```

    ## , , 1
    ## 
    ##              [,1]         [,2]       [,3]       [,4]       [,5]
    ## [1,] 4.796649e-01 9.453555e-05 0.09463145 0.00000000 0.08959552
    ## [2,] 9.453555e-05 1.818119e-06 0.00000000 0.00000000 0.00000000
    ## [3,] 9.463145e-02 0.000000e+00 0.45723020 0.03885141 0.13153208
    ## [4,] 0.000000e+00 0.000000e+00 0.03885141 0.32953745 0.11229934
    ## [5,] 8.959552e-02 0.000000e+00 0.13153208 0.11229934 0.42567693
    ## 
    ## , , 2
    ## 
    ##             [,1]         [,2]         [,3]         [,4]       [,5]
    ## [1,] 0.002637201 0.0000000000 0.0000000000 0.0000000000 0.00000000
    ## [2,] 0.000000000 0.0002575665 0.0000000000 0.0000000000 0.00000000
    ## [3,] 0.000000000 0.0000000000 0.0004097972 0.0000000000 0.00000000
    ## [4,] 0.000000000 0.0000000000 0.0000000000 0.0003940897 0.00000000
    ## [5,] 0.000000000 0.0000000000 0.0000000000 0.0000000000 0.03802578

## <a name="cu"></a>

## 6. Family planning source data

These are are two family planning commodity source datasets provided in
this package - one for the national level observations,
`national_FPsource_data` and one for the subnational level data
`subnat_FPsource_data`. For the national level data, there is a vignette
`calculate_FPsource_national_data_from_DHSmicrodata` that explains how
the national level data was calculated using the DHS microdata. A
similar apporoach was used for the subnational data using IPUMS data.

``` r
national_FPsource_data
```

    ## # A tibble: 2,448 × 8
    ## # Groups:   year, Method, Country [903]
    ##     year Country      Super_region   Method      average_year sector_category    proportion     n
    ##    <dbl> <chr>        <chr>          <chr>              <dbl> <chr>                   <dbl> <dbl>
    ##  1  2008 Sierra Leone Western Africa Injectables        2008. Commercial_medical    0.265      68
    ##  2  2008 Sierra Leone Western Africa Injectables        2008. Other                 0.00540     1
    ##  3  2008 Sierra Leone Western Africa Injectables        2008. Public                0.730     178
    ##  4  2008 Sierra Leone Western Africa OC Pills           2008. Commercial_medical    0.587     140
    ##  5  2008 Sierra Leone Western Africa OC Pills           2008. Other                 0.0224      7
    ##  6  2008 Sierra Leone Western Africa OC Pills           2008. Public                0.390      98
    ##  7  2013 Sierra Leone Western Africa Implants           2014. Commercial_medical    0.240     166
    ##  8  2013 Sierra Leone Western Africa Implants           2014. Other                 0.00567     8
    ##  9  2013 Sierra Leone Western Africa Implants           2014. Public                0.755     542
    ## 10  2013 Sierra Leone Western Africa Injectables        2014. Commercial_medical    0.198     336
    ## # ℹ 2,438 more rows

``` r
subnat_FPsource_data
```

    ## # A tibble: 13,504 × 8
    ##    Country  Region              Method               average_year sector_categories proportion SE.proportion     n
    ##    <chr>    <chr>               <chr>                       <dbl> <chr>                  <dbl>         <dbl> <int>
    ##  1 Zimbabwe Bulawayo            Female Sterilization        2016. Other               2.14e-11         0        NA
    ##  2 Zimbabwe Harare              Female Sterilization        2016. Other               1.95e-11         0        NA
    ##  3 Zimbabwe Manicaland          Female Sterilization        2016. Other               2.06e-11         0        NA
    ##  4 Zimbabwe Mashonaland Central Female Sterilization        2016. Other               5.32e-11         0        NA
    ##  5 Zimbabwe Mashonaland East    Female Sterilization        2016. Other               5.60e-11         0        NA
    ##  6 Zimbabwe Mashonaland West    Female Sterilization        2016. Other               2.12e-11         0        NA
    ##  7 Zimbabwe Masvingo            Female Sterilization        2016. Other               5.59e-11         0        NA
    ##  8 Zimbabwe Matabeleland North  Female Sterilization        2016. Other               1.93e-11         0        NA
    ##  9 Zimbabwe Matabeleland South  Female Sterilization        2016. Other               2.29e- 1         0.154     2
    ## 10 Zimbabwe Midlands            Female Sterilization        2016. Other               2.09e-11         0        NA
    ## # ℹ 13,494 more rows
