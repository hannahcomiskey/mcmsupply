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

    ## Warning: replacing previous import 'stats::filter' by 'dplyr::filter' when
    ## loading 'mcmsupply'

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

    ##                                          Country.or.area ISO.Code
    ## 1                                            Afghanistan        4
    ## 2                                                Albania        8
    ## 3                                                Algeria       12
    ## 4                                         American Samoa       16
    ## 5                                                Andorra       20
    ## 6                                                 Angola       24
    ## 7                                               Anguilla      660
    ## 8                                    Antigua and Barbuda       28
    ## 9                                              Argentina       32
    ## 10                                               Armenia       51
    ## 11                                                 Aruba      533
    ## 12                                             Australia       36
    ## 13                                               Austria       40
    ## 14                                            Azerbaijan       31
    ## 15                                               Bahamas       44
    ## 16                                               Bahrain       48
    ## 17                                            Bangladesh       50
    ## 18                                              Barbados       52
    ## 19                                               Belarus      112
    ## 20                                               Belgium       56
    ## 21                                                Belize       84
    ## 22                                                 Benin      204
    ## 23                                               Bermuda       60
    ## 24                                                Bhutan       64
    ## 25                      Bolivia (Plurinational State of)       68
    ## 26                                Bosnia and Herzegovina       70
    ## 27                                              Botswana       72
    ## 28                                                Brazil       76
    ## 29                                British Virgin Islands       92
    ## 30                                     Brunei Darussalam       96
    ## 31                                              Bulgaria      100
    ## 32                                          Burkina Faso      854
    ## 33                                               Burundi      108
    ## 34                                              Cambodia      116
    ## 35                                              Cameroon      120
    ## 36                                                Canada      124
    ## 37                                            Cabo Verde      132
    ## 38                                        Cayman Islands      136
    ## 39                              Central African Republic      140
    ## 40                                                  Chad      148
    ## 41                                       Channel Islands      830
    ## 42                                                 Chile      152
    ## 43                                                 China      156
    ## 44        China, Hong Kong Special Administrative Region      344
    ## 45                                              Colombia      170
    ## 46                                               Comoros      174
    ## 47                                                 Congo      178
    ## 48                                          Cook Islands      184
    ## 49                                            Costa Rica      188
    ## 50                                         Cote d'Ivoire      384
    ## 51                                               Croatia      191
    ## 52                                                  Cuba      192
    ## 53                                                Cyprus      196
    ## 54                                        Czech Republic      203
    ## 55                 Democratic People's Republic of Korea      408
    ## 56                             Congo Democratic Republic      180
    ## 57                                               Denmark      208
    ## 58                                              Djibouti      262
    ## 59                                              Dominica      212
    ## 60                                    Dominican Republic      214
    ## 61                                               Ecuador      218
    ## 62                                                 Egypt      818
    ## 63                                           El Salvador      222
    ## 64                                     Equatorial Guinea      226
    ## 65                                               Eritrea      232
    ## 66                                               Estonia      233
    ## 67                                              Ethiopia      231
    ## 68                                        Faeroe Islands      234
    ## 69                           Falkland Islands (Malvinas)      238
    ## 70                                                  Fiji      242
    ## 71                                               Finland      246
    ## 72                                                France      250
    ## 73                                         French Guiana      254
    ## 74                                      French Polynesia      258
    ## 75                                                 Gabon      266
    ## 76                                                Gambia      270
    ## 77                                               Georgia      268
    ## 78                                               Germany      276
    ## 79                                                 Ghana      288
    ## 80                                             Gibraltar      292
    ## 81                                                Greece      300
    ## 82                                             Greenland      304
    ## 83                                               Grenada      308
    ## 84                                            Guadeloupe      312
    ## 85                                                  Guam      316
    ## 86                                             Guatemala      320
    ## 87                                                Guinea      324
    ## 88                                         Guinea-Bissau      624
    ## 89                                                Guyana      328
    ## 90                                                 Haiti      332
    ## 91                                              Holy See      336
    ## 92                                              Honduras      340
    ## 93                                               Hungary      348
    ## 94                                               Iceland      352
    ## 95                                                 India      356
    ## 96                                             Indonesia      360
    ## 97                            Iran (Islamic Republic of)      364
    ## 98                                                  Iraq      368
    ## 99                                               Ireland      372
    ## 100                                          Isle of Man      833
    ## 101                                               Israel      376
    ## 102                                                Italy      380
    ## 103                                              Jamaica      388
    ## 104                                                Japan      392
    ## 105                                               Jordan      400
    ## 106                                           Kazakhstan      398
    ## 107                                                Kenya      404
    ## 108                                             Kiribati      296
    ## 109                                               Kuwait      414
    ## 110                                           Kyrgyzstan      417
    ## 111                     Lao People's Democratic Republic      418
    ## 112                                               Latvia      428
    ## 113                                              Lebanon      422
    ## 114                                              Lesotho      426
    ## 115                                              Liberia      430
    ## 116                                                Libya      434
    ## 117                                        Liechtenstein      438
    ## 118                                            Lithuania      440
    ## 119                                           Luxembourg      442
    ## 120         Macao Special Administrative Region of China      446
    ## 121                                           Madagascar      450
    ## 122                                               Malawi      454
    ## 123                                             Malaysia      458
    ## 124                                             Maldives      462
    ## 125                                                 Mali      466
    ## 126                                                Malta      470
    ## 127                                     Marshall Islands      584
    ## 128                                           Martinique      474
    ## 129                                           Mauritania      478
    ## 130                                            Mauritius      480
    ## 131                                              Mayotte      175
    ## 132                                               Mexico      484
    ## 133                     Micronesia (Federated States of)      583
    ## 134                                               Monaco      492
    ## 135                                             Mongolia      496
    ## 136                                           Montenegro      499
    ## 137                                           Montserrat      500
    ## 138                                              Morocco      504
    ## 139                                           Mozambique      508
    ## 140                                              Myanmar      104
    ## 141                                              Namibia      516
    ## 142                                                Nauru      520
    ## 143                                                Nepal      524
    ## 144                                          Netherlands      528
    ## 145                                 Netherlands Antilles      530
    ## 146                                        New Caledonia      540
    ## 147                                          New Zealand      554
    ## 148                                            Nicaragua      558
    ## 149                                                Niger      562
    ## 150                                              Nigeria      566
    ## 151                                                 Niue      570
    ## 152                             Northern Mariana Islands      580
    ## 153                                               Norway      578
    ## 154                                                 Oman      512
    ## 155                                             Pakistan      586
    ## 156                                                Palau      585
    ## 157                                               Panama      591
    ## 158                                     Papua New Guinea      598
    ## 159                                             Paraguay      600
    ## 160                                                 Peru      604
    ## 161                                          Philippines      608
    ## 162                                             Pitcairn      612
    ## 163                                               Poland      616
    ## 164                                             Portugal      620
    ## 165                                          Puerto Rico      630
    ## 166                                                Qatar      634
    ## 167                                    Republic of Korea      410
    ## 168                                  Republic of Moldova      498
    ## 169                                              R_union      638
    ## 170                                              Romania      642
    ## 171                                   Russian Federation      643
    ## 172                                               Rwanda      646
    ## 173                                         Saint Helena      654
    ## 174                                Saint Kitts and Nevis      659
    ## 175                                          Saint Lucia      662
    ## 176                            Saint Pierre and Miquelon      666
    ## 177                     Saint Vincent and the Grenadines      670
    ## 178                                                Samoa      882
    ## 179                                           San Marino      674
    ## 180                                Sao Tome and Principe      678
    ## 181                                         Saudi Arabia      682
    ## 182                                              Senegal      686
    ## 183                                               Serbia      688
    ## 184                                           Seychelles      690
    ## 185                                         Sierra Leone      694
    ## 186                                            Singapore      702
    ## 187                                             Slovakia      703
    ## 188                                             Slovenia      705
    ## 189                                      Solomon Islands       90
    ## 190                                              Somalia      706
    ## 191                                         South Africa      710
    ## 192                                          South Sudan      728
    ## 193                                                Spain      724
    ## 194                                            Sri Lanka      144
    ## 195                                   State of Palestine      275
    ## 196                                                Sudan      729
    ## 197                                             Suriname      740
    ## 198                                            Swaziland      748
    ## 199                                               Sweden      752
    ## 200                                          Switzerland      756
    ## 201                                 Syrian Arab Republic      760
    ## 202                                           Tajikistan      762
    ## 203                                             Thailand      764
    ## 204            The former Yugoslav Republic of Macedonia      807
    ## 205                                          Timor-Leste      626
    ## 206                                                 Togo      768
    ## 207                                              Tokelau      772
    ## 208                                                Tonga      776
    ## 209                                  Trinidad and Tobago      780
    ## 210                                              Tunisia      788
    ## 211                                               Turkey      792
    ## 212                                         Turkmenistan      795
    ## 213                             Turks and Caicos Islands      796
    ## 214                                               Tuvalu      798
    ## 215                                               Uganda      800
    ## 216                                              Ukraine      804
    ## 217                                 United Arab Emirates      784
    ## 218 United Kingdom of Great Britain and Northern Ireland      826
    ## 219                                             Tanzania      834
    ## 220                             United States of America      840
    ## 221                         United States Virgin Islands      850
    ## 222                                              Uruguay      858
    ## 223                                           Uzbekistan      860
    ## 224                                              Vanuatu      548
    ## 225                   Venezuela (Bolivarian Republic of)      862
    ## 226                                              Vietnam      704
    ## 227                            Wallis and Futuna Islands      876
    ## 228                                       Western Sahara      732
    ## 229                                                Yemen      887
    ## 230                                               Zambia      894
    ## 231                                             Zimbabwe      716
    ##                          Major.area                    Region Developed.region
    ## 1                              Asia             Southern Asia               No
    ## 2                            Europe           Southern Europe              Yes
    ## 3                            Africa           Northern Africa               No
    ## 4                           Oceania                 Polynesia               No
    ## 5                            Europe           Southern Europe              Yes
    ## 6                            Africa             Middle Africa               No
    ## 7   Latin America and the Caribbean                 Caribbean               No
    ## 8   Latin America and the Caribbean                 Caribbean               No
    ## 9   Latin America and the Caribbean             South America               No
    ## 10                             Asia              Western Asia               No
    ## 11  Latin America and the Caribbean                 Caribbean               No
    ## 12                          Oceania Australia and New Zealand              Yes
    ## 13                           Europe            Western Europe              Yes
    ## 14                             Asia              Western Asia               No
    ## 15  Latin America and the Caribbean                 Caribbean               No
    ## 16                             Asia              Western Asia               No
    ## 17                             Asia             Southern Asia               No
    ## 18  Latin America and the Caribbean                 Caribbean               No
    ## 19                           Europe            Eastern Europe              Yes
    ## 20                           Europe            Western Europe              Yes
    ## 21  Latin America and the Caribbean           Central America               No
    ## 22                           Africa            Western Africa               No
    ## 23                 Northern America          Northern America              Yes
    ## 24                             Asia             Southern Asia               No
    ## 25  Latin America and the Caribbean             South America               No
    ## 26                           Europe           Southern Europe              Yes
    ## 27                           Africa           Southern Africa               No
    ## 28  Latin America and the Caribbean             South America               No
    ## 29  Latin America and the Caribbean                 Caribbean               No
    ## 30                             Asia        South-Eastern Asia               No
    ## 31                           Europe            Eastern Europe              Yes
    ## 32                           Africa            Western Africa               No
    ## 33                           Africa            Eastern Africa               No
    ## 34                             Asia        South-Eastern Asia               No
    ## 35                           Africa             Middle Africa               No
    ## 36                 Northern America          Northern America              Yes
    ## 37                           Africa            Western Africa               No
    ## 38  Latin America and the Caribbean                 Caribbean               No
    ## 39                           Africa             Middle Africa               No
    ## 40                           Africa             Middle Africa               No
    ## 41                           Europe           Northern Europe              Yes
    ## 42  Latin America and the Caribbean             South America               No
    ## 43                             Asia              Eastern Asia               No
    ## 44                             Asia              Eastern Asia               No
    ## 45  Latin America and the Caribbean             South America               No
    ## 46                           Africa            Eastern Africa               No
    ## 47                           Africa             Middle Africa               No
    ## 48                          Oceania                 Polynesia               No
    ## 49  Latin America and the Caribbean           Central America               No
    ## 50                           Africa            Western Africa               No
    ## 51                           Europe           Southern Europe              Yes
    ## 52  Latin America and the Caribbean                 Caribbean               No
    ## 53                             Asia              Western Asia               No
    ## 54                           Europe            Eastern Europe              Yes
    ## 55                             Asia              Eastern Asia               No
    ## 56                           Africa             Middle Africa               No
    ## 57                           Europe           Northern Europe              Yes
    ## 58                           Africa            Eastern Africa               No
    ## 59  Latin America and the Caribbean                 Caribbean               No
    ## 60  Latin America and the Caribbean                 Caribbean               No
    ## 61  Latin America and the Caribbean             South America               No
    ## 62                           Africa           Northern Africa               No
    ## 63  Latin America and the Caribbean           Central America               No
    ## 64                           Africa             Middle Africa               No
    ## 65                           Africa            Eastern Africa               No
    ## 66                           Europe           Northern Europe              Yes
    ## 67                           Africa            Eastern Africa               No
    ## 68                           Europe           Northern Europe              Yes
    ## 69  Latin America and the Caribbean             South America               No
    ## 70                          Oceania                 Melanesia               No
    ## 71                           Europe           Northern Europe              Yes
    ## 72                           Europe            Western Europe              Yes
    ## 73  Latin America and the Caribbean             South America               No
    ## 74                          Oceania                 Polynesia               No
    ## 75                           Africa             Middle Africa               No
    ## 76                           Africa            Western Africa               No
    ## 77                             Asia              Western Asia               No
    ## 78                           Europe            Western Europe              Yes
    ## 79                           Africa            Western Africa               No
    ## 80                           Europe           Southern Europe              Yes
    ## 81                           Europe           Southern Europe              Yes
    ## 82                 Northern America          Northern America              Yes
    ## 83  Latin America and the Caribbean                 Caribbean               No
    ## 84  Latin America and the Caribbean                 Caribbean               No
    ## 85                          Oceania                Micronesia               No
    ## 86  Latin America and the Caribbean           Central America               No
    ## 87                           Africa            Western Africa               No
    ## 88                           Africa            Western Africa               No
    ## 89  Latin America and the Caribbean             South America               No
    ## 90  Latin America and the Caribbean                 Caribbean               No
    ## 91                           Europe           Southern Europe              Yes
    ## 92  Latin America and the Caribbean           Central America               No
    ## 93                           Europe            Eastern Europe              Yes
    ## 94                           Europe           Northern Europe              Yes
    ## 95                             Asia             Southern Asia               No
    ## 96                             Asia        South-Eastern Asia               No
    ## 97                             Asia             Southern Asia               No
    ## 98                             Asia              Western Asia               No
    ## 99                           Europe           Northern Europe              Yes
    ## 100                          Europe           Northern Europe              Yes
    ## 101                            Asia              Western Asia               No
    ## 102                          Europe           Southern Europe              Yes
    ## 103 Latin America and the Caribbean                 Caribbean               No
    ## 104                            Asia              Eastern Asia              Yes
    ## 105                            Asia              Western Asia               No
    ## 106                            Asia              Central Asia               No
    ## 107                          Africa            Eastern Africa               No
    ## 108                         Oceania                Micronesia               No
    ## 109                            Asia              Western Asia               No
    ## 110                            Asia              Central Asia               No
    ## 111                            Asia        South-Eastern Asia               No
    ## 112                          Europe           Northern Europe              Yes
    ## 113                            Asia              Western Asia               No
    ## 114                          Africa           Southern Africa               No
    ## 115                          Africa            Western Africa               No
    ## 116                          Africa           Northern Africa               No
    ## 117                          Europe            Western Europe              Yes
    ## 118                          Europe           Northern Europe              Yes
    ## 119                          Europe            Western Europe              Yes
    ## 120                            Asia              Eastern Asia               No
    ## 121                          Africa            Eastern Africa               No
    ## 122                          Africa            Eastern Africa               No
    ## 123                            Asia        South-Eastern Asia               No
    ## 124                            Asia             Southern Asia               No
    ## 125                          Africa            Western Africa               No
    ## 126                          Europe           Southern Europe              Yes
    ## 127                         Oceania                Micronesia               No
    ## 128 Latin America and the Caribbean                 Caribbean               No
    ## 129                          Africa            Western Africa               No
    ## 130                          Africa            Eastern Africa               No
    ## 131                          Africa            Eastern Africa               No
    ## 132 Latin America and the Caribbean           Central America               No
    ## 133                         Oceania                Micronesia               No
    ## 134                          Europe            Western Europe              Yes
    ## 135                            Asia              Eastern Asia               No
    ## 136                          Europe           Southern Europe              Yes
    ## 137 Latin America and the Caribbean                 Caribbean               No
    ## 138                          Africa           Northern Africa               No
    ## 139                          Africa            Eastern Africa               No
    ## 140                            Asia        South-Eastern Asia               No
    ## 141                          Africa           Southern Africa               No
    ## 142                         Oceania                Micronesia               No
    ## 143                            Asia             Southern Asia               No
    ## 144                          Europe            Western Europe              Yes
    ## 145 Latin America and the Caribbean                 Caribbean               No
    ## 146                         Oceania                 Melanesia               No
    ## 147                         Oceania Australia and New Zealand              Yes
    ## 148 Latin America and the Caribbean           Central America               No
    ## 149                          Africa            Western Africa               No
    ## 150                          Africa            Western Africa               No
    ## 151                         Oceania                 Polynesia               No
    ## 152                         Oceania                Micronesia               No
    ## 153                          Europe           Northern Europe              Yes
    ## 154                            Asia              Western Asia               No
    ## 155                            Asia             Southern Asia               No
    ## 156                         Oceania                Micronesia               No
    ## 157 Latin America and the Caribbean           Central America               No
    ## 158                         Oceania                 Melanesia               No
    ## 159 Latin America and the Caribbean             South America               No
    ## 160 Latin America and the Caribbean             South America               No
    ## 161                            Asia        South-Eastern Asia               No
    ## 162                         Oceania                 Polynesia               No
    ## 163                          Europe            Eastern Europe              Yes
    ## 164                          Europe           Southern Europe              Yes
    ## 165 Latin America and the Caribbean                 Caribbean               No
    ## 166                            Asia              Western Asia               No
    ## 167                            Asia              Eastern Asia               No
    ## 168                          Europe            Eastern Europe              Yes
    ## 169                          Africa            Eastern Africa               No
    ## 170                          Europe            Eastern Europe              Yes
    ## 171                          Europe            Eastern Europe              Yes
    ## 172                          Africa            Eastern Africa               No
    ## 173                          Africa            Western Africa               No
    ## 174 Latin America and the Caribbean                 Caribbean               No
    ## 175 Latin America and the Caribbean                 Caribbean               No
    ## 176                Northern America          Northern America              Yes
    ## 177 Latin America and the Caribbean                 Caribbean               No
    ## 178                         Oceania                 Polynesia               No
    ## 179                          Europe           Southern Europe              Yes
    ## 180                          Africa             Middle Africa               No
    ## 181                            Asia              Western Asia               No
    ## 182                          Africa            Western Africa               No
    ## 183                          Europe           Southern Europe              Yes
    ## 184                          Africa            Eastern Africa               No
    ## 185                          Africa            Western Africa               No
    ## 186                            Asia        South-Eastern Asia               No
    ## 187                          Europe            Eastern Europe              Yes
    ## 188                          Europe           Southern Europe              Yes
    ## 189                         Oceania                 Melanesia               No
    ## 190                          Africa            Eastern Africa               No
    ## 191                          Africa           Southern Africa               No
    ## 192                          Africa            Eastern Africa               No
    ## 193                          Europe           Southern Europe              Yes
    ## 194                            Asia             Southern Asia               No
    ## 195                            Asia              Western Asia               No
    ## 196                          Africa           Northern Africa               No
    ## 197 Latin America and the Caribbean             South America               No
    ## 198                          Africa           Southern Africa               No
    ## 199                          Europe           Northern Europe              Yes
    ## 200                          Europe            Western Europe              Yes
    ## 201                            Asia              Western Asia               No
    ## 202                            Asia              Central Asia               No
    ## 203                            Asia        South-Eastern Asia               No
    ## 204                          Europe           Southern Europe              Yes
    ## 205                            Asia        South-Eastern Asia               No
    ## 206                          Africa            Western Africa               No
    ## 207                         Oceania                 Polynesia               No
    ## 208                         Oceania                 Polynesia               No
    ## 209 Latin America and the Caribbean                 Caribbean               No
    ## 210                          Africa           Northern Africa               No
    ## 211                            Asia              Western Asia               No
    ## 212                            Asia              Central Asia               No
    ## 213 Latin America and the Caribbean                 Caribbean               No
    ## 214                         Oceania                 Polynesia               No
    ## 215                          Africa            Eastern Africa               No
    ## 216                          Europe            Eastern Europe              Yes
    ## 217                            Asia              Western Asia               No
    ## 218                          Europe           Northern Europe              Yes
    ## 219                          Africa            Eastern Africa               No
    ## 220                Northern America          Northern America              Yes
    ## 221 Latin America and the Caribbean                 Caribbean               No
    ## 222 Latin America and the Caribbean             South America               No
    ## 223                            Asia              Central Asia               No
    ## 224                         Oceania                 Melanesia               No
    ## 225 Latin America and the Caribbean             South America               No
    ## 226                            Asia        South-Eastern Asia               No
    ## 227                         Oceania                 Polynesia               No
    ## 228                          Africa           Northern Africa               No
    ## 229                            Asia              Western Asia               No
    ## 230                          Africa            Eastern Africa               No
    ## 231                          Africa            Eastern Africa               No
    ##     Least.developed.country Sub.Saharan.Africa FP2020
    ## 1                       Yes                 No    Yes
    ## 2                        No                 No     No
    ## 3                        No                 No     No
    ## 4                        No                 No     No
    ## 5                        No                 No     No
    ## 6                       Yes                Yes     No
    ## 7                        No                 No     No
    ## 8                        No                 No     No
    ## 9                        No                 No     No
    ## 10                       No                 No     No
    ## 11                       No                 No     No
    ## 12                       No                 No     No
    ## 13                       No                 No     No
    ## 14                       No                 No     No
    ## 15                       No                 No     No
    ## 16                       No                 No     No
    ## 17                      Yes                 No    Yes
    ## 18                       No                 No     No
    ## 19                       No                 No     No
    ## 20                       No                 No     No
    ## 21                       No                 No     No
    ## 22                      Yes                Yes    Yes
    ## 23                       No                 No     No
    ## 24                      Yes                 No    Yes
    ## 25                       No                 No    Yes
    ## 26                       No                 No     No
    ## 27                       No                Yes     No
    ## 28                       No                 No     No
    ## 29                       No                 No     No
    ## 30                       No                 No     No
    ## 31                       No                 No     No
    ## 32                      Yes                Yes    Yes
    ## 33                      Yes                Yes    Yes
    ## 34                      Yes                 No    Yes
    ## 35                       No                Yes    Yes
    ## 36                       No                 No     No
    ## 37                       No                Yes     No
    ## 38                       No                 No     No
    ## 39                      Yes                Yes    Yes
    ## 40                      Yes                Yes    Yes
    ## 41                       No                 No     No
    ## 42                       No                 No     No
    ## 43                       No                 No     No
    ## 44                       No                 No     No
    ## 45                       No                 No     No
    ## 46                      Yes                Yes    Yes
    ## 47                       No                Yes    Yes
    ## 48                       No                 No     No
    ## 49                       No                 No     No
    ## 50                       No                Yes    Yes
    ## 51                       No                 No     No
    ## 52                       No                 No     No
    ## 53                       No                 No     No
    ## 54                       No                 No     No
    ## 55                       No                 No    Yes
    ## 56                      Yes                Yes    Yes
    ## 57                       No                 No     No
    ## 58                      Yes                Yes    Yes
    ## 59                       No                 No     No
    ## 60                       No                 No     No
    ## 61                       No                 No     No
    ## 62                       No                 No    Yes
    ## 63                       No                 No     No
    ## 64                      Yes                Yes     No
    ## 65                      Yes                Yes    Yes
    ## 66                       No                 No     No
    ## 67                      Yes                Yes    Yes
    ## 68                       No                 No     No
    ## 69                       No                 No     No
    ## 70                       No                 No     No
    ## 71                       No                 No     No
    ## 72                       No                 No     No
    ## 73                       No                 No     No
    ## 74                       No                 No     No
    ## 75                       No                Yes     No
    ## 76                      Yes                Yes    Yes
    ## 77                       No                 No     No
    ## 78                       No                 No     No
    ## 79                       No                Yes    Yes
    ## 80                       No                 No     No
    ## 81                       No                 No     No
    ## 82                       No                 No     No
    ## 83                       No                 No     No
    ## 84                       No                 No     No
    ## 85                       No                 No     No
    ## 86                       No                 No     No
    ## 87                      Yes                Yes    Yes
    ## 88                      Yes                Yes    Yes
    ## 89                       No                 No     No
    ## 90                      Yes                 No    Yes
    ## 91                       No                 No     No
    ## 92                       No                 No    Yes
    ## 93                       No                 No     No
    ## 94                       No                 No     No
    ## 95                       No                 No    Yes
    ## 96                       No                 No    Yes
    ## 97                       No                 No     No
    ## 98                       No                 No    Yes
    ## 99                       No                 No     No
    ## 100                      No                 No     No
    ## 101                      No                 No     No
    ## 102                      No                 No     No
    ## 103                      No                 No     No
    ## 104                      No                 No     No
    ## 105                      No                 No     No
    ## 106                      No                 No     No
    ## 107                      No                Yes    Yes
    ## 108                     Yes                 No     No
    ## 109                      No                 No     No
    ## 110                      No                 No    Yes
    ## 111                     Yes                 No    Yes
    ## 112                      No                 No     No
    ## 113                      No                 No     No
    ## 114                     Yes                Yes    Yes
    ## 115                     Yes                Yes    Yes
    ## 116                      No                 No     No
    ## 117                      No                 No     No
    ## 118                      No                 No     No
    ## 119                      No                 No     No
    ## 120                      No                 No     No
    ## 121                     Yes                Yes    Yes
    ## 122                     Yes                Yes    Yes
    ## 123                      No                 No     No
    ## 124                      No                 No     No
    ## 125                     Yes                Yes    Yes
    ## 126                      No                 No     No
    ## 127                      No                 No     No
    ## 128                      No                 No     No
    ## 129                     Yes                Yes    Yes
    ## 130                      No                Yes     No
    ## 131                      No                Yes     No
    ## 132                      No                 No     No
    ## 133                      No                 No     No
    ## 134                      No                 No     No
    ## 135                      No                 No    Yes
    ## 136                      No                 No     No
    ## 137                      No                 No     No
    ## 138                      No                 No     No
    ## 139                     Yes                Yes    Yes
    ## 140                     Yes                 No    Yes
    ## 141                      No                Yes     No
    ## 142                      No                 No     No
    ## 143                     Yes                 No    Yes
    ## 144                      No                 No     No
    ## 145                      No                 No     No
    ## 146                      No                 No     No
    ## 147                      No                 No     No
    ## 148                      No                 No    Yes
    ## 149                     Yes                Yes    Yes
    ## 150                      No                Yes    Yes
    ## 151                      No                 No     No
    ## 152                      No                 No     No
    ## 153                      No                 No     No
    ## 154                      No                 No     No
    ## 155                      No                 No    Yes
    ## 156                      No                 No     No
    ## 157                      No                 No     No
    ## 158                      No                 No    Yes
    ## 159                      No                 No     No
    ## 160                      No                 No     No
    ## 161                      No                 No    Yes
    ## 162                      No                 No     No
    ## 163                      No                 No     No
    ## 164                      No                 No     No
    ## 165                      No                 No     No
    ## 166                      No                 No     No
    ## 167                      No                 No     No
    ## 168                      No                 No     No
    ## 169                      No                Yes     No
    ## 170                      No                 No     No
    ## 171                      No                 No     No
    ## 172                     Yes                Yes    Yes
    ## 173                      No                Yes     No
    ## 174                      No                 No     No
    ## 175                      No                 No     No
    ## 176                      No                 No     No
    ## 177                      No                 No     No
    ## 178                      No                 No     No
    ## 179                      No                 No     No
    ## 180                     Yes                Yes    Yes
    ## 181                      No                 No     No
    ## 182                     Yes                Yes    Yes
    ## 183                      No                 No     No
    ## 184                      No                Yes     No
    ## 185                     Yes                Yes    Yes
    ## 186                      No                 No     No
    ## 187                      No                 No     No
    ## 188                      No                 No     No
    ## 189                     Yes                 No    Yes
    ## 190                     Yes                Yes    Yes
    ## 191                      No                Yes     No
    ## 192                     Yes                Yes    Yes
    ## 193                      No                 No     No
    ## 194                      No                 No    Yes
    ## 195                      No                 No    Yes
    ## 196                     Yes                Yes    Yes
    ## 197                      No                 No     No
    ## 198                      No                Yes     No
    ## 199                      No                 No     No
    ## 200                      No                 No     No
    ## 201                      No                 No     No
    ## 202                      No                 No    Yes
    ## 203                      No                 No     No
    ## 204                      No                 No     No
    ## 205                     Yes                 No    Yes
    ## 206                     Yes                Yes    Yes
    ## 207                      No                 No     No
    ## 208                      No                 No     No
    ## 209                      No                 No     No
    ## 210                      No                 No     No
    ## 211                      No                 No     No
    ## 212                      No                 No     No
    ## 213                      No                 No     No
    ## 214                     Yes                 No     No
    ## 215                     Yes                Yes    Yes
    ## 216                      No                 No     No
    ## 217                      No                 No     No
    ## 218                      No                 No     No
    ## 219                     Yes                Yes    Yes
    ## 220                      No                 No     No
    ## 221                      No                 No     No
    ## 222                      No                 No     No
    ## 223                      No                 No    Yes
    ## 224                     Yes                 No     No
    ## 225                      No                 No     No
    ## 226                      No                 No    Yes
    ## 227                      No                 No     No
    ## 228                      No                 No     No
    ## 229                     Yes                 No    Yes
    ## 230                     Yes                Yes    Yes
    ## 231                      No                Yes    Yes

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
