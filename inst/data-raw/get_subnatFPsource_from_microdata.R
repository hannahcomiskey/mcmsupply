###############
# Libraries
###############

library(ipumsr)
library(tidyverse)
library(dplyr)
library(haven)
library(survey)
library(labelled)
library(sf)
library(stringr)
library(readxl)

#########################
# Set Results Location
##########################

folder <- "inst/data-raw/"

##########################
# Analysis of IPUMS data
##########################

# Read data into R
ddi <- ipumsr::read_ipums_ddi("data/IPUMS/idhs_00005.xml") # Access this data through the IPUMS website.
ipums_data <- ipumsr::read_ipums_micro(ddi)

# Convert the labels to factors (and drop the unused levels)
ipums_data <- ipums_data %>%
  dplyr::mutate(Country = haven::as_factor(ipumsr::lbl_clean(COUNTRY)))

table(ipums_data$Country, useNA = "always") %>% nrow() # 42 countries - come back to this. Why?

# Count surveys for each country - influences choice of region column
n_surveys <- ipums_data %>%
  dplyr::group_by(Country) %>%
  dplyr::select(Country, YEAR) %>%
  dplyr::distinct() %>%
  dplyr::count()

country_codes <- readxl::read_excel("inst/data-raw/country_codes_DHS.xlsx") %>%
  dplyr::select(Code, `Country Name`) %>%
  dplyr::rename(Country = `Country Name`)

surveys_country_codes <- merge(n_surveys, country_codes)


##############################################
# Replace numeric codes with district names
##############################################

refined_data <- ipums_data %>%
  dplyr::mutate(Afghanistan_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_AF2015))) %>%
  dplyr::mutate(Angola_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_AO2015))) %>%
  dplyr::mutate(Bangladesh_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_BD1994_2014))) %>%
  dplyr::mutate(Benin_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_BJ1996_2017))) %>%
  dplyr::mutate(BFaso_subnat_93 = haven::as_factor(ipumsr::lbl_clean(GEO_BF1993))) %>%
  dplyr::mutate(BFaso_subnat_98 = haven::as_factor(ipumsr::lbl_clean(GEO_BF1998))) %>%
  dplyr::mutate(BFaso_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_BF2003_2010))) %>%
  dplyr::mutate(Burundi_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_BI2010_2016))) %>%
  dplyr::mutate(Cameroon_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_CM1991_2011))) %>%
  dplyr::mutate(Chad_subnat_96 = haven::as_factor(ipumsr::lbl_clean(GEOALT_TD1996))) %>%
  dplyr::mutate(Chad_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_TD2004_2014))) %>%
  dplyr::mutate(DRCongo_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_CD2007_2013))) %>%
  dplyr::mutate(CdIvoire_subnat_98 = haven::as_factor(ipumsr::lbl_clean(GEO_CI1998))) %>%
  dplyr::mutate(CdIvoire_subnat_11 = haven::as_factor(ipumsr::lbl_clean(GEO_CI2011))) %>%
  dplyr::mutate(CdIvoire_subnat_94 = haven::as_factor(ipumsr::lbl_clean(GEO_CI1994))) %>%
  dplyr::mutate(Egypt_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_EG1988_2014))) %>%
  dplyr::mutate(Ethiopia_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_ET2000_2016))) %>%
  dplyr::mutate(Ghana_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_GH1988_2014))) %>%
  dplyr::mutate(Guinea_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_GN1999_2018))) %>%
  dplyr::mutate(India_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_IA1992_2015))) %>%
  dplyr::mutate(Jordan_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_JO1990_2017))) %>%
  dplyr::mutate(Kenya_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_KE1989_2014))) %>%
  dplyr::mutate(Lesotho_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_LS2004_2014))) %>%
  dplyr::mutate(Liberia_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_LR2007_2013))) %>%
  dplyr::mutate(Madagascar_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_MG1992_2008))) %>%
  dplyr::mutate(Malawi_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_MW1992_2016))) %>%
  dplyr::mutate(Mali_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_ML1987_2018))) %>%
  dplyr::mutate(Morocco_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_MA1987_2003))) %>%
  dplyr::mutate(Mozambique_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_MZ1997_2011))) %>%
  dplyr::mutate(Myanmar_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_MM2015))) %>%
  dplyr::mutate(Namibia_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_NM1992_2013))) %>%
  dplyr::mutate(Nepal_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_NP1996_2016))) %>%
  dplyr::mutate(Niger_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_NE1992_2012))) %>%
  dplyr::mutate(Nigeria_subnat_99 = haven::as_factor(ipumsr::lbl_clean(GEO_NG1999))) %>%
  dplyr::mutate(Nigeria_subnat_90 = haven::as_factor(ipumsr::lbl_clean(GEO_NG1990))) %>%
  dplyr::mutate(Nigeria_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_NG2003_2018))) %>%
  dplyr::mutate(Pakistan_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_PK1991_2017))) %>%
  dplyr::mutate(Rwanda_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_RW1992_2005))) %>%
  #dplyr::mutate(Rwanda_subnat_08 = haven::as_factor(ipumsr::lbl_clean(GEO_RW2008))) %>%
  dplyr::mutate(Rwanda_subnat_1014 = haven::as_factor(ipumsr::lbl_clean(GEO_RW2005_2014))) %>%
  dplyr::mutate(Senegal_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_SN1986_2017))) %>%
  dplyr::mutate(SAfrica_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_ZA1998_2016))) %>%
  dplyr::mutate(SLanka_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_LK1987))) %>%
  dplyr::mutate(Sudan_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_SD1989))) %>%
  dplyr::mutate(Tanzania_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_TZ1991_2015))) %>%
  dplyr::mutate(Tunisia_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_TN1988))) %>%
  dplyr::mutate(Uganda_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_UG1995_2016))) %>%
  dplyr::mutate(Yemen_subnat_91 = haven::as_factor(ipumsr::lbl_clean(GEO_YE1991))) %>%
  dplyr::mutate(Yemen_subnat_13 = haven::as_factor(ipumsr::lbl_clean(GEO_YE2013))) %>%
  dplyr::mutate(Zambia_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_ZM1992_2018))) %>%
  dplyr::mutate(Zimbabwe_subnat = haven::as_factor(ipumsr::lbl_clean(GEO_ZW1994_2015)))

##############################################
# Replace source columns with labels
##############################################

refined_data <- refined_data %>%
  dplyr::mutate(Current_source_standard = haven::as_factor(ipumsr::lbl_clean(FPLASTSRCS))) %>%
  dplyr::mutate(Current_source_detailed = haven::as_factor(ipumsr::lbl_clean(FPLASTSRCD))) %>%
  dplyr::mutate(Method = haven::as_factor(ipumsr::lbl_clean(FPMETHNOW))) %>%
  dplyr::select(PSU, DOMAIN, SAMPLE, Country, YEAR, PERWEIGHT, Current_source_standard, Current_source_detailed, Method, Afghanistan_subnat:Zimbabwe_subnat)

##############################################
# Reduce to 5 methods
##############################################

subnat_source_data <- refined_data %>%
  dplyr::filter(Method %in% c("Pill", "Norplant/Implants", "Female Sterilization", "Injections", "IUD"))

##############################################
# Clean up
##############################################
# Mapping detailed columns to Current_source labels
Current_source_map <- subnat_source_data %>%
  dplyr::ungroup() %>%
  dplyr::select(Current_source_standard, Current_source_detailed) %>%
  dplyr::distinct()

subnat_source_data <- subnat_source_data %>%
  dplyr::mutate(Current_source_standard = as.character(Current_source_standard)) %>%
  dplyr::mutate(Current_source_standard =
           ifelse(is.na(Current_source_standard),
                  purrr::map_chr(.x = .$Current_source_detailed, ~ as.character(Current_source_map$Current_source_standard[match(.x, Current_source_map$Current_source_detailed)])),
                  Current_source_standard))

subnat_source_data <- subnat_source_data %>% dplyr::mutate(sector_standard = dplyr::case_when(Current_source_standard=="Govt Clinic/Pharm" | Current_source_standard=="Govt Home/Comm delivery" ~ "Public",
                                                                                Current_source_standard=="NGO" ~ "Commercial_medical",
                                                                                Current_source_standard=="Private Clin/Deliv" ~ "Commercial_medical",
                                                                                Current_source_standard=="Private Pharmacy" ~ "Commercial_medical",
                                                                                Current_source_standard=="Church, Shop, friends, books" ~ "Other",
                                                                                Current_source_standard=="Other" ~ "Other",
                                                                                Current_source_standard=="Don't know" | Current_source_standard=="Missing" | Current_source_standard=="NIU (not in universe)"  ~  NA_character_))


saveRDS(subnat_source_data, file = paste0(folder,"draft_subnat_source_data_1_ipums.RDS")) # saving interim results

# Collapse regions into one column
Region <- vector()
colindex1 <- which(colnames(subnat_source_data)=="Afghanistan_subnat")
colindex2 <- which(colnames(subnat_source_data)=="Zimbabwe_subnat")
for(i in 1:nrow(subnat_source_data)) {
  reg_index <- which(!is.na(subnat_source_data[i,colindex1:colindex2]))+(colindex1-1)
  if(length(reg_index)==0) {
    Region[i] <- NA
  } else {
    Region[i] <- as.vector(unlist(subnat_source_data[i,reg_index]))
  }
}

subnat_source_data_2 <- subnat_source_data
subnat_source_data_2$Region <- as.factor(Region)
subnat_source_data_2$sector <- as.factor(subnat_source_data_2$sector_standard)

saveRDS(subnat_source_data_2, file=paste0(folder,"draft_subnat_source_data_2_ipums.RDS")) # saving interim results

###########################
# dplyr::select required columns
###########################
subnat_source_data_3 <- subnat_source_data_2 %>%
  dplyr::select(!Afghanistan_subnat:Zimbabwe_subnat)

# Append columns together for pivots
subnat_source_data_3$method_region_country_year <- as.factor(paste(subnat_source_data_3$Method, "_", subnat_source_data_3$Region, "_", subnat_source_data_3$Country, "_", subnat_source_data_3$YEAR, sep=""))
subnat_source_data_3$method_region_country_sector <- as.factor(paste(subnat_source_data_3$Method, "_", subnat_source_data_3$Region,"_", subnat_source_data_3$Country,"_", subnat_source_data_3$sector,sep=""))
subnat_source_data_3$num <- 1

##############################################
# Remove any stratum that only have one PSU
##############################################
one_psu_strata <- subnat_source_data_3 %>% dplyr::count(DOMAIN) %>% dplyr::filter(n == 1)
subnat_source_data_3 <- subnat_source_data_3 %>%
  dplyr::filter(!(DOMAIN %in% one_psu_strata$DOMAIN))

saveRDS(subnat_source_data_3, file= paste0(folder,"draft_subnat_source_data_3_ipums.RDS")) # saving interim results

#############################################################
# Set up survey design - filter out NAs from design columns
#############################################################
n_methods <- c("Pill", "Norplant/Implants", "Female Sterilization", "Injections", "IUD")

subnat_source_data_3 <- subnat_source_data_3 %>%
  dplyr::filter(is.na(PSU)==FALSE & is.na(method_region_country_year)==FALSE & is.na(sector)==FALSE & is.na(Method)==FALSE & is.na(DOMAIN)==FALSE & is.na(PERWEIGHT)==FALSE & is.na(YEAR)==FALSE)

IPUMSdesign<- survey::svydesign(id= subnat_source_data_3$PSU, strata=subnat_source_data_3$DOMAIN, weights=subnat_source_data_3$PERWEIGHT, data=subnat_source_data_3, nest=TRUE)
options(survey::survey.lonely.psu="adjust")

####################################################
## Calculate subnational FPsource data  ------------
####################################################

method.sector <- as.data.frame(prop.table(survey::svytable(~method_region_country_year +sector, IPUMSdesign), 1))
method.sector <- method.sector %>% tidyr::separate(method_region_country_year, c("Method","Region", "Country", "Year"), "_") %>%
  dplyr::rename(Subnat_Freq=Freq)

# Subset design for sectors
country_year_comb <- subnat_source_data_3 %>%
  dplyr::group_by(Country) %>%
  dplyr::select(Country, YEAR) %>%
  dplyr::distinct()

for(i in 1:nrow(country_year_comb)) {
  country_code <- country_year_comb$Country[i]
  year <- country_year_comb$YEAR[i]
  tmp <- subnat_source_data_3 %>% dplyr::filter(Country==country_code & YEAR==year)
  tmp$method_region <- as.factor(paste(tmp$Method, "_", tmp$Region, sep=""))
  tmpdesign<-survey::svydesign(id= tmp$PSU, strata=tmp$DOMAIN, weights=tmp$PERWEIGHT, data=tmp, nest=TRUE)

  counts <- tmp %>%
    dplyr::group_by(sector) %>%
    dplyr::count(method_region) %>%
    tidyr::separate(method_region, c("Method","Region"), "_") %>%
    dplyr::rename(sector_categories = sector)

  my_SEdf_tmp1 <- as_tibble(survey::svyby(~I(sector=="Commercial_medical"), ~I(method_region), design=tmpdesign, svyciprop)) %>%
    tidyr::separate(`I(method_region)`, c("Method","Region"), "_") %>%
    dplyr::mutate(sector_categories="Commercial_medical") %>%
    dplyr::mutate(Country = country_code) %>%
    dplyr::mutate(Year = year) %>%
    dplyr::rename(Subnat_Freq=`I(sector == \"Commercial_medical\")`) %>%
    dplyr::rename(Subnat_Freq_SE = `se.as.numeric(I(sector == \"Commercial_medical\"))`)

  my_SEdf_tmp2 <- as_tibble(survey::svyby(~I(sector=="Public"), ~I(method_region), design=tmpdesign, svyciprop)) %>%
    tidyr::separate(`I(method_region)`, c("Method","Region"), "_") %>%
    dplyr::mutate(sector_categories="Public") %>%
    dplyr::mutate(Country = country_code) %>%
    dplyr::mutate(Year = year) %>%
    dplyr::rename(Subnat_Freq=`I(sector == \"Public\")`) %>%
    dplyr::rename(Subnat_Freq_SE = `se.as.numeric(I(sector == \"Public\"))`)

  my_SEdf_tmp3 <- as_tibble(survey::svyby(~I(sector=="Other"), ~I(method_region), design=tmpdesign, svyciprop)) %>%
    tidyr::separate(`I(method_region)`, c("Method","Region"), "_") %>%
    dplyr::mutate(sector_categories="Other") %>%
    dplyr::mutate(Country = country_code) %>%
    dplyr::mutate(Year = year) %>%
    dplyr::rename(Subnat_Freq=`I(sector == \"Other\")`) %>%
    dplyr::rename(Subnat_Freq_SE = `se.as.numeric(I(sector == \"Other\"))`)

  my_SEdfall <- rbind(my_SEdf_tmp3, my_SEdf_tmp2)
  my_SEdfall <- rbind(my_SEdfall, my_SEdf_tmp1)
  my_SEdfall <- dplyr::left_join(my_SEdfall, counts)


  saveRDS(as.data.frame(my_SEdfall), paste(folder, "/",country_code ,"_", year, "_SEdf_ipums.RDS" , sep="")) # Saving individual country-year surveys
}

##############################################
# Create interim functions
##############################################

combine_SEdata <- function(mydatapath) {
  my_SEestimate_files <- list.files(mydatapath)
  v326_files <- my_SEestimate_files %>%
    stringr::str_subset(pattern = "^.*\\_v326.xlsx") # Extracting the files that were updated using the v326 column

  india_files <- my_SEestimate_files %>%
    stringr::str_subset(pattern = "IA3") # Extracting the files that were updated using the v326 column

  if(length(v326_files)!=0) {
    remove_repeats <- stringr::str_split(v326_files, "_v326.xlsx", simplify = TRUE)[,1]
    remove_repeats <- paste0(remove_repeats, ".xlsx") # remove countries without v326 column
    my_SEestimate_files <- my_SEestimate_files[!my_SEestimate_files %in% remove_repeats]
  }

  if(length(india_files)!=0) {
    my_SEestimate_files <- my_SEestimate_files[!my_SEestimate_files %in% india_files]
  }

  # Extract country codes
  country_codes <- substr(my_SEestimate_files, 1,2)

  # Create list of file names
  tmp <- strsplit(sub("(_)(?=[^_]+$)", " ", my_SEestimate_files, perl=T), " ")
  file_names <- unlist(lapply(tmp, function(l) l[[1]]))
  file_names <- sub("_SEdf",  "",  file_names)

  # Read in all data into list
  df.list <- lapply(paste0(mydatapath,my_SEestimate_files), function(i){
    x = readxl::read_excel(i)
    x
  })

  # Mutate Year column to be double in all dfs
  df.list <- lapply(df.list, function(x) {
    data.frame(x) %>%
      dplyr::mutate(year = as.double(year)) %>%
      dplyr::select(modern_method_source:n)
  })

  # Read in all data into list
  df.list <- lapply(paste0(mydatapath,my_SEestimate_files), function(i){
    x = readxl::read_excel(i)
    x
  })

  names(df.list) <- file_names

  return(df.list)
}

# Make data; function to create universal df of estimates -------------------
combine_FPsource_files <- function(myfile_names, mydf.list, mycountry_codes, mycountry_codes_DHS){
  updated.df <- tibble::tibble()
  for(i in 1:length(myfile_names)) {
    print(i)
    countrycode <- names(mydf.list[i])
    countrycode <- substr(countrycode, 1,2)
    mydata <-  tibble::tibble(mydf.list[[i]])
    mydata <- mydata %>% dplyr::mutate(country_code = mycountry_codes[i],
                                       year = as.numeric(year),
                                       n = as.numeric(n))
    if(countrycode=="IA") {
      country_name <- "India" } else {
        if(countrycode=="PG") {
          country_name <- "Papua New Guinea" } else {
            country_name <- mycountry_codes_DHS %>%
              dplyr::dplyr::filter(Code==mycountry_codes[i]) %>%
              dplyr::select(`Country Name`) }
      }
    country_name <- as.vector(unlist(country_name))
    mydata <- mydata %>% dplyr::mutate(Country = country_name)
    updated.df <- dplyr::dplyr::bind_rows(mydata, updated.df)
  }
  return(updated.df)
}

###############################################
# Create subnational FP source
###############################################
sedf1 <- combine_SEdata(mydatapath = folder) # combine the individual FPsource files into one master list
file_names <- names(sedf1)
country_codes <- substr(file_names, 1,2)

# Match country codes and country to create new column in each file
country_codes_DHS <- readxl::read_excel(paste0(folder,"country_codes_DHS.xlsx")) # country codes and names

# Run function to combine dfs into one
country_SEestimates <- combine_FPsource_files(file_names, sedf1, country_codes, country_codes_DHS)

# Checking original data to see if sums to 1
country_SEestimates <- country_SEestimates %>%
  dplyr::group_by(year, modern_method_source, region, Country) %>%
  dplyr::mutate(check_sum = sum(Subnat_Freq, na.rm=TRUE))

#####################################
# Address individual country typos
#####################################

# Afghanistan
country_SEestimates[which(country_SEestimates$Country=="Afghanistan"),"year"] <- 2015 # 1394

# India
updated_india_df <- country_SEestimates %>%
  dplyr::filter(Country=="India") %>%
  dplyr::mutate(year = dplyr::recode(year, "0" = "2002",
                                     "1992" = "1992",
                                     "99" = "1999",
                                     "98" = "1998",
                                     "92" = "1992")) %>% # Checked using .DOC files from microdata
  dplyr::mutate(year = as.numeric(year))

all_other_df <- country_SEestimates %>%
  dplyr::filter(Country!="India")

joined_df <- dplyr::bind_rows(updated_india_df, all_other_df)

# Ethiopia
updated_et_df <- joined_df %>%
  dplyr::filter(Country=="Ethiopia") %>%
  dplyr::mutate(year = year + 8) # Checked using .DOC files from microdata

all_other_df <- joined_df %>%
  dplyr::filter(Country!="Ethiopia")

joined_df <- dplyr::bind_rows(updated_et_df, all_other_df)

# Nepal
updated_nepal_df <- joined_df %>%
  dplyr::filter(Country=="Nepal") %>%
  dplyr::mutate(year = dplyr::recode(year, "52" = 1996,
                                     "2057" = 2001,
                                     "2062" = 2006,
                                     "2067" = 2011,
                                     "2073" = 2016)) # Checked using DHS microdata .DOC files

all_other_df <- joined_df %>%
  dplyr::filter(Country!="Nepal")

joined_df <- dplyr::bind_rows(updated_nepal_df, all_other_df)

# Liberia
updated_liberia_df <- joined_df %>%
  dplyr::filter(Country=="Liberia") %>%
  dplyr::mutate(year = dplyr::recode(year, "2006" = 2007)) # Checked using DHS microdata .DOC files

all_other_df <- joined_df %>%
  dplyr::filter(Country!="Liberia")

joined_df <- dplyr::bind_rows(updated_liberia_df, all_other_df)

# Addressing issue with Pakistan and region names
joined_df <- joined_df %>%
  dplyr::mutate(region = dplyr::recode(region, "ict" = "islamabad (ict)", "kpk" = "khyber pakhtunkhwa", "gb"="gilgit baltistan"))

# Regularising Year inputs adding "19" to 99, 98, 96 etc...
cleaned_SE_source <- joined_df %>%
  dplyr::mutate(year= ifelse(nchar(year)==2, year+1900, year))

# Regularising Year to 1999-00 to 1999
cleaned_SE_source <- cleaned_SE_source %>%
  dplyr::mutate(year= ifelse(year==0, 1999, year))

# Regularising Year 2 to 2002 for Vietnam
cleaned_SE_source <- cleaned_SE_source %>%
  dplyr::mutate(year= ifelse(year==2, 2002, year))

# Extracting the method we are interested in for now (removing condoms, other, emergency)
cleaned_SE_source <- cleaned_SE_source %>%
  dplyr::filter(modern_method_source %in% c("Implant", "Injectable", "IUD", "Sterilization (F)", "OC Pills")) %>%
  dplyr::arrange(Country, year)

# Recoding methods to match original data
cleaned_SE_source <- cleaned_SE_source %>%
  dplyr::mutate(Method = dplyr::recode(modern_method_source, "Sterilization (F)" = "Female Sterilization",
                                       "Implant" = "Implants",
                                       "OC Pills" = "OC Pills",
                                       "IUD" = "IUD",
                                       "Injectable" = "Injectables")) %>%
  dplyr::rename(sector_category=sector_categories) %>%
  dplyr::rename(proportion = Subnat_Freq) %>%
  dplyr::rename(SE.proportion = Subnat_Freq_SE) %>%
  dplyr::mutate(average_year = year + 0.5) %>%
  dplyr::filter(!is.na(region)) # Taking out dodgey india file

# Taking required columns
my_vars <- c("Country", "region", "Method", "average_year", "sector_category", "proportion", "SE.proportion", "n")
cleaned_SE_source <- cleaned_SE_source[,my_vars]

# Fixing region names for typos and constant naming ---------------
cleaned_SE_source_2 <- cleaned_SE_source %>%
  dplyr::mutate(Region = tolower(region)) %>%
  dplyr::mutate(Region = gsub('eastern', 'east', Region)) %>%
  dplyr::mutate(Region = gsub('western', 'west', Region)) %>%
  dplyr::mutate(Region = gsub('southern', 'south', Region)) %>%
  dplyr::mutate(Region = gsub('northern', 'north', Region)) %>%
  dplyr::mutate(region = tolower(region)) %>%
  dplyr::mutate(Region = dplyr::case_when(region=="nwfp" ~ "nw frontier",
                                          region=="davao peninsula" ~ "davao",
                                          region=="central region" ~ "central",
                                          region=="arunachalpradesh" ~ "arunachal pradesh",
                                          region=="central /south" ~ "central/south",
                                          region=="northern mindanao" ~ "north mindanao",
                                          region=="southern mindanao" ~ "south mindanao",
                                          region=="western mindanao" ~ "west mindanao",
                                          region=="eastern visayas" ~ "east visayas",
                                          region=="southern visayas" ~ "south visayas",
                                          region=="western visayas" ~ "west visayas",
                                          region=="northern visayas" ~ "north visayas",
                                          region=="southern tagalog" ~ "south tagalog",
                                          region=="greater accra region" ~ "greater accra",
                                          region=="kasai-occidental" ~ "kasai occidental",
                                          region=="kasai-oriental" ~ "kasai oriental",
                                          region=="maharastra" ~ "maharashtra",
                                          region=="national capital region" ~ "national capital",
                                          region=="central region" ~ "central",
                                          region=="eastern region" ~ "east",
                                          region=="far-western" ~ "far west",
                                          region=="ashanti region" ~ "ashanti",
                                          region=="atlantique" ~ "atlantic",
                                          region=="est" ~ "east",
                                          region=="armm" ~ "autonomous region in muslim mindanao",
                                          region=="cordillera admin region" ~ "cordillera",
                                          region=="davao peninsula" ~ "davao",
                                          region=="extrême-nord" ~ "extreme north",
                                          region=="nord-ouest /sud-ouest" ~ "northwest & southwest",
                                          region=="centre" ~ "center",
                                          region=="centre-nord" ~ "center north",
                                          region=="centre-est" ~ "center east",
                                          region=="nord-est" ~ "north east",
                                          region=="north eastern" ~ "north east",
                                          region=="centre-ouest" ~ "center west",
                                          region=="extreme nor" ~ "extreme north",
                                          region=="nord ouest" ~ "north west",
                                          region=="north western" ~ "north west",
                                          region=="north-west" ~ "north west",
                                          region=="ouest /littoral" ~ "west/littoral",
                                          region=="west & littoral" ~ "west/littoral",
                                          region=="karnataka." ~ "karnataka",
                                          region=="nord" ~ "north",
                                          region=="nord-ouest" ~ "north west",
                                          region=="nord-est" ~ "north east",
                                          region=="nord est" ~ "north east",
                                          region=="ouest" ~ "west",
                                          region=="sud" ~ "south",
                                          region=="western" ~ "west",
                                          region=="eastern" ~ "east",
                                          region=="centre-sud" ~ "center south",
                                          region=="sud ouest" ~ "south west",
                                          region=="sud-ouest" ~ "south west",
                                          region=="south-west" ~ "south west",
                                          region=="centre /sud/est" ~ "central, south, & east",
                                          region=="northern region" ~ "north",
                                          region=="northern" ~ "north",
                                          region=="northeastern" ~ "north east",
                                          region=="farwestern" ~ "far west",
                                          region=="northwest" ~ "north west",
                                          region=="northeast" ~ "north east",
                                          region=="southeast" ~ "south east",
                                          region=="toumbouctou" ~ "tombouctou",
                                          region=="ouémé" ~ "oueme",
                                          region=="rest zanzibar" ~ "rest of zanzibar",
                                          region=="s‚gou" ~ "south gou",
                                          region=="southern region" ~ "south",
                                          region=="southern" ~ "south",
                                          region=="shinyinga" ~ "shinyanga",
                                          region=="addis" ~ "addis ababa",
                                          region=="addis abeba" ~ "addis ababa",
                                          region=="addis adaba"  ~ "addis ababa",
                                          region=="benishangul" ~ "benishangul-gumuz",
                                          region=="ben-gumz" ~ "benishangul-gumuz",
                                          region=="brong ahafo"  ~ "brong-ahafo",
                                          region=="brong ahafo region" ~ "brong-ahafo",
                                          region=="car" ~ "cordillera",
                                          region=="cordillera administ."  ~ "cordillera",
                                          region=="dar es salam" ~ "dar es salaam",
                                          region=="hauts basins" ~ "hauts bassins",
                                          region=="i - ilocos"  ~ "ilocos",
                                          region=="I - Ilocos Region" ~ "ilocos",
                                          region=="i - ilocos region " ~ "ilocos",
                                          region=="i - ilocos region" ~ "ilocos",
                                          region=="reg i, ilocos" ~ "ilocos",
                                          region=="ii - cagayan valley" ~ "cagayan valley",
                                          region=="ii - cagayan valley " ~ "cagayan valley",
                                          region=="reg ii, cagayan" ~ "cagayan valley",
                                          region=="iii - central luzon" ~ "central luzon",
                                          region=="iii - central luzon " ~ "central luzon",
                                          region=="reg iii, c-luzon" ~ "central luzon",
                                          region=="iva - calabarzon" ~ "calabarzon",
                                          region=="iva - calabarzon " ~ "calabarzon",
                                          region=="ivb - mimaropa" ~ "mimaropa",
                                          region=="ivb - mimaropa " ~ "mimaropa",
                                          region=="ix - zamboanga peninsula" ~ "zamboanga peninsula",
                                          region=="ix - zamboanga peninsula " ~ "zamboanga peninsula",
                                          region=="xi - davao" ~ "davao",
                                          region=="xi - davao peninsula" ~ "davao peninsula",
                                          region=="xii - soccsksargen" ~ "soccsksargen",
                                          region=="xiii - caraga" ~ "caraga",
                                          region=="viii - eastern visayas"~ "east visayas",
                                          region=="reg iv, s-tagalog" ~ "south tagalog",
                                          region=="s-tagalog" ~ "south tagalog",
                                          region=="reg ix, w-mindanao" ~ "west mindanao",
                                          region=="w-mindanao" ~ "west mindanao",
                                          region=="n-mindanao" ~ "north mindanao",
                                          region=="reg x, n-mindanao" ~ "north mindanao",
                                          region=="x - northern mindanao" ~ "north mindanao",
                                          region=="x - northern mindanao " ~ "north mindanao",
                                          region=="yaound‚ /douala" ~ "yaound/douala",
                                          region=="reg xi, s-mindanao" ~ "south mindanao",
                                          region=="reg v, bicol" ~ "bicol",
                                          region=="v - bicol" ~ "bicol",
                                          region=="v - bicol " ~ "bicol",
                                          region=="v - bicol region" ~ "bicol",
                                          region=="reg vi, w-visayas" ~ "west visayas",
                                          region=="vi - western visayas" ~ "west visayas",
                                          region=="vi - western visayas " ~ "west visayas",
                                          region=="reg vii, c-visayas" ~ "central visayas",
                                          region=="vii - central visayas" ~ "central visayas",
                                          region=="vii - central visayas " ~ "central visayas",
                                          region=="viii - eastern visayas " ~ "east visayas",
                                          region=="viii - eastern visayas" ~ "east visayas",
                                          region=="reg viii, e-samar" ~ "east samar",
                                          region=="xi - davao " ~ "davao",
                                          region=="xi - davao peninsula " ~ "davao peninsula",
                                          region=="xii - soccsksargen " ~ "soccsksargen",
                                          region=="xiii - caraga " ~ "caraga",
                                          region=="reg xii, c-mindanao" ~  "central mindanao",
                                          region=="c-mindanao" ~  "central mindanao",
                                          region=="snnp" ~ "snnpr",
                                          region=="tillabéri" ~ "tillaberi",
                                          region=="upper east region" ~ "upper east",
                                          region=="karnataka." ~ "karnataka",
                                          region=="maharastra" ~ "maharashtra",
                                          region=="midwestern" ~ "mid-west",
                                          region=="ncr"  ~ "national capital",
                                          region=="national capital region"  ~ "national capital",
                                          region=="oromiya" ~ "oromia",
                                          region=="reg i, ilocos" ~ "addis ababa",
                                          region=="yaoundé" ~ "yaounde",
                                          region=="zanziba south" ~ "zanzibar south",
                                          region=="affar" ~ "afar",
                                          region=="coast" ~ "coastal",
                                          region=="kasaï occident" ~ "kasai occidental",
                                          region=="kasaï oriental" ~ "kasai oriental",
                                          region=="tillab‚ri" ~ "tillaberi",
                                          region=="upper west region" ~ "upper west",
                                          region=="volta region" ~ "volta",
                                          region=="western region" ~ "west",
                                          region=="zamb‚zia" ~ "zambezia",
                                          region=="zamb‚zia " ~ "zambezia",
                                          region=="southwest" ~ "south west",
                                          region=="kigali ville (pvk)" ~ "kigali",
                                          TRUE ~ Region))

subnat_country <- cleaned_SE_source_2 %>%
  dplyr::ungroup() %>%
  dplyr::select(Country, Region) %>%
  dplyr::distinct()
n_subnat <- unique(subnat_country$Region)

saveRDS(cleaned_SE_source_2, file="data/subnatSE_source_data_admin1.RDS")
