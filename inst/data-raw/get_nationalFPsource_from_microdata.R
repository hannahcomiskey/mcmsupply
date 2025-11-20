###############
# Libraries
###############
library(tidyverse)
library(dplyr)
library(haven)
library(survey)
library(labelled)
library(stringr)
library(readxl)
library(xlsx)

###########################
# Set Results Location
###########################

folder <- "inst/data-raw/"

###########################
# Interim functions
###########################

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
    updated.df <- dplyr::dplyr::dplyr::bind_rows(mydata, updated.df)
  }
  return(updated.df)
}

#### Collapsing Some Methods into 'Others' category
collapse_methods_fun <- function(my_data) {
  my_data$Method_collapse <- as.vector(sapply(my_data[,'Method'], FUN = function(x)
    ifelse(x=="Implants","Implants",
           ifelse(x=="Injectables","Injectables",
                  ifelse(x=="IUD","IUD",
                         ifelse(x %in% c("Female Sterilization","Female sterilization"),"Female sterilization",
                                ifelse(x %in% c("Male Sterilization","Male sterilization"),"Male sterilization",
                                       ifelse(x %in% c("OC Pills", "Pill"),"Pill",
                                              ifelse(x=="Condom","Condom", "Others")))))))))
  return(my_data)
}

########################################################
# Analysis of most recent survey
########################################################
country_codes_DHS <- readxl::read_excel("data/country_codes_DHS.xlsx") # country codes and names

data <- haven::read_dta(file="data/dta_files/UGIR61FL.DTA")
year <-  min(data$v007)
survey <- "DHS"
population <- "AW"
country_code <- unique(data$v000)
year
labelled::val_labels(data$v024)

data$region <- haven::as_factor(data$v024, levels = "labels")

# Percentage currently using a modern method
data <- data %>% dplyr::mutate(mcpr= dplyr::case_when(v313==3 ~ 1, TRUE ~ 0))

# Percentage married
data <- data %>% dplyr::mutate(married= dplyr::case_when(v502==1 ~ 1, TRUE ~ 0))

labelled::val_labels(data$v312)
data <- data %>% dplyr::mutate(modern_method= dplyr::case_when(v312==6 ~ "Sterilization (female)",
                                                 v312==7 ~ "Sterilization (male)",
                                                 v312==2 ~ "IUD",
                                                 v312==11 ~ "Implant",
                                                 v312==3 ~ "Injectable",
                                                 v312==1 ~ "OC Pills",
                                                 v312==5 ~ "Condom (m)"  ,
                                                 v312==13 ~ "LAM" ,
                                                 v312==4 | v312==14 | v312==15 | v312==16 | v312==17 | v312==18 | v312==19 | v312==20 ~ "Other Modern Methods"  ,
                                                 v312==0 | v312==8 | v312==9 | v312==10 | v312==12 ~ "None" ))

data <- data %>% dplyr::mutate(modern_method_source= dplyr::case_when(v312==6 ~ "Sterilization (F)",
                                                        v312==7 ~ "Sterilization (M)",
                                                        v312==2 ~ "IUD",
                                                        v312==11 ~ "Implant",
                                                        v312==3 ~ "Injectable",
                                                        v312==1 ~ "OC Pills",
                                                        v312==5 ~ "Condom (M)",
                                                        v312==14 ~ "Condom (F)",
                                                        v312==4 | v312==15 | v312== 17 | v312== 18 | v312== 19 | v312== 20 ~ "Other Modern Methods",
                                                        v312==16 ~ "Emergency contraception",
                                                        v312==0 | v312==8 | v312==9 | v312==10 | v312==12 ~ "None"))


labelled::val_labels(data$v327)
data <- data %>% dplyr::mutate(sector= dplyr::case_when(v327==1 | v327==2 ~ "Public",
                                          v327==3 ~ "NGO",
                                          v327==4 ~ "Private Clinic",
                                          v327==5 ~ "Pharmacy",
                                          v327==6 ~ "Shop Church Friend",
                                          v327==7 ~ "Other",
                                          v327==8 ~  NA_character_))

data <- data %>% dplyr::mutate(sector_categories= dplyr::case_when(v327==1 | v327==2 ~ "Public",
                                          v327==3 ~ "Commercial_medical",
                                          v327==4 ~ "Commercial_medical",
                                          v327==5 ~ "Commercial_medical",
                                          v327==6 ~ "Other",
                                          v327==7 ~ "Other",
                                          v327==8 ~  NA_character_))

data$num <- 1
data$sampleweights <- data$v005/1000000

data <- data %>%
  dplyr::filter(is.na(sector_categories)==FALSE & modern_method_source %in% c("Implant", "Injectable", "OC Pills", "Sterilization (F)", "Sterilization (M)", "Condom (M)", "Condom (F)", "IUD","Other Modern Methods", "Emergency contraception"))

data %>% dplyr::count(sector)
data %>% dplyr::count(sector_categories)
data %>% dplyr::count(modern_method_source)
counts <- data %>%
  dplyr::group_by(sector_categories) %>%
  dplyr::count(modern_method_source) %>%
  dplyr::rename(method = modern_method_source)


######################################################################################################
# Calculate Percent distribution of current users of modern methods, by most recent source of method
######################################################################################################

test <- data %>%
  dplyr::select(v021, v023, sampleweights, modern_method_source, sector_categories)

# Numerator: Number of women, by declared most recent source of contraception (V326)
# Denominator: Number of women who currently use a modern method of contraception, excluding LAM (v313 = 3 & v312 ≠ 13)

# Complex sample design parameters

DHSdesign<- survey::svydesign(id=data$v021, strata=NULL, weights=data$sampleweights, data=data, nest=TRUE)
options(survey::survey.lonely.psu="adjust")

# #################################
# # Calculate SE  ---------------
# #################################

method.sector.nat <- as.data.frame(prop.table(survey::svytable(~modern_method_source +sector, DHSdesign), 1))

survey::svytable(~modern_method_source +sector, DHSdesign)

method.sector.nat <- method.sector.nat %>% dplyr::rename(Nat_Freq=Freq)

summary(DHSdesign)

##############################################
# Remove any stratum that only have one PSU
##############################################

one_psu_strata <- data %>%
  dplyr::count(v023) %>%
  dplyr::filter(n == 1)

data <- data %>%
  dplyr::filter(!(v023 %in% one_psu_strata$v023))

##############################################
# CI = yhat +/- 2SE
##############################################

# Calculating sector, method SE proportion -------------------------------
n_methods <- c("Implant", "Injectable", "IUD", "Condom (M)", "Sterilization (F)", "OC Pills", "Emergency contraception", "Other Modern Methods")

my_SEdf_public <- data.frame()
  for(m in n_methods) {
    print(m)
    test_data <- data %>%
      dplyr::filter(sector_categories=="Public" & modern_method_source==m)
    if(nrow(test_data) != 0) {

      tmp <- as.data.frame(survey::svyby(~I(sector_categories=="Public"), ~I(modern_method_source==m), design=DHSdesign, survey::svyciprop))

      my_tmpDF <- tibble(
        sector_categories="Public",
        method = m,
        proportion = tmp[2,2], # hardcode for now
        SE.proportion =  tmp[2,3])

      my_SEdf_public <- rbind(my_tmpDF, my_SEdf_public)
    } else {
      next
    }
  }

my_SEdf_comm_med <- data.frame()
  for(m in n_methods) {
    print(m)
    test_data <- data %>% dplyr::filter(sector_categories=="Commercial_medical" & modern_method_source==m)
    if(nrow(test_data) != 0) {
      tmp <- as.data.frame(survey::svyby(~I(sector_categories=="Commercial_medical"), ~I(modern_method_source==m), design=DHSdesign, survey::svyciprop))
      my_tmpDF <- tibble(
        sector_categories="Commercial_medical",
        method = m,
        proportion = tmp[2,2], # hardcode for now
        SE.proportion =  tmp[2,3])

      my_SEdf_comm_med <- rbind(my_tmpDF, my_SEdf_comm_med)
      } else {
      next
      }
  }

my_SEdf_other <- data.frame()
for(m in n_methods) {
  print(m)
  test_data <- data %>% dplyr::filter(sector_categories=="Other" & modern_method_source==m)
  if(nrow(test_data) != 0) {
    tmp <- as.data.frame(survey::svyby(~I(sector_categories=="Other"), ~I(modern_method_source==m), design=DHSdesign, survey::svyciprop))
    my_tmpDF <- tibble(
      sector_categories="Other",
      method = m,
      proportion = tmp[2,2], # hardcode for now
      SE.proportion =  tmp[2,3])

    my_SEdf_other <- rbind(my_tmpDF, my_SEdf_other)
  } else{
      next }
}

my_SEdf <- rbind(my_SEdf_comm_med, my_SEdf_other)
my_SEdf <- rbind(my_SEdf, my_SEdf_public)
my_SEdf$country_code <- country_code
my_SEdf$year <- year
my_SEdf <- merge(my_SEdf, counts)

openxlsx::write.xlsx(as.data.frame(my_SEdf), paste(folder, "/", country_code,"_", year, "_SEdf.xlsx" , sep=""))


##############################################
# Read in results
##############################################

my_SEestimate_files <- list.files(folder)

# Extract country codes
country_codes <- substr(my_SEestimate_files, 1,2)

# Create list of file names
tmp <- strsplit(sub("(_)(?=[^_]+$)", " ", my_SEestimate_files, perl=T), " ")
file_names <- unlist(lapply(tmp, function(l) l[[1]]))

# Read in all data into list
df.list <- lapply(paste0("data/SEdata/",my_SEestimate_files), function(i){
  x = readxl::read_excel(i)
  x
})

names(df.list) <- file_names


# Mutate Year column to be double in all dfs
df.list <- lapply(df.list, function(x) {
  data.frame(x) %>%
    dplyr::mutate(year = as.double(year))})

# Match country codes and country to create new column in each file
country_codes_DHS <- readxl::read_excel(paste0(folder,"country_codes_DHS.xlsx")) # country codes and names

# Run function to combine dfs into one
country_SEestimates <- combine_FPsource_files(file_names, df.list, country_codes, country_codes_DHS)

# Checking original data to see if sums to 1
country_SEestimates <- country_SEestimates %>%
  dplyr::group_by(year, Method, Country) %>%
  dplyr::mutate(check_sum = sum(proportion, na.rm=TRUE))

# Afganistan
country_SEestimates[which(country_SEestimates$Country=="Afghanistan"),"year"] <- 2015 # 1394

# India
updated_india_df <- country_SEestimates %>%
  dplyr::filter(Country=="India") %>%
  dplyr::mutate(year = dplyr::recode(year, "92" = 1992)) # Checked using .DOC files from microdata

all_other_df <- country_SEestimates %>%
  dplyr::filter(Country!="India")

joined_df <- dplyr::bind_rows(updated_india_df, all_other_df)

# Replace 2 with 2002
country_SEestimates_2000 <- joined_df %>%
  dplyr::filter(year==0) %>%
  dplyr::mutate(year = dplyr::recode(year, "0" = 2000))

all_other_df <- joined_df %>%
  dplyr::filter(year!=0)

joined_df <- dplyr::bind_rows(country_SEestimates_2000, all_other_df)

# Fixing years in Nepal, India, Ethiopia and Afghanistan
# Ethiopia
updated_et_df <- joined_df %>%
  dplyr::filter(Country=="Ethiopia") %>%
  dplyr::mutate(year = year + 8) # Checked using .DOC files from microdata

all_other_df <- joined_df %>%
  dplyr::filter(Country!="Ethiopia")

joined_df <- dplyr::bind_rows(updated_et_df, all_other_df)


# nepal
unique(country_SEestimates[which(country_SEestimates$Country=="Nepal"),"year"])

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

# Liberia 2006 to 2007
unique(country_SEestimates[which(country_SEestimates$Country=="Liberia"),"year"])

updated_liberia_df <- joined_df %>%
  dplyr::filter(Country=="Liberia") %>%
  dplyr::mutate(year = dplyr::recode(year, "2006" = 2007)) # Checked using DHS microdata .DOC files

all_other_df <- joined_df %>%
  dplyr::filter(Country!="Liberia")

joined_df <- dplyr::bind_rows(updated_liberia_df, all_other_df)

# Regularising Year inputs adding "19" to 99, 98, 96 etc...
cleaned_SE_source <- joined_df %>%
  dplyr::mutate(year= ifelse(nchar(year)==2, year+1900, year))

# Extracting the method we are interested in for now (removing condoms, other, emergency)
cleaned_SE_source <- cleaned_SE_source %>%
  dplyr::filter(Method %in% c("Implant", "Injectable", "IUD", "Sterilization (F)", "OC Pills")) %>%
  arrange(Country, year)

# Recoding methods to match original data
cleaned_SE_source <- cleaned_SE_source %>%
  dplyr::mutate(Method = dplyr::recode(Method, "Sterilization (F)" = "Female Sterilization",
                         "Implant" = "Implants",
                         "OC Pills" = "OC Pills",
                         "IUD" = "IUD",
                         "Injectable" = "Injectables")) %>%
  dplyr::rename(sector_category = sector_categories) %>%
  dplyr::mutate(average_year = year + 0.5)

# Remove duplicate Rwanda and Sierra Leone data
updated_rwanda_df <- cleaned_SE_source %>%
  dplyr::filter(Country=="Rwanda") %>%
  dplyr::filter(is.na(n)==FALSE) %>%
  dplyr::distinct(Country, sector_category, Method,year,proportion, SE.proportion,country_code,n,average_year)

all_other_df <- cleaned_SE_source %>%
  dplyr::filter(Country!="Rwanda")

cleaned_SE_source <- dplyr::bind_rows(updated_rwanda_df, all_other_df)

updated_SL_df <- cleaned_SE_source %>%
  dplyr::filter(Country=="Sierra Leone") %>%
  dplyr::filter(is.na(n)==FALSE) %>%
  dplyr::distinct(Country, sector_category, Method,year,proportion, SE.proportion,country_code,n,average_year)

all_other_df <- cleaned_SE_source %>%
  dplyr::filter(Country!="Sierra Leone")

cleaned_SE_source <- dplyr::bind_rows(updated_SL_df, all_other_df)

# Method_collapse column
cleaned_SE_source <- collapse_methods_fun(cleaned_SE_source)
saveRDS(cleaned_SE_source, file=paste0(folder,"nat_FPsource_data.RDS"))

# Removing observations with less than 15 sampling units
SE15_source <- cleaned_SE_source %>%
  dplyr::filter(n >=15)
included_groups <- SE15_source %>%
  dplyr::select(Country, Method, average_year) %>%
  dplyr::distinct()
SE15_source1 <- dplyr::left_join(included_groups, cleaned_SE_source)
saveRDS(SE15_source1, file=paste0(folder,"nat_FPsourcedata_15.RDS"))

# Removing observations with less than 20 sampling units and keeping 3 sectors if at least one is >= 20.
SE20_source <- cleaned_SE_source %>%
  dplyr::filter(n >=20 | is.na(n))
included_groups <- SE20_source %>%
  dplyr::select(Country, Method, average_year) %>%
  dplyr::distinct()
SE20_source1 <- dplyr::left_join(included_groups, cleaned_SE_source)
saveRDS(SE20_source1, file=paste0(folder,"natSE_source_data_n20.RDS"))
