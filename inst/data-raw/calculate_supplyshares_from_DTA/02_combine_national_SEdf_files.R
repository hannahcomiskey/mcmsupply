##############################################
# Source files
##############################################
library(dplyr)
library(stringr)
library(readxl)
library(haven)
library(tidyverse)
library(dplyr)

# Load in the helper functions
source("inst/data-raw/calculate_supplyshares_from_DTA/helper_functions.R")

##############################################
# Read in results
##############################################

# List the names of the proportions data
my_SEestimate_files <- list.files("inst/data-raw/calculate_supplyshares_from_DTA/proportions/")

# remove dudlist from list
my_SEestimate_files <- my_SEestimate_files[my_SEestimate_files != "dud_SElist_jobrun.RDS"]

# Extract country codes from file names
country_codes <- substr(my_SEestimate_files, 6,7)

# Create list of file names
tmp <- strsplit(sub("(_)(?=[^_]+$)", " ", my_SEestimate_files, perl=T), " ")
file_names <- unlist(lapply(tmp, function(l) l[[1]]))

# Read in all proportions data into one list
df.list <- lapply(paste0("inst/data-raw/calculate_supplyshares_from_DTA/proportions/",my_SEestimate_files), function(i){
  x = read_excel(i)
  x
})

# Apply file names to list of proportions data
names(df.list) <- file_names

# Mutate Year column to be double in all dfs
df.list <- lapply(df.list, function(x) {
  data.frame(x) %>%
    mutate(year = as.double(year))
  })

# Match country codes and country to create new column in each file
country_codes_DHS <- read_excel("inst/data-raw/country_codes_DHS.xlsx")

# Run function to combine dfs into one (function found in helper_functions.R file)
country_SEestimates <- create_subnational_df(file_names, df.list, country_codes, country_codes_DHS)

# Checking original data to see if sums to 1
country_SEestimates <- country_SEestimates %>%
  rowwise() %>%
  mutate(check_sum = sum( c(Public, Commercial_medical, Other),  na.rm=TRUE))

# Fixing country specific typos
# Afganistan
country_SEestimates[which(country_SEestimates$Country=="Afghanistan"),"year"] <- 2015 # listed as 1394

# India
updated_india_df <- country_SEestimates %>%
  filter(Country=="India") %>%
  mutate(year = recode(year, "92" = 1992)) # Checked using .DOC files from microdata

all_other_df <- country_SEestimates %>%
  filter(Country!="India")

joined_df <- bind_rows(updated_india_df, all_other_df)

# Replace 2 with 2002
country_SEestimates_2000 <- joined_df %>%
  filter(year==0) %>%
  mutate(year = recode(year, "0" = 2000))

all_other_df <- joined_df %>%
  filter(year!=0)

joined_df <- bind_rows(country_SEestimates_2000, all_other_df)

# Fixing years in Nepal, India, Ethiopia and Afghanistan
# Ethiopia
updated_et_df <- joined_df %>%
  filter(Country=="Ethiopia") %>%
  mutate(year = year + 8) # Checked using .DOC files from microdata

all_other_df <- joined_df %>%
  filter(Country!="Ethiopia")

joined_df <- bind_rows(updated_et_df, all_other_df)


# Nepal
unique(country_SEestimates[which(country_SEestimates$Country=="Nepal"),"year"])

updated_nepal_df <- joined_df %>%
  filter(Country=="Nepal") %>%
  mutate(year = recode(year, "52" = 1996,
                       "2057" = 2001,
                       "2062" = 2006,
                       "2067" = 2011,
                       "2073" = 2016,
                       "2078" = 2022)) # Checked using DHS microdata .DOC files

all_other_df <- joined_df %>%
  filter(Country!="Nepal")

joined_df <- bind_rows(updated_nepal_df, all_other_df)

# Liberia 2006 to 2007
unique(country_SEestimates[which(country_SEestimates$Country=="Liberia"),"year"])

updated_liberia_df <- joined_df %>%
  filter(Country=="Liberia") %>%
  mutate(year = recode(year, "2006" = 2007)) # Checked using DHS microdata .DOC files

all_other_df <- joined_df %>%
  filter(Country!="Liberia")

joined_df <- bind_rows(updated_liberia_df, all_other_df)

# Checking what surveys are present
joined_df %>% select(Country, year) %>% distinct()

# Regularising Year inputs adding "19" to 99, 98, 96 etc...
cleaned_SE_source <- joined_df %>%
  mutate(year= ifelse(nchar(year)==2, year+1900, year))

# Checking what surveys are present
cleaned_SE_source %>% select(Country, year) %>% distinct()

# Extracting the method we are interested in for now (removing condoms, other, emergency)
cleaned_SE_source <- cleaned_SE_source %>%
  filter(Method %in% c("Implant", "Injectable", "IUD", "Sterilization (F)", "OC Pills")) %>%
  arrange(Country, year)

# Recoding methods to match original data
cleaned_SE_source <- cleaned_SE_source %>%
  mutate(Method = recode(Method, "Sterilization (F)" = "Female Sterilization",
                         "Implant" = "Implants",
                         "OC Pills" = "OC Pills",
                         "IUD" = "IUD",
                         "Injectable" = "Injectables")) %>%
  mutate(average_year = year + 0.5)

# Load existing data to merge in new observations
load("~/Documents/R/mcmsupply/data/national_FPsource_data.rda") # load existing data

# # # Save all observations before any filtering based on sample size
joined_SE_source <- bind_rows(national_FPsource_data, cleaned_SE_source) %>%

saveRDS(joined_SE_source, file="inst/data-raw/SE_source_data_2025.RDS")

# # Removing observations with less than 15 sampling units
SE15_source <- joined_SE_source %>%
  filter(Public_n >=15 | Commercial_medical_n>=15 | Other_n >=15)
included_groups <- SE15_source %>% select(Country, Method, average_year) %>% distinct()
SE15_source1 <- left_join(included_groups, joined_SE_source)
SE15_source1 <- collapse_methods_fun(SE15_source1)
saveRDS(SE15_source1, file="inst/data-raw/SE_source_data_15_2025.RDS")

# # Removing observations with less than 20 sampling units and keeping 3 sectors if at least one is >= 20.
# # NOTE: I use this dataset in estimation
SE20_source <- joined_SE_source %>%
  filter(Public_n >=20 | Commercial_medical_n>=20 | Other_n >=20)
included_groups <- SE20_source %>% select(Country, Method, average_year) %>% distinct()
SE20_source1 <- left_join(included_groups, joined_SE_source)
SE20_source1 <- collapse_methods_fun(SE20_source1)
saveRDS(SE20_source1, file="inst/data-raw/national_FPsource_data.RDS")


#######################################################################
# Calculate covariance matrices for each method, country, year -------
#######################################################################

my_SEestimate_files <- list.files("inst/data-raw/calculate_supplyshares_from_DTA/varcov/") # list all the variance-covariance files you have

# remove dudlist from file list
my_SEestimate_files <- my_SEestimate_files[my_SEestimate_files != "dud_jobrun.RDS"]

# Extract country codes from filenames
country_codes <- substr(my_SEestimate_files,8,9)

# Match country codes and country to create new column in each file
load("data/Country_and_area_classification.rda") # country codes and names
year_codes <- gsub("(.*_){2}(\\d+)_.+", "\\2", my_SEestimate_files)


# Hacky: Fixing up year-codes (this list evolved over time. Feel free to improve!)
updated_year_codes <- vector()
for(i in 1:length(year_codes)) {
  updated_year_codes[i] <- ifelse(year_codes[i]=="1394", 2015, year_codes[i])
  updated_year_codes[i] <- ifelse(updated_year_codes[i]=="52", 1996, updated_year_codes[i])
  updated_year_codes[i] <- ifelse(updated_year_codes[i]=="2057", 2001,updated_year_codes[i] )
  updated_year_codes[i] <- ifelse(updated_year_codes[i]=="2062", 2006,updated_year_codes[i] )
  updated_year_codes[i] <- ifelse(updated_year_codes[i]=="2067", 2011,updated_year_codes[i] )
  updated_year_codes[i] <- ifelse(updated_year_codes[i]=="2073", 2016, updated_year_codes[i] ) # "2057" "2062" "2067" "2073"
  updated_year_codes[i] <- ifelse(nchar(updated_year_codes[i])==2, as.numeric(updated_year_codes[i])+1900, updated_year_codes[i]) # 98, 97
  updated_year_codes[i] <- ifelse(updated_year_codes[i]==0, 2000, updated_year_codes[i])
}

# Bespoke year details for Ethiopia and Liberia
e_index <- which(country_codes=='ET')
l_index <- which(country_codes=='LB')
updated_year_codes[e_index] <- as.numeric(updated_year_codes[e_index])+8
updated_year_codes[l_index[1]] <- as.numeric(2007)

# Create list of file names
tmp <- strsplit(sub("(_)(?=[^_]+$)", " ", my_SEestimate_files, perl=T), " ")
file_names <- unlist(lapply(tmp, function(l) l[[1]]))

# Read in all data files into one list
df.list <- lapply(paste0("inst/data-raw/calculate_supplyshares_from_DTA/varcov/",my_SEestimate_files), function(i){
  x = read_excel(i)
  x
})

# Apply the country-code names to the list
names(df.list) <- paste0(country_codes, "_", updated_year_codes)

# HACKY: There is some differences between variance-covariance matrices and the proportions.
# These are just the variations of the method names
n_method_cols <- c("Sterilization..F.","Implant", "Injectable", "IUD","OC.Pills") # as they are in raw varcov data
n_method <- c("Sterilization (F)","Implant", "Injectable", "IUD","OC Pills")
n_method_easier <- c("Sterilization","Implant", "Injectable", "IUD","OC.Pill") # Works better in grep
n_sector <- c('Public', 'Commercial_medical')
combinations <- paste0(rep(n_method, each=2), ":", n_sector) # change to, at the end
colsnames_vcov <- paste0(rep(n_method_cols, each=2), ".", n_sector)

# Create an array to store the variance-covariance matrices in
varcov <- array(dim = c(2, 2, length(n_method), length(updated_year_codes)))

for(i in 1:length(names(df.list))) {
  print(i)
  mycountrycode <- substr(names(df.list)[i], 1,2)
  mycountry <- country_codes_DHS %>% filter(Code==mycountrycode) %>% select(`Country Name`) %>% unlist() %>% as.vector()
  myyear <- sub("^[^_]*_", "", names(df.list)[i])
  mydf <- df.list[[i]]

  # Filter matrix for the data we are interested in (Public, commercial medical etc.)
  mydf <- mydf %>%
    filter(Method_sector %in% combinations) %>% # take the rows of method and sectors we are interested in
    select(Method_sector, any_of(combinations)) %>% # take the columns of method and sectors we are interested in
    mutate(Method_sector = case_when(Method_sector == "Implant:Commercial_medical" ~ "Implant.Commercial_medical",
                                     Method_sector == "Implant:Public" ~ "Implant.Public",
                                     Method_sector == "Sterilization (F):Commercial_medical" ~ "Sterilization..F..Commercial_medical",
                                     Method_sector == "Sterilization (F):Public" ~ "Sterilization..F..Public",
                                     Method_sector == "Injectable:Commercial_medical" ~ "Injectable.Commercial_medical",
                                     Method_sector == "Injectable:Public" ~ "Injectable.Public",
                                     Method_sector == "IUD:Commercial_medical" ~  "IUD.Commercial_medical",
                                     Method_sector == "IUD:Public" ~ "IUD.Public",
                                     Method_sector == "OC Pills:Commercial_medical" ~  "OC.Pills.Commercial_medical",
                                     Method_sector == "OC Pills:Public" ~ "OC.Pills.Public"))

  # Put the matrix into the correct order of rows and columns
  mydf <- mydf %>% arrange(factor(Method_sector, levels = colsnames_vcov))
  mydf <- mydf %>% select(any_of(c('Method_sector', combinations)))

  for(m in 1:length(n_method)) {
    mat <- as.matrix(mydf)
    row_indices <- grep(n_method_easier[m], mydf$Method_sector)
    if(length(row_indices)==0) {
      next
    } else {
      col_indices <- as.numeric(row_indices)+1
      mat <- as_tibble(mat[row_indices, col_indices])
      mat <- mat %>%
        mutate(across(where(is.character), as.numeric))
      varcov[,,m,i] <- as.matrix(mat)
    }
  }
}

saveRDS(names(df.list), file = 'inst/data-raw/names_varcov_2025.RDS') # save the order of countries-years that the array is made up of
saveRDS(varcov, file= "inst/data-raw/SE_source_data_VARCOV_2025.RDS") # save the array of variance-covariance matrices

# ##############################################################################################
# # Calculate logit transformation of covariance matrices for each method, country, year -------
# ##############################################################################################

# NOTE:
# The national model in mcmsupply uses logit-transformed variance-covariance matrices during the estimation process.
# When running the mcmsupply R package, if you need to supply data use the proportions and this logit-transformed variance-covariance matrix

# Read in the proportions data
SE_source_data <- readRDS("inst/data-raw/national_FPsource_data.RDS") %>%
  rename(Public.SE = se.Public,
         Commercial_medical.SE = se.Commercial_medical,
         Other.SE = se.Other) # Bivariate setting

# Read in the additional area information for each country
mcmsupply::Country_and_area_classification

area_classification <- mcmsupply::Country_and_area_classification %>%
  dplyr::select(`Country or area`, `Region`) %>%
  dplyr::rename(Country = `Country or area`)

# Join these dataframes
SE_source_data <- SE_source_data %>% left_join(area_classification)

# Adding missing country world regions
SE_source_data <- SE_source_data %>%
  mutate(Region = case_when(Country=="Bolivia" ~ "South America",
                            Country=="Kyrgyz Republic" ~ "Central Asia",
                            Country=="Moldova" ~ "Eastern Europe",
                            TRUE ~ as.character(Region)))

# Filter where the sample size is smaller than 2 people across all three sectors ---------
FP_source_data_wide <- SE_source_data %>% # Proportion data
  dplyr::select(Country, Region, Method,  average_year, Commercial_medical, Other, Public,  Commercial_medical.SE, Other.SE, Public.SE,  Commercial_medical_n, Other_n, Public_n, check_sum)

# When check_Total=1, replace missing values with 0 ----------
col_index <- which(colnames(FP_source_data_wide)=="Commercial_medical")-1 # column index before CM column, as CM=1
for (i in 1:nrow(FP_source_data_wide)) {
  if(FP_source_data_wide$check_sum[i]>0.99) {
    na_cols <- which(is.na(FP_source_data_wide[i, c("Commercial_medical", "Other", "Public")])==TRUE) # NA values
    FP_source_data_wide[i, na_cols+col_index] <- as.list(rep(0, length(na_cols)))
  }
}

# Transform exactly 1 and 0 values away from boundary using lemon-squeezer approach ---------
FP_source_data_wide <- FP_source_data_wide %>%
  dplyr::mutate(Commercial_medical = (Commercial_medical*(nrow(FP_source_data_wide)-1)+0.33)/nrow(FP_source_data_wide)) %>%   # Y and SE transformation to account for (0,1) limits (total in sector)
  dplyr::mutate(Other = (Other*(nrow(FP_source_data_wide)-1)+0.33)/nrow(FP_source_data_wide)) %>%
  dplyr::mutate(Public = (Public*(nrow(FP_source_data_wide)-1)+0.33)/nrow(FP_source_data_wide)) %>%
  dplyr::select(Country, Region, Method, average_year, Commercial_medical, Other, Public,  Commercial_medical.SE, Other.SE, Public.SE, Other_n, Public_n, Commercial_medical_n, check_sum) # count_NA, remainder)

# Clean SE values --------------
FP_source_data_wide$count_SE.NA <- rowSums(is.na(FP_source_data_wide %>% select(Public.SE, Commercial_medical.SE, Other.SE))) # count NAs
SE_source_data_wide_norm <- FP_source_data_wide %>% dplyr::filter(count_SE.NA==0 & Commercial_medical.SE>0 & Public.SE>0) # Normal obs. No action needed.
SE_source_data_wide_X <- FP_source_data_wide %>% dplyr::filter(Commercial_medical.SE==0 | Public.SE==0) # Get obs with two missing sectors

# Replacing observations with SE=0 and no NAs with imputed SE: Leontine's suggestion
SE_source_data_wide_X <- SE_source_data_wide_X %>%
  dplyr::filter(Public_n>=20 | Commercial_medical_n >=20 | Other_n >=20) # Remove small sample sizes (DHS has 10 units sampled per cluster as min., 20 as average)

col_index <- which(colnames(SE_source_data_wide_X)=="Commercial_medical.SE")-1 # column index before CM column, as CM=1
DEFT_data <- readxl::read_xlsx("inst/data-raw/DEFT_DHS_database.xlsx") %>%
  rename(average_year = Year)

# Impute standard errors if required
if(nrow(SE_source_data_wide_X)>0) {
  SE_source_data_wide_X <- SE_source_data_wide_X %>% left_join(DEFT_data)

  # https://onlinestatbook.com/2/sampling_distributions/samp_dist_p.html

  for(i in 1:nrow(SE_source_data_wide_X)) {
    num.SE0 <- which(SE_source_data_wide_X[i,c("Commercial_medical.SE","Other.SE","Public.SE")]<0.0001)
    num.SEna <- which(is.na(SE_source_data_wide_X[i,c("Commercial_medical.SE","Other.SE","Public.SE")])==TRUE)
    DEFT <- ifelse(is.na(SE_source_data_wide_X$DEFT[i])==TRUE, 1.5, SE_source_data_wide_X$DEFT[i])
    N1 <- sum(SE_source_data_wide_X[i, c('Other_n', 'Public_n', 'Commercial_medical_n')], na.rm=TRUE) # Number of women surveyed
    phat <- 0.5/(N1+1) # Posterior mean of p under Jefferys prior for true prevalence of 0s.
    SE.hat <- sqrt((phat*(1-phat))/N1)
    SE_source_data_wide_X[i,c(col_index+num.SEna,col_index+num.SE0)] <- SE.hat*DEFT
  }
  FP_source_data_wide <- bind_rows(SE_source_data_wide_norm, SE_source_data_wide_X) # Put data back together again
} else {
  FP_source_data_wide <- SE_source_data_wide_norm
}

# Remove proportions with two sectors still missing
FP_source_data_wide <- FP_source_data_wide %>%
  filter(is.na(Public)==FALSE & is.na(Other)==FALSE | is.na(Public)==FALSE & is.na(Commercial_medical)==FALSE | is.na(Commercial_medical)==FALSE & is.na(Other)==FALSE)

FP_source_data_wide <- FP_source_data_wide %>% dplyr::arrange(Country, Region, Method, average_year)

# Match standard methods naming
FP_source_data_wide <- FP_source_data_wide %>%
  mutate(Method = case_when(Method =="Female sterilization" ~ "Female Sterilization",
                            Method=="Pill" ~ "OC Pills",
                            TRUE ~ as.character(Method)))

# Apply row-ids for pulling out variance matrices in the correct order from variance-covariance array
FP_source_data_wide$row_id <- c(1:nrow(FP_source_data_wide))

# Get data indexing - used for arrays
method_order <- c("Female sterilization", "Implants", "Injectables", "IUD", "Pill" ) # As per the method correlation matrix
n_method <- c("Female Sterilization","Implants", "Injectables", "IUD","OC Pills")

# Adding indexes to proportion (functions found in helper_functions.R file) -----------------------------------------------------
FP_source_data_wide <- country_index_fun(FP_source_data_wide, unique(FP_source_data_wide$Country))
FP_source_data_wide <- region_index_fun(FP_source_data_wide, unique(FP_source_data_wide$Region))
FP_source_data_wide <- method_index_fun(FP_source_data_wide, n_method)  # Method column

# Time indexing - important for splines -----------------------------------
all_years <- seq(from = 1990, to = 2030, by=0.5)
n_all_years <- length(all_years)

time_index_table <- tibble(average_year = all_years, index_year = 1:length(all_years))

FP_source_data_wide <- FP_source_data_wide %>%
  mutate(index_year = match(average_year,all_years))

# Read in variance array ------------------
load("~/Documents/R/mcmsupply/data/national_FPsource_VARCOV_bivarlogitnormal.rda") # existing 3D format
vcov_array <- readRDS('inst/data-raw/SE_source_data_VARCOV_2025.RDS')
names_varcov <- readRDS('inst/data-raw/names_varcov_2025.RDS') # order of array as calculated
country_codes <- tibble(Code = substr(names_varcov, 1,2)) # Extract country codes

# Match country codes and country to create new column in each file
country_codes_DHS <- readxl::read_excel("inst/data-raw/country_codes_DHS.xlsx") %>% select(!c(`India States`, `State Name`)) # country codes and names
country_codes <- country_codes %>% left_join(country_codes_DHS)
year_codes <- word(names_varcov, 2, sep = "_")

array_codes <- country_codes %>% mutate(Year = as.numeric(year_codes)) %>% rename(Country = 'Country Name') # Matches dim of var-cov to pull out index of array to match data.
data_codes <- FP_source_data_wide %>% ungroup() %>% select(Country, Method, index_method, average_year) %>% mutate(Year = floor(average_year))

cleaned_SE_source <- method_index_fun(cleaned_SE_source, n_method)

# Transform the variance-covariance array to a matrix containing all the observations in the correct order
final_covar_df <- tibble()
for(i in 1:nrow(cleaned_SE_source)) { # newly added proportions data from above
  tmp <- cleaned_SE_source[i,]
  mycountry <- tmp$Country
  myyear <- tmp$year #as.numeric(tmp$average_year)-0.5 # match var-covar file names
  mymethod <- tmp$index_method
  array_num <- which(array_codes$Country==mycountry & array_codes$Year==myyear)
  varcov_mat_tmp <- vcov_array[,,mymethod,array_num]
  SE_vals <- sqrt(diag(varcov_mat_tmp))
  cov_vals <- c(varcov_mat_tmp[1,2], varcov_mat_tmp[2,1])
  mytib <- tibble(Country = mycountry,
                    Year = myyear,
                    Method = n_method[mymethod],
                    Sector = c('Public', 'Commercial_medical'),
                    SE_obs = SE_vals,
                    Covar_obs = cov_vals)
  final_covar_df <- rbind(final_covar_df, mytib)
}


##### NOTE:
# If you don't have any imputed data, you will not have the SE_source_data_wide_X object.
# You won't need the code below (lines 431-440) in that case, unless you have the countries with edits listed below

# Get transformed SE terms ----------------------------------------
SE_source_data_wide_X <- SE_source_data_wide_X %>%
  select(!DEFT)

# Adding in Pakistan 2017 for transformation due to weird behavior with covariance --------------
pak_data <- SE_source_data_wide_norm %>% filter(Country=='Pakistan' & average_year=='2017.5')
SE_source_data_wide_X <- rbind(SE_source_data_wide_X, pak_data)

# Adding in India 2005 for transformation due to weird behavior with covariance --------------
india_data <- SE_source_data_wide_norm %>% filter(Country=='India' & average_year=='2000.5' & Method=='IUD')
SE_source_data_wide_X <- rbind(SE_source_data_wide_X, india_data)

# Replace transformed SE values -----------------------------------
row_ids <- SE_source_data_wide_X %>% # logit.SE_source_data_wide_X %>%
  select(Country, Method, average_year) %>%
  mutate(Year = floor(average_year)) %>%
  distinct() %>%
  mutate(Method = case_when(Method =="Female sterilization" ~ "Female Sterilization",
                            Method=="Pill" ~ "OC Pills",
                            TRUE ~ as.character(Method))) %>%
  select(!average_year)

row_ids_covar <- row_ids %>% left_join(final_covar_df) %>% select(!SE_obs)

trans_SE <- SE_source_data_wide_X %>%
  select(Country, Method, average_year, Commercial_medical.SE, Public.SE) %>%
  mutate(Year = floor(average_year)) %>%
  select(!average_year) %>%
  pivot_longer(cols = c(Commercial_medical.SE, Public.SE), names_to = 'Sector', values_to =  'SE_obs') %>%
  mutate(Method = case_when(Method =="Female sterilization" ~ "Female Sterilization",
                            Method=="Pill" ~ "OC Pills",
                            TRUE ~ as.character(Method))) %>%
  mutate(Sector = gsub('.SE', '', Sector)) %>%
  left_join(row_ids_covar) %>%
  mutate(Covar_obs = replace_na(Covar_obs, 0)) # replace missing covariance with 0

final_covar_df.new <- anti_join(final_covar_df, row_ids) # remove old rows from dataset
final_covar_df.new <- rbind(final_covar_df.new, trans_SE) # replace with updated SE values
final_covar_df.new <- final_covar_df.new %>% mutate(average_year = Year + 0.5) %>% distinct()

# Get unique combinations of variables to recreate matrices
my_ids <- final_covar_df.new %>% ungroup() %>% select(Country, average_year, Method) %>% distinct()

# Create variance-covariance matrices
varcov.new <- array(dim = c(2, 2, nrow(my_ids))) # to store transformed SE matrices
for(i in 1:nrow(my_ids)[1]) {
  print(i)
  covmat_tmp <- matrix( nrow=2, ncol=2)
  tmp <- final_covar_df.new %>%
    filter(Country == my_ids$Country[i] & Year == floor(my_ids$average_year[i]) & Method==my_ids$Method[i]) %>% distinct() # get matching covariance data
  public_SE <- tmp %>% filter(Sector=='Public') %>% select(SE_obs) %>% unlist() %>% as.vector() # pull out public SE
  CM_SE <- tmp %>% filter(Sector=='Commercial_medical') %>% select(SE_obs) %>% unlist() %>% as.vector()
  tmp_covar <- tmp %>% filter(Sector=='Public') %>% select(Covar_obs) %>% unlist() %>% as.vector()

  covmat_tmp[1,1] <- public_SE^2
  covmat_tmp[2,2] <- CM_SE^2
  covmat_tmp[1,2] <- covmat_tmp[2,1] <-  tmp_covar
  varcov.new[,,i] <- covmat_tmp + diag(ncol(covmat_tmp))*0.0001 # random noise to make it invertible
  solve(varcov.new[,,i]) # check for suitability in dmnorm
}

# Bind to existing data
varcov.exist <- abind::abind(national_FPsource_VARCOV_bivarlogitnormal, varcov.new, along=3)
my_ids.exist <- bind_rows(national_varcov_order_bivarlogitnormal, my_ids) %>%
  group_by(Country, average_year, Method) %>%  # group first
  mutate(unique_id = row_number()) %>% # count repititions
  ungroup() %>%
  mutate(row_id = 1:n()) %>% # assign an ID for each entry
  filter(unique_id == 1) # only use the first ID for each combo

varcov.exist <- varcov.exist[,,my_ids.exist$row_id] # extract unique entries as per above

saveRDS(varcov.exist, file="inst/data-raw/national_FPsource_VARCOV_bivar.RDS")
saveRDS(my_ids.exist, file='inst/data-raw/national_varcov_order_bivarlogitnormal.RDS')


# Assume you have a 2x2 variance-covariance matrix 'vcov_mat' (observed varcov)
# and a vector of parameter estimates 'beta' (observed proportions)
# Define the logistic function
logit <- function(p) {
  return(log(p / (1 - p)))
}

# Compute the derivative of the logit function
grad_logit <- function(p) {
  return(1 / (p * (1 - p)))
}

# Transform the 2x2 variance-covariance matrix onto the logit scale (https://www.stat.rice.edu/~dobelman/notes_papers/math/TaylorAppDeltaMethod.pdf)
transform_vcov_logit <- function(vcov_mat, p) {
  # Calculate the gradient of the logit function
  J <- grad_logit(p)

  # Transform the variance-covariance matrix onto the logit scale
  vcov_logit <- matrix(NA, nrow=dim(vcov_mat)[1], ncol=dim(vcov_mat)[2])
  for(i in 1:dim(vcov_mat)[1]) {
    for(j in 1:dim(vcov_mat)[2]) {
      vcov_logit[i,j] <- J[i]* J[j] *vcov_mat[i,j]
    }
  }

  return(vcov_logit)
}

# Delta method to transform covariance matrix ---------------------------
varcov.logit <- array(dim = c(2, 2, nrow(my_ids.exist)))
for(i in 1:nrow(my_ids.exist)) {
  print(i)
  myid <- my_ids.exist[i,]

  my_p <- FP_source_data_wide %>% # Get public and private sectors
    select(Country, average_year, Method, Public, Commercial_medical) %>%
    filter(Country==myid$Country & average_year==myid$average_year &  Method==myid$Method) %>%
    select(Public, Commercial_medical) %>%
    unlist() %>% as.vector()

  myvcov <- varcov.exist[,,i]

  covmat_tmp <- transform_vcov_logit(myvcov, my_p)

  solve(covmat_tmp)

  varcov.logit[,,i] <- covmat_tmp

}

# These are the logit transformed variance-covariance matrices to be used in the mcmsupply R package
saveRDS(varcov.logit, file="inst/data-raw/national_FPsource_VARCOV_bivarlogitnormal.RDS")
