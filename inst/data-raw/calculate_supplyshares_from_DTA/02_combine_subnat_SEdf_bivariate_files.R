library(stringr)
library(readxl)
library(haven)
library(tidyverse)
library(dplyr)

# Read in results
my_SEestimate_files <- list.files("data/IPUMS/bivariate/proportions/")

# remove dudlist
my_SEestimate_files <- my_SEestimate_files[my_SEestimate_files != "country_folder"]

# Create list of file names
tmp <- strsplit(sub("(_)(?=[^_]+$)", " ", my_SEestimate_files, perl=T), " ")
file_names <- unlist(lapply(tmp, function(l) l[[1]]))
file_names <- str_replace(file_names, 'prop_', "")

# Read in all data into list 
df.list <- lapply(paste0("data/IPUMS/bivariate/proportions/",my_SEestimate_files), function(i){
  x = readRDS(i)
  x
})

names(df.list) <- file_names

# Match country codes and country to create new column in each file
country_codes_DHS <- readxl::read_excel("data/country_codes_DHS.xlsx") # country codes and names

country_SEestimates <- data.frame()
# Run function to combine dfs into one
for(i in 1:length(file_names)) {
  mydata <- data.frame(df.list[[i]]) %>%
    mutate(Country = as.character(Country),
           Region=as.character(Region)) 
  country_SEestimates <- bind_rows(mydata, country_SEestimates)
}

# Fixing Region names for typos and constant naming ---------------
country_SEestimates <- country_SEestimates %>% 
  select(Country, Region, Method, Year, Public, Private, se.Public, se.Private, Public_n, Private_n) %>%
  mutate(Region = str_to_title(Region)) %>%
  mutate(Region = str_to_title(Region)) %>%
  mutate(Region = case_when(Country=='Burkina Faso' & Region =='Est' ~ 'East', 
                            Country=='Burkina Faso' & Region =='Sud' ~ 'South',
                            Country=='Burkina Faso' & Region =='Nord' ~ 'North',
                            Country=='Burkina Faso' & Region =='Centre-Ouest' ~ 'Central/West',
                            Country=='Burkina Faso' & Region =='Centre-Sud' ~ 'Central/South',
                            Country=='Burkina Faso' & Region =='Centre-Nord' ~ 'Central/North',
                            Country=='Burkina Faso' & Region =='Centre-Est' ~ 'Central/East',
                            Country=='Burkina Faso' & Region =='Sud-Ouest' ~ 'South/West',
                            Country=="Cote d'Ivoire" & Region =='South Without Abidjan' ~ 'South',
                            Country=="Cote d'Ivoire" & Region =='Center' ~ 'Centre',
                            Country=="Cote d'Ivoire" & Region =='Capital (Abidjan)' ~ 'Abidjan',
                            Country=="Cote d'Ivoire" & Region =='City Of Abidjan' ~ 'Abidjan',
                            Country=="Cote d'Ivoire" & Region =='Center East' ~ 'Centre-East',
                            Country=="Cote d'Ivoire" & Region =='Center North' ~ 'Centre-North',
                            Country=="Cote d'Ivoire" & Region =='Center West' ~ 'Centre-West',
                            Country=="Cote d'Ivoire" & Region =='North East' ~ 'North-East',
                            Country=="Cote d'Ivoire" & Region =='North West' ~ 'North-West',
                            Country=="Cote d'Ivoire" & Region =='South West' ~ 'South-West',
                            Country=="Nigeria" & Region =='Southeast' ~ 'South-East',
                            Country=="Nigeria" & Region =='South East' ~ 'South-East',
                            Country=="Nigeria" & Region =='Southwest' ~ 'South-West',
                            Country=="Nigeria" & Region =='South West' ~ 'South-West',
                            Country=="Nigeria" & Region =='Northwest' ~ 'North-West',
                            Country=="Nigeria" & Region =='North West' ~ 'North-West',
                            Country=="Nigeria" & Region =='Northeast' ~ 'North-East',
                            Country=="Nigeria" & Region =='North East' ~ 'North-East',
                            .default = as.character(Region))) %>%
  filter(Region!='Na' & Region != "Countryside" & Region != "Small City") 

# Checking original data to see if sums to 1
country_SEestimates <- country_SEestimates %>%
  group_by(Year, Method, Country, Region) %>%
  mutate(check_sum = sum(Public, Private, na.rm=TRUE),
         Method = as.character(Method),
         Country = as.character(Country))

# Checking what surveys are present 
View(country_SEestimates %>% select(Country, Year) %>% distinct())

# Extracting the method we are interested in for now (removing condoms, other, emergency)
country_SEestimates <- country_SEestimates %>%
 # filter(Method %in% c("Norplant/Implants", "Injections", "IUD", "Female Sterilization", "Pill")) %>%
  arrange(Country, Year)

# Recoding methods to match original data
cleaned_SE_source <- country_SEestimates %>%
  # mutate(Method = recode(Method, 
  #                        "Female Sterilization" = "Female Sterilization",
  #                        "Norplant/Implants" = "Implants",
  #                        "Pill" = "OC Pills",
  #                        "IUD" = "IUD",
  #                        "Injections" = "Injectables")) %>%
  mutate(average_year = Year + 0.5)

saveRDS(cleaned_SE_source, file="data/subnat_bivar_SE_source_data.RDS")

# Removing observations with less than 15 sampling units
SE15_source <- cleaned_SE_source %>%
  filter(Public_n >=15 | Private_n >=15)
# included_groups <- SE15_source %>% select(Country, Method, average_year) %>% distinct()
# SE15_source1 <- left_join(included_groups, cleaned_SE_source)
saveRDS(SE15_source, file="data/subnat_bivar_SE_source_data_15.RDS")

# Removing observations with less than 20 sampling units and keeping 3 sectors if at least one is >= 20.
SE20_source <- cleaned_SE_source %>%
  filter(Public_n >=20 | Private_n >=20)
saveRDS(SE20_source, file="data/subnat_bivar_SE_source_data_20.RDS")

#######################################################################
# Calculate covariance matrices for each method, country, year -------
#######################################################################

my_SEestimate_files <- list.files("data/IPUMS/bivariate/varcov/") # list all the variance-covariance files you have

# Match country codes and country to create new column in each file
country_codes_DHS <- readxl::read_excel("data/country_codes_DHS.xlsx") # country codes and names
year_codes <- gsub("(.*_){2}(\\d+)_.+", "\\2", my_SEestimate_files)

# Create list of file names
tmp <- strsplit(sub("(_)(?=[^_]+$)", " ", my_SEestimate_files, perl=T), " ")
file_names <- unlist(lapply(tmp, function(l) l[[1]]))
file_names <- str_replace(file_names, 'varcov_', "")

# Read in all data files into one list 
df.list <- lapply(paste0("data/IPUMS/bivariate/varcov/",my_SEestimate_files), function(i){
  x = readRDS(i)
  x
})

# Apply the country-code names to the list
names(df.list) <- file_names

# HACKY: There is some differences between variance-covariance matrices and the proportions. 
# These are just the variations of the method names
n_method_cols <- c("Female Sterilization","Norplant/Implants", "Injections", "IUD","Pill") # as they are in raw varcov data
n_method <- c("Female Sterilization","Implants", "Injectables", "IUD","OC Pill")
n_sector <- c('Public', 'Private')
combinations <- paste0(rep(n_method, each=2), ":", n_sector) # change to, at the end
colsnames_vcov <- paste0(rep(n_method_cols, each=2), ":", n_sector)

# Create an array to store the variance-covariance matrices in
varcov <- array(dim = c(2, 2, length(n_method_cols), length(year_codes))) 
varcov_info <- tibble(Country = NA, Region = NA, Year = NA)

for(i in 1:length(file_names)) {
  print(i)
  mydf <- df.list[[i]]
  
  # Filter matrix for the data we are interested in (Public, commercial medical etc.)
  mydf <- mydf %>%
    filter(Method_sector %in% colsnames_vcov) %>% # take the rows of method and sectors we are interested in
    select(Country, Region, Year, Method_sector, any_of(colsnames_vcov)) 
  mydf_info <- mydf %>% select(Country, Region, Year) %>% distinct()
  varcov_info <- rbind(varcov_info, mydf_info)
  
  # Put the matrix into the correct order of rows and columns
  mydf <- mydf %>% arrange(factor(Method_sector, levels = colsnames_vcov)) 
  mydf <- mydf %>% select(any_of(c('Method_sector', colsnames_vcov)))
  
  for(m in 1:length(n_method_cols)) {
    mat <- as.matrix(mydf)
    row_indices <- grep(n_method_cols[m], mydf$Method_sector)
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
varcov_info <- varcov_info %>% filter(is.na(Country)==FALSE)
saveRDS(varcov_info, file = "data/names_varcov_bivar_2022.RDS") # save the order of countries-years that the array is made up of
saveRDS(varcov, file = "data/SE_source_data_VARCOV_bivar_2022.RDS") # save the array of variance-covariance matrices

# ##############################################################################################
# # Calculate logit transformation of covariance matrices for each method, country, year -------
# ##############################################################################################

# NOTE:
# The national model in mcmsupply uses logit-transformed variance-covariance matrices during the estimation process. 
# When running the mcmsupply R package, if you need to supply data use the proportions and this logit-transformed variance-covariance matrix

# Read in the proprtions data
SE_source_data <- readRDS("data/subnat_bivar_SE_source_data_20.RDS") %>%
  rename(Public.SE = se.Public,
         Private.SE = se.Private) # Bivariate setting

# Read in the additional area information for each country
area_classification <- read.csv("data/Country-and-area-classification-inclFP2020.csv")

area_classification <- area_classification %>% 
  dplyr::select(Country.or.area, Region) %>% 
  dplyr::rename(Country = Country.or.area)

# Join these dataframes 
SE_source_data <- SE_source_data %>% left_join(area_classification)

# Adding missing country world Region
SE_source_data <- SE_source_data %>% 
  mutate(Region=case_when(Country=="Bolivia" ~ "South America",
                            Country=="Kyrgyz Republic" ~ "Central Asia",
                            Country=="Moldova" ~ "Eastern Europe",
                            TRUE ~ as.character(Region)))

# Filter where the sample size is smaller than 2 people across all three sectors ---------
FP_source_data_wide <- SE_source_data %>% # Proportion data
  ungroup() %>%
  dplyr::select(Country, Region, Method,  average_year, Private, Public,  Private.SE, Public.SE,  Private_n, Public_n, check_sum)

# When check_Total=1, replace missing values with 0 ----------
col_index <- which(colnames(FP_source_data_wide)=="Private")-1 # column index before CM column, as CM=1
for (i in 1:nrow(FP_source_data_wide)) {
  if(FP_source_data_wide$check_sum[i]>0.99) {
    na_cols <- which(is.na(FP_source_data_wide[i, c("Private", "Public")])==TRUE) # NA values
    FP_source_data_wide[i, na_cols+col_index] <- as.list(rep(0, length(na_cols)))
  }
}

# Transform exactly 1 and 0 values away from boundary using lemon-squeezer approach ---------
FP_source_data_wide <- FP_source_data_wide %>%
  dplyr::mutate(Private = (Private*(nrow(FP_source_data_wide)-1)+0.5)/nrow(FP_source_data_wide)) %>%   # Y and SE transformation to account for (0,1) limits (total in sector)
  dplyr::mutate(Public = (Public*(nrow(FP_source_data_wide)-1)+0.5)/nrow(FP_source_data_wide)) %>%
  dplyr::select(Country, Region, Method, average_year, Private, Public,  Private.SE, Public.SE, Private_n, Public_n, check_sum) # count_NA, remainder)

# Clean SE values --------------
FP_source_data_wide$count_SE.NA <- rowSums(is.na(FP_source_data_wide %>% select(Public.SE, Private.SE))) # count NAs
SE_source_data_wide_norm <- FP_source_data_wide %>% dplyr::filter(count_SE.NA==0 & Private.SE>0 & Public.SE>0) # Normal obs. No action needed.
SE_source_data_wide_X <- FP_source_data_wide %>% dplyr::filter(Private.SE==0 | Public.SE==0 | is.na(Public.SE)==TRUE) # Get obs with two missing sectors

# Replacing observations with SE=0 and no NAs with imputed SE: Leontine's suggestion 
SE_source_data_wide_X <- SE_source_data_wide_X %>%
  dplyr::filter(Public_n>=20 | Private_n >=20 ) # Remove small sample sizes (DHS has 10 units sampled per cluster as min., 20 as average)
col_index <- which(colnames(SE_source_data_wide_X)=="Private.SE")-1 # column index before CM column, as CM=1
DEFT_data <- readxl::read_xlsx("data/DEFT_DHS_database.xlsx") %>%
  rename(average_year = Year)

SE_source_data_wide_X <- SE_source_data_wide_X %>% left_join(DEFT_data)

# https://onlinestatbook.com/2/sampling_distributions/samp_dist_p.html

for(i in 1:nrow(SE_source_data_wide_X)) {
  num.SE0 <- which(SE_source_data_wide_X[i,c("Private.SE","Public.SE")]<0.0001)
  num.SEna <- which(is.na(SE_source_data_wide_X[i,c("Private.SE","Public.SE")])==TRUE)
  DEFT <- ifelse(is.na(SE_source_data_wide_X$DEFT[i])==TRUE, 1.5, SE_source_data_wide_X$DEFT[i])
  N1 <- sum(SE_source_data_wide_X[i, c('Public_n', 'Private_n')], na.rm=TRUE) # Number of women surveyed
  phat <- 0.5/(N1+1) # Posterior mean of p under Jefferys prior for true prevalence of 0s.
  SE.hat <- sqrt((phat*(1-phat))/N1)
  SE_source_data_wide_X[i,c(col_index+num.SEna,col_index+num.SE0)] <- SE.hat*DEFT
}
FP_source_data_wide <- bind_rows(SE_source_data_wide_norm, SE_source_data_wide_X) # Put data back together again

FP_source_data_wide <- FP_source_data_wide %>% dplyr::arrange(Country, Region, Method, average_year)

# Apply row-ids for pulling out variance matrices in the correct order from variance-covariance array
FP_source_data_wide$row_id <- c(1:nrow(FP_source_data_wide))

# Get data indexing - used for arrays 
method_order <- c("Female Sterilization", "Implants", "Injectables", "IUD", "OC Pills" ) # As per the method correlation matrix
n_method <- c("Female Sterilization","Implants", "Injectables", "IUD","OC Pills")

# Adding indexes to proportion -----------------------------------------------------
method_index_table <- tibble(Method = n_method, index_method=1:length(n_method))
FP_source_data_wide <- FP_source_data_wide %>% left_join(method_index_table)

n_country <- unique(FP_source_data_wide$Country)
country_index_table <- tibble(Country = n_country, index_method=1:length(n_country))
FP_source_data_wide <- FP_source_data_wide %>% left_join(country_index_table)

region_index_table <- FP_source_data_wide %>% 
  select(Country, Region) %>% 
  distinct() 

region_index_table <- region_index_table %>% 
  mutate(index_Region=1:nrow(region_index_table))

FP_source_data_wide <- FP_source_data_wide %>% left_join(region_index_table)


# Time indexing - important for splines -----------------------------------
all_years <- seq(from = 1990, to = 2030, by=0.5)
n_all_years <- length(all_years)

time_index_table <- tibble(average_year = all_years, index_year = 1:length(all_years))

FP_source_data_wide <- FP_source_data_wide %>%
  mutate(index_year = match(average_year,all_years)) 

# Read in variance array ------------------
vcov_array <- readRDS('data/SE_source_data_VARCOV_bivar_2022.RDS')
names_varcov <- readRDS('data/names_varcov_bivar_2022.RDS') %>%
  mutate(Region = str_to_title(Region)) %>%
  mutate(Region = case_when(Country=='Burkina Faso' & Region =='Est' ~ 'East', 
                            Country=='Burkina Faso' & Region =='Sud' ~ 'South',
                            Country=='Burkina Faso' & Region =='Nord' ~ 'North',
                            Country=='Burkina Faso' & Region =='Centre-Ouest' ~ 'Central/West',
                            Country=='Burkina Faso' & Region =='Centre-Sud' ~ 'Central/South',
                            Country=='Burkina Faso' & Region =='Centre-Nord' ~ 'Central/North',
                            Country=='Burkina Faso' & Region =='Centre-Est' ~ 'Central/East',
                            Country=='Burkina Faso' & Region =='Sud-Ouest' ~ 'South/West',
                            Country=="Cote d'Ivoire" & Region =='South Without Abidjan' ~ 'South',
                            Country=="Cote d'Ivoire" & Region =='Center' ~ 'Centre',
                            Country=="Cote d'Ivoire" & Region =='Capital (Abidjan)' ~ 'Abidjan',
                            Country=="Cote d'Ivoire" & Region =='City Of Abidjan' ~ 'Abidjan',
                            Country=="Cote d'Ivoire" & Region =='Center East' ~ 'Centre-East',
                            Country=="Cote d'Ivoire" & Region =='Center North' ~ 'Centre-North',
                            Country=="Cote d'Ivoire" & Region =='Center West' ~ 'Centre-West',
                            Country=="Cote d'Ivoire" & Region =='North East' ~ 'North-East',
                            Country=="Cote d'Ivoire" & Region =='North West' ~ 'North-West',
                            Country=="Cote d'Ivoire" & Region =='South West' ~ 'South-West',
                            Country=="Nigeria" & Region =='Southeast' ~ 'South-East',
                            Country=="Nigeria" & Region =='South East' ~ 'South-East',
                            Country=="Nigeria" & Region =='Southwest' ~ 'South-West',
                            Country=="Nigeria" & Region =='South West' ~ 'South-West',
                            Country=="Nigeria" & Region =='Northwest' ~ 'North-West',
                            Country=="Nigeria" & Region =='North West' ~ 'North-West',
                            Country=="Nigeria" & Region =='Northeast' ~ 'North-East',
                            Country=="Nigeria" & Region =='North East' ~ 'North-East',
                            .default = as.character(Region)
  ))


data_codes <- FP_source_data_wide %>% ungroup() %>% select(Country, Region, Method, index_method, average_year) %>% mutate(Year = floor(average_year))

# Transform the variance-covariance array to a matrix containing all the observations in the correct order
final_covar_df <- tibble()
for(i in 1:nrow(data_codes)) {
  print(i)
  tmp <- data_codes[i,]
  mycountry <- tmp$Country
  myregion <- tmp$Region
  myyear <- as.numeric(tmp$average_year)-0.5 # match var-covar file names
  mymethod <- tmp$index_method
  array_num <- which(names_varcov$Country==mycountry & names_varcov$Region==myregion & names_varcov$Year==myyear)
  varcov_mat_tmp <- vcov_array[,,mymethod,array_num]
  SE_vals <- sqrt(diag(varcov_mat_tmp))
  cov_vals <- c(varcov_mat_tmp[1,2], varcov_mat_tmp[2,1]) 
  mytib <- tibble(Country = mycountry,
                  Region=myregion,
                  Year = myyear,
                  Method = tmp$Method,
                  Sector = c('Public', 'Private'),
                  SE_obs = SE_vals,
                  Covar_obs = cov_vals)
  final_covar_df <- rbind(final_covar_df, mytib)
}


# Get transformed SE terms ----------------------------------------
SE_source_data_wide_X <- SE_source_data_wide_X %>%
  select(!DEFT)

# # Adding in Pakistan 2017 for transformation due to weird behavior with covariance --------------
# #pak_data <- SE_source_data_wide_norm %>% filter(Country=='Pakistan' & average_year=='2017.5')
# #SE_source_data_wide_X <- rbind(SE_source_data_wide_X, pak_data)
# 
# # Adding in India 2005 for transformation due to weird behavior with covariance --------------
# india_data <- SE_source_data_wide_norm %>% filter(Country=='India' & average_year=='2000.5' & Method=='IUD')
# SE_source_data_wide_X <- rbind(SE_source_data_wide_X, india_data)

# Replace transformed SE values -----------------------------------
row_ids <- SE_source_data_wide_X %>% # logit.SE_source_data_wide_X %>%
  select(Country, Region, Method, average_year) %>%
  mutate(Year = floor(average_year)) %>%
  distinct() %>%
  mutate(Method = case_when(Method =="Female sterilization" ~ "Female Sterilization",
                            Method=="Pill" ~ "OC Pills", 
                            TRUE ~ as.character(Method))) %>%
  select(!average_year)

row_ids_covar <- row_ids %>% left_join(final_covar_df) %>% select(!SE_obs)

trans_SE <- SE_source_data_wide_X %>%
  select(Country, Region, Method, average_year, Private.SE, Public.SE) %>% 
  mutate(Year = floor(average_year)) %>%
  select(!average_year) %>%
  pivot_longer(cols = c(Private.SE, Public.SE), names_to = 'Sector', values_to =  'SE_obs') %>%
  mutate(Method = case_when(Method =="Female sterilization" ~ "Female Sterilization",
                            Method=="Pill" ~ "OC Pills", 
                            TRUE ~ as.character(Method))) %>%
  mutate(Sector = gsub('.SE', '', Sector)) %>%
  left_join(row_ids_covar) %>%
  mutate(Covar_obs = replace_na(Covar_obs, 0)) # replace missing covariance with 0

final_covar_df.new <- anti_join(final_covar_df, row_ids) # remove old rows from dataset
final_covar_df.new <- rbind(final_covar_df.new, trans_SE) # replace with updated SE values
final_covar_df.new <- final_covar_df.new %>% mutate(average_year = Year + 0.5)
saveRDS(final_covar_df.new, file="data/SE_source_data_VARCOV_updatedforSE_bivar_2022.RDS") # save intermediate files

# Get unique combinations of variables to recreate matrices
my_ids <- final_covar_df.new %>% ungroup() %>% select(Country, Region, average_year, Method) %>% distinct()

# Create variance-covariance matrices
varcov.new <- array(dim = c(2, 2, nrow(my_ids))) # to store transformed SE matrices
for(i in 1:nrow(my_ids)[1]) {
  print(i)
  covmat_tmp <- matrix( nrow=2, ncol=2)
  tmp <- final_covar_df.new %>%
    filter(Country == my_ids$Country[i] & Region==my_ids$Region[i] & Year == floor(my_ids$average_year[i]) & Method==my_ids$Method[i]) %>% distinct() # get matching covariance data
  public_SE <- tmp %>% filter(Sector=='Public') %>% select(SE_obs) %>% unlist() %>% as.vector() # pull out public SE
  CM_SE <- tmp %>% filter(Sector=='Private') %>% select(SE_obs) %>% unlist() %>% as.vector()
  tmp_covar <- tmp %>% filter(Sector=='Public') %>% select(Covar_obs) %>% unlist() %>% as.vector()
  
  covmat_tmp[1,1] <- public_SE^2
  covmat_tmp[2,2] <- CM_SE^2
  covmat_tmp[1,2] <- covmat_tmp[2,1] <-  tmp_covar
  varcov.new[,,i] <- covmat_tmp + diag(ncol(covmat_tmp))*0.0001 # random noise to make it invertible
  solve(varcov.new[,,i]) # check for suitability in dmnorm
}

saveRDS(varcov.new, file="data/SE_source_data_VARCOV_updatedforSE_array_bivar_2022.RDS")
saveRDS(my_ids, file='data/varcov_order_bivar_2022.RDS')

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
varcov.logit <- array(dim = c(2, 2, nrow(my_ids)))
for(i in 1:nrow(my_ids)) {
  print(i)
  myid <- my_ids[i,]
  
  my_p <- FP_source_data_wide %>% # Get public and private sectors 
    select(Country, Region, average_year, Method, Public, Private) %>% 
    filter(Country==myid$Country & Region==myid$Region & average_year==myid$average_year &  Method==myid$Method) %>%
    select(Public, Private) %>% 
    unlist() %>% as.vector()
  
  myvcov <- varcov.new[,,i]
  
  covmat_tmp <- transform_vcov_logit(myvcov, my_p)
  
  solve(covmat_tmp)
  
  varcov.logit[,,i] <- covmat_tmp
  
}

# These are the logit transformed variance-covariance matrices to be used in the mcmsupply R package
saveRDS(varcov.logit, file="data/SE_source_data_VARCOV_updatedforSE_logit_bivar_2022.RDS")
