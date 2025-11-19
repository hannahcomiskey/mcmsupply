library(survey)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(tibble)
library(ipumsr)

########################################################
# Analysis of IPUMS data 
########################################################
# Read data in
options(stringsAsFactors = TRUE)
ddi <- read_ipums_ddi("data/idhs_00013.xml")
ipums_data <- read_ipums_micro(ddi)

# Convert the labels to factors (and drop the unused levels)
ipums_data <- ipums_data %>%
  mutate(Country = as_factor(lbl_clean(COUNTRY)))

table(ipums_data$Country, useNA = "always") %>% nrow()

# Count surveys for each country - influences choice of region column
n_surveys <- ipums_data %>%
  group_by(Country) %>%
  select(Country, YEAR) %>%
  distinct() %>%
  count()

country_codes <- readxl::read_excel("data/country_codes_DHS.xlsx") %>%
  select(Code, `Country Name`) %>%
  rename(Country = `Country Name`)

surveys_country_codes <- merge(n_surveys, country_codes) 

##############################################
# Replace numeric codes with district names
##############################################

refined_data <- ipums_data %>%
  mutate(Afghanistan_subnat = as_factor(lbl_clean(GEO_AF2015))) %>%
  mutate(Benin_subnat = as_factor(lbl_clean(GEO_BJ1996_2017))) %>%
  mutate(BFaso_subnat_93 = as_factor(lbl_clean(GEO_BF1993))) %>%
  mutate(BFaso_subnat_98 = as_factor(lbl_clean(GEO_BF1998))) %>%
  mutate(BFaso_subnat = as_factor(lbl_clean(GEO_BF2003_2018))) %>%
  mutate(Cameroon_subnat = as_factor(lbl_clean(GEO_CM1991_2022))) %>%
  mutate(DRCongo_subnat = as_factor(lbl_clean(GEO_CD2007_2013))) %>%
  mutate(CdIvoire_subnat_98 = as_factor(lbl_clean(GEO_CI1998))) %>%
  mutate(CdIvoire_subnat_11 = as_factor(lbl_clean(GEO_CI2011))) %>%
  mutate(CdIvoire_subnat_94 = as_factor(lbl_clean(GEO_CI1994))) %>%
  mutate(Ethiopia_subnat = as_factor(lbl_clean(GEO_ET2000_2019))) %>%
  mutate(Ghana_subnat = as_factor(lbl_clean(GEO_GH1988_2019))) %>%
  mutate(Guinea_subnat = as_factor(lbl_clean(GEO_GN1999_2021))) %>%
  mutate(India_subnat = as_factor(lbl_clean(GEO_IA1992_2019))) %>%
  mutate(Kenya_subnat = as_factor(lbl_clean(GEO_KE1989_2014))) %>%
  mutate(Liberia_subnat = as_factor(lbl_clean(GEO_LR2007_2019))) %>%
  mutate(Madagascar_subnat = as_factor(lbl_clean(GEO_MG1992_2021))) %>%
  mutate(Malawi_subnat = as_factor(lbl_clean(GEO_MW1992_2017))) %>%
  mutate(Mali_subnat = as_factor(lbl_clean(GEO_ML1987_2021))) %>%
  mutate(Mozambique_subnat = as_factor(lbl_clean(GEO_MZ1997_2018))) %>%
  mutate(Myanmar_subnat = as_factor(lbl_clean(GEO_MM2015))) %>%
  mutate(Nepal_subnat = as_factor(lbl_clean(GEO_NP1996_2016))) %>%
  mutate(Niger_subnat = as_factor(lbl_clean(GEO_NE1992_2021))) %>%
  mutate(Nigeria_subnat_99 = as_factor(lbl_clean(GEO_NG1999))) %>%
  mutate(Nigeria_subnat_90 = as_factor(lbl_clean(GEO_NG1990))) %>%
  mutate(Nigeria_subnat = as_factor(lbl_clean(GEO_NG2003_2021))) %>%
  mutate(Pakistan_subnat = as_factor(lbl_clean(GEO_PK1991_2017))) %>%
  mutate(Rwanda_subnat = as_factor(lbl_clean(GEO_RW1992_2005))) %>%
  mutate(Rwanda_subnat_0519 = as_factor(lbl_clean(GEO_RW2005_2019))) %>%
  mutate(Senegal_subnat = as_factor(lbl_clean(GEO_SN1986_2021))) %>%
  mutate(Tanzania_subnat = as_factor(lbl_clean(GEO_TZ1991_2017))) %>%
  mutate(Uganda_subnat = as_factor(lbl_clean(GEO_UG1995_2018))) %>%
  mutate(Zimbabwe_subnat = as_factor(lbl_clean(GEO_ZW1994_2015))) 

##############################################
# Replace source columns with labels
##############################################

refined_data <- refined_data %>%
  mutate(Current_source_standard = as_factor(lbl_clean(FPLASTSRCS))) %>%
  mutate(Current_source_detailed = as_factor(lbl_clean(FPLASTSRCD))) %>%
  mutate(Religion = as_factor(lbl_clean(RELIGION))) %>%
  mutate(Wealth_quintiles = as_factor(lbl_clean(WEALTHQ))) %>%
  mutate(Method = as_factor(lbl_clean(FPMETHNOW))) %>%
  select(PSU, DOMAIN, SAMPLE, Country, YEAR, PERWEIGHT, Current_source_standard, Current_source_detailed, Method, Afghanistan_subnat:Zimbabwe_subnat)

##############################################
# Reduce to 5 methods
##############################################

subnat_source_data <- refined_data %>% 
  filter(Method %in% c("Pill", "Norplant/Implants", "Female Sterilization", "Injections", "IUD"))

##############################################
# Clean up 
##############################################
# Mapping detailed columns to Current_source labels
Current_source_map <- subnat_source_data %>%
  ungroup() %>%
  select(Current_source_standard, Current_source_detailed) %>%
  distinct()

subnat_source_data <- subnat_source_data %>% 
  mutate(Current_source_standard = as.character(Current_source_standard)) %>% 
  mutate(Current_source_standard = 
           ifelse(is.na(Current_source_standard),
                  purrr::map_chr(.x = .$Current_source_detailed, ~ as.character(Current_source_map$Current_source_standard[match(.x, Current_source_map$Current_source_detailed)])), 
                  Current_source_standard))

subnat_source_data <- subnat_source_data %>% mutate(sector_standard = case_when(Current_source_standard=="Govt Clinic/Pharm" | Current_source_standard=="Govt Home/Comm delivery" ~ "Public",
                                                                                Current_source_standard=="NGO" ~ "Private",
                                                                                Current_source_standard=="Private Clin/Deliv" ~ "Private",
                                                                                Current_source_standard=="Private Pharmacy" ~ "Private",
                                                                                Current_source_standard=="Church, Shop, friends, books" ~ "Private",
                                                                                Current_source_standard=="Other" ~ "Private",
                                                                                Current_source_standard=="Don't know" | Current_source_standard=="Missing" | Current_source_standard=="NIU (not in universe)"  ~  NA_character_))



# Address issue with Rwanda subnational multi-names 
subnat_source_data <- subnat_source_data %>%
  mutate(Rwanda_subnat = case_when(Rwanda_subnat== "Butare, Gitarama (Central, South)" ~ "South",
                                   Rwanda_subnat== "Cyangugu, Gikongoro (Southwest)" ~ "South",
                                   Rwanda_subnat== "Byumba, Kibungo, Umutara (Northeast)" ~ "North",
                                   Rwanda_subnat== "Gisenyi, Kibuye, Ruhengeri (Northwest)" ~ "West",
                                   TRUE ~ as.character(Rwanda_subnat))) 

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

###########################
# Select required columns
###########################
subnat_source_data_3 <- subnat_source_data_2 %>%
  select(!Afghanistan_subnat:Zimbabwe_subnat)

# Append columns together for pivots
subnat_source_data_3$region_country_year <- as.factor(paste(subnat_source_data_3$Region, "_", subnat_source_data_3$Country, "_", subnat_source_data_3$YEAR, sep=""))
subnat_source_data_3$method_region_country_year <- as.factor(paste(subnat_source_data_3$Method, "_", subnat_source_data_3$Region, "_", subnat_source_data_3$Country, "_", subnat_source_data_3$YEAR, sep=""))
subnat_source_data_3$method_region_country_sector <- as.factor(paste(subnat_source_data_3$Method, "_", subnat_source_data_3$Region,"_", subnat_source_data_3$Country,"_", subnat_source_data_3$sector,sep=""))
subnat_source_data_3$num <- 1

##############################################
# Remove any stratum that only have one PSU
##############################################
one_psu_strata <- subnat_source_data_3 %>% count(DOMAIN) %>% filter(n == 1)
subnat_source_data_3 <- subnat_source_data_3 %>%
  filter(!(DOMAIN %in% one_psu_strata$DOMAIN))

#############################################################
# Set up survey design - filter out NAs from design columns
#############################################################
n_methods <- c("Pill", "Norplant/Implants", "Female Sterilization", "Injections", "IUD")

subnat_source_data_3 <- subnat_source_data_3 %>% 
  filter(is.na(PSU)==FALSE & is.na(method_region_country_year)==FALSE & is.na(sector)==FALSE & is.na(Method)==FALSE & is.na(DOMAIN)==FALSE & is.na(PERWEIGHT)==FALSE & is.na(YEAR)==FALSE)

IPUMSdesign <-svydesign(id= subnat_source_data_3$PSU, strata=subnat_source_data_3$DOMAIN, weights=subnat_source_data_3$PERWEIGHT, data=subnat_source_data_3, nest=TRUE)
options(survey.lonely.psu="adjust")

##################################
## Calculate SE  ---------------
##################################

folder <- "data/IPUMS/bivariate/"

method.sector <- as.data.frame(prop.table(svytable(~method_region_country_year +sector, IPUMSdesign), 1))
method.sector <- method.sector %>% separate(method_region_country_year, c("Method","Region", "Country", "Year"), "_") %>%
  rename(Subnat_Freq=Freq)

# Subset design for sectors
country_region_year_comb <- subnat_source_data_3 %>%
  group_by(Country, Region) %>%
  select(Country, Region, YEAR) %>%
  mutate(Country = as.character(Country),
         Region = as.character(Region)) %>%
  distinct() %>%
  filter(is.na(Region)==FALSE)

finished_entries <- vector() ## issue countries: 668, 669
for(i in 670:nrow(country_region_year_comb)) {
  print(i)
  country_code <- country_region_year_comb$Country[i]
  year <- country_region_year_comb$YEAR[i]
  region <- country_region_year_comb$Region[i]
  tmp <- subnat_source_data_3 %>% filter(Country==country_code & YEAR==year & Region==region) %>%
    mutate(across(where(is.factor), as.character))
  tmp$method_region <- as.factor(paste(tmp$Method, "_", tmp$Region, sep=""))
  tmp$method_sector <- as.factor(paste(tmp$Method, "_", tmp$sector, sep=""))
  tmpregion <- stringr::str_replace_all(region, "[[:punct:]]", " ")
  
  # Get proportions and var-covar
  tmpdesign<-svydesign(id= tmp$PSU, strata=tmp$DOMAIN, weights=tmp$PERWEIGHT, data=tmp, nest=TRUE)
  d.s <- update(tmpdesign, Method = factor(Method, levels = n_methods))
  d.s <- update(d.s, sector = factor(sector, levels = c('Private', 'Public')))

  counts <- tmp %>% 
    group_by(sector) %>% 
    count(method_region) %>% 
    separate(method_region, c("Method","Region"), "_")
  
  # Pivot counts for incorporation into data
  counts_wide <- counts %>% tidyr::pivot_wider(names_from='sector', values_from = 'n')
  colnames(counts_wide)[3:4] <- paste0(colnames(counts_wide)[3:4], '_n')
  
  tryCatch({ # Calculate  
    prop_mat <- svyby(~I(sector), ~I(Method), design=d.s, svymean, stringsAsFactors = TRUE, covmat=TRUE)
    vcov_matrix <- vcov(prop_mat)

    # Clean up proportion matrix
    colnames(prop_mat) <- gsub("I\\(sector\\)", "", colnames(prop_mat))
    colnames(prop_mat) <- gsub("I\\(Method\\)", "Method", colnames(prop_mat))
    prop_mat$Country <- country_code
    prop_mat$Region <- region
    prop_mat$Year <- year
    prop_mat <- merge(prop_mat, counts_wide)
    prop_mat <- prop_mat %>% mutate(Method = case_when(Method == "Injections" ~ "Injectables",
                                                       Method == "Norplant/Implants" ~ "Implants",
                                                       Method == "Pill" ~ "OC Pills",
                                                       .default = as.character(Method)))
    saveRDS(prop_mat, paste(folder, "/proportions/prop_", country_code,"_", tmpregion,"_", year, "_SEdf.RDS" , sep=""))
    
    # Clean up covariance matrix
    colnames(vcov_matrix) <- gsub("I\\(sector\\)", "", colnames(vcov_matrix))
    rownames(vcov_matrix) <- gsub("I\\(sector\\)", "", rownames(vcov_matrix))
    vcov_matrix <- as_tibble(vcov_matrix) %>% mutate(Method_sector = rownames(vcov_matrix))
    vcov_matrix$Country <- country_code
    vcov_matrix$Region <- region
    vcov_matrix$Year <- year
    saveRDS(vcov_matrix, paste(folder, "/varcov/varcov_", country_code,"_", tmpregion,"_", year, "_SEdf.RDS" , sep=""))
    finished_entries <- c(finished_entries, i)
    
    }, 
    error = function(e){
      prop_mat <- counts %>% mutate(Country = country_code,
                                    Year = year,
                                    Public = rep(1.0, nrow(counts)),
                                    se.Public = NA)
      
      saveRDS(prop_mat, paste(folder, "/proportions/prop_", country_code,"_", tmpregion,"_", year, "_SEdf.RDS" , sep=""))
      
      # Clean up covariance matrix
      combos <- paste0('Public:',counts$Method)
      tmpvcov_matrix <- tibble(Method_sector = combos,
                            Country = country_code, 
                            Region = region,
                            Year = year)
      tmpmat <- matrix(NA, nrow=length(combos), ncol=length(combos)) 
      colnames(tmpmat) <- combos
      vcov_matrix <- cbind(tmpvcov_matrix, as_tibble(tmpmat))
      
      saveRDS(vcov_matrix, paste(folder, "/varcov/varcov_", country_code,"_", tmpregion,"_", year, "_SEdf.RDS" , sep=""))
      })
}


  # 
  # # Calculate  
  # prop_mat <- svyby(~I(sector), ~I(Method), design=d.s, svymean, covmat=TRUE)
  # # prop_mat <- svymean(~Method, design=d.s, covmat=TRUE) # When only 1 sector is present
  # 
  # vcov_matrix <- vcov(prop_mat)
  # # prop_mat <- as.data.frame(prop_mat)
  # # 
  # # Clean up proportion matrix
  # colnames(prop_mat) <- gsub("I\\(sector\\)", "", colnames(prop_mat))
  # colnames(prop_mat) <- gsub("I\\(Method\\)", "Method", colnames(prop_mat))
  # 
  # # colnames(prop_mat) <- c('Public', 'se.Public') # When only 1 sector is present
  # # rownames(prop_mat) <- gsub("Method", "", rownames(prop_mat)) 
  # # prop_mat$Method <- rownames(prop_mat)
  #   
  # prop_mat$Country <- country_code
  # prop_mat$Region <- region
  # prop_mat$Year <- year
  # prop_mat <- merge(prop_mat, counts_wide)
  # prop_mat <- prop_mat %>% mutate(Method = case_when(Method == "Injections" ~ "Injectables",
  #                                                    Method == "Norplant/Implants" ~ "Implants",
  #                                                    Method == "Pill" ~ "OC Pills",
  #                                                    .default = as.character(Method)))
  # saveRDS(prop_mat, paste(folder, "/proportions/prop_", country_code,"_", region,"_", year, "_SEdf.RDS" , sep=""))
  # 
  # # Clean up covariance matrix
  # colnames(vcov_matrix) <- gsub("I\\(sector\\)", "", colnames(vcov_matrix))
  # rownames(vcov_matrix) <- gsub("I\\(sector\\)", "", rownames(vcov_matrix))
  # 
  # # colnames(vcov_matrix) <- gsub("Method", "Public:", colnames(vcov_matrix)) # When only 1 sector is present
  # # rownames(vcov_matrix) <- gsub("Method", "Public:", rownames(vcov_matrix))
  # 
  # vcov_matrix <- as_tibble(vcov_matrix) %>% mutate(Method_sector = rownames(vcov_matrix))
  # vcov_matrix$Country <- country_code
  # vcov_matrix$Region <- region
  # vcov_matrix$Year <- year
  # saveRDS(vcov_matrix, paste(folder, "/varcov/varcov_", country_code,"_", tmpregion,"_", year, "_SEdf.RDS" , sep=""))

# prop_mat <- svymean(~sector, design=d.s, covmat=TRUE) # When only 1 sector is present
# 
# vcov_matrix <- vcov(prop_mat)
# prop_mat <- as.data.frame(prop_mat)
# 
# # Clean up proportion matrix
# colnames(prop_mat) <- gsub("I\\(sector\\)", "", colnames(prop_mat))
# colnames(prop_mat) <- gsub("I\\(Method\\)", "Method", colnames(prop_mat))
# colnames(prop_mat) <- c('Public', 'se.Public') # When only 1 sector is present
# rownames(prop_mat) <- gsub("Method", "", rownames(prop_mat))
# prop_mat$Method <- rownames(prop_mat)
# prop_mat$Country <- country_code
# prop_mat$Region <- region
# prop_mat$Year <- year
# prop_mat <- merge(prop_mat, counts_wide)
# prop_mat <- prop_mat %>% mutate(Method = case_when(Method == "Injections" ~ "Injectables",
#                                                    Method == "Norplant/Implants" ~ "Implants",
#                                                    Method == "Pill" ~ "OC Pills",
#                                                    .default = as.character(Method)))
# colnames(vcov_matrix) <- gsub("I\\(sector\\)", "", colnames(vcov_matrix))
# rownames(vcov_matrix) <- gsub("I\\(sector\\)", "", rownames(vcov_matrix))
# colnames(vcov_matrix) <- gsub("Method", "Public:", colnames(vcov_matrix)) # When only 1 sector is present
# rownames(vcov_matrix) <- gsub("Method", "Public:", rownames(vcov_matrix))
# 
# vcov_matrix <- as_tibble(vcov_matrix) %>% mutate(Method_sector = rownames(vcov_matrix))
# vcov_matrix$Country <- country_code
# vcov_matrix$Region <- region
# vcov_matrix$Year <- year
  