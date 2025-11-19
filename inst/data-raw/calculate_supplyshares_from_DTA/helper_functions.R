# Standard naming
standard_method_names <- function(my_data) {
  #levels(my_data$Method) <- c(levels(my_data$Method), "Condom", "Female Sterilization", "Male Sterilization", "Other Modern Methods", "OC Pills")
  my_data <- my_data %>%
    mutate(Method = replace(Method, Method  %in% c("condom", "male condom", "Condom (m+f)"), "Condom")) %>%
    #mutate(Method = replace(Method, Method == "female condom", "Condom")) %>%
    mutate(Method = replace(Method, Method %in% c("Female sterilization", "female sterilization", "Sterilization (female)"), "Female Sterilization")) %>%
    mutate(Method = replace(Method, Method %in% c("Sterilization (male)", "male sterilization", "Male sterilization"), "Male Sterilization")) %>%
    mutate(Method = replace(Method, Method %in% c("pill", "Pill"), "OC Pills")) %>%
    mutate(Method = replace(Method, Method %in% c("injections", "Injections"), "Injectables")) %>%
    mutate(Method = replace(Method, Method %in% c("implants", "implant", "Implant"), "Implants")) %>%
    mutate(Method = replace(Method, Method %in% c("Other", "Other Modern Methods", "LAM", "diaphragm", "female condom", "foam or jelly", "standard days method", "diaphragm, foam or jelly", "lactational amenorrhea", "emergency contraception", "other modern methods"), "Other Modern Methods")) #%>%
  #as.data.frame()
  return(my_data)
}

### Calculating country_index 
country_index_fun <- function(my_data, my_countries) {
  for (i in 1:length(my_countries)) {
    for (j in 1:nrow(my_data)) {
      country_name <- my_countries[i]
      if(my_data$Country[j]==country_name) {
        my_data$index_country[j] <- i
      } 
    }
  }
  return(my_data)
}

### Calculating region_index 
region_index_fun <- function(my_data, n_region) {
  my_data$index_region <- NA
  for (i in 1:length(n_region)) {
    for (j in 1:nrow(my_data)) {
      region_name <- n_region[i]
      if(my_data$Region[j]==region_name) {
        my_data$index_region[j] <- i
      } 
    }
  }
  return(my_data)
}

### Calculating method_index 
method_index_fun <- function(my_data, my_methods) {
  my_data$index_method <- rep(NA, nrow(my_data))
  for (i in 1:length(my_methods)) {
    for (j in 1:nrow(my_data)) {
      method_name <- my_methods[i]
      if(my_data$Method[j]==method_name) {
        my_data$index_method[j] <- i
      } 
    }
  }
  return(my_data)
}

### Calculating sector_index 
sector_index_fun <- function(my_data, my_sectors) {
  for (i in 1:length(my_sectors)) {
    for (j in 1:nrow(my_data)) {
      sector_name <- my_sectors[i]
      if(my_data$sector_category[j]==sector_name) {
        my_data$index_sector[j] <- i
      } 
    }
  }
  return(my_data)
}

#### Collapsing Some Methods into 'Others' category
collapse_methods_fun <- function(my_data) {
  my_data$Method <- as.vector(sapply(my_data[,'Method'], FUN = function(x)
    ifelse(x=="Implants","Implants", 
           ifelse(x=="Injectables","Injectables",
                  ifelse(x=="IUD","IUD",
                         ifelse(x %in% c("Female Sterilization","Female sterilization"),"Female sterilization",
                                ifelse(x %in% c("Male Sterilization","Male sterilization"),"Male sterilization",
                                       ifelse(x %in% c("OC Pills", "Pill"),"Pill",
                                              ifelse(x=="Condom","Condom", "Others"
                                              )))))))) )
  
  return(my_data)
}

# Function to create universal df of estimates -------------------
create_subnational_df <- function(myfile_names, mydf.list, mycountry_codes, mycountry_codes_DHS){
  updated.df <- tibble()
  for(i in 1:length(myfile_names)) {
    countrycode <- names(mydf.list[i])
    countrycode <- substr(countrycode, 1,2)
    mydata <- tibble(mydf.list[[i]])
    mydata <- mydata %>% mutate(country_code = mycountry_codes[i])
    if(countrycode=="IA") {
      country_name <- "India" } else {
        if(countrycode=="PG") {
          country_name <- "Papua New Guinea" } else {
            country_name <- mycountry_codes_DHS %>% filter(Code==mycountry_codes[i]) %>% dplyr::select(`Country Name`) }
      }
    country_name <- as.vector(unlist(country_name))
    mydata <- mydata %>% 
      mutate(Country = country_name) %>% 
      rename(Method=method) 
    updated.df <- bind_rows(mydata, updated.df)
  }
  
  updated.df <- updated.df %>% select(Country, Method, year, country_code, Public, se.Public, Public_n, Commercial_medical, se.Commercial_medical, Commercial_medical_n, Other, se.Other, Other_n)
  
  return(updated.df)
}