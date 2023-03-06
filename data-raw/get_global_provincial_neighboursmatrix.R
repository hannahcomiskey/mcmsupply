library(tidyverse)
library(sf)
library(dplyr)
library(maptools)
library(raster)
library(ggplot2)
library(rgdal)

datalocation = "data/boundries/" # Change this to wherever you store your GPS data

#######################################################################
# Source: GADM Maps and Data R package
# https://gadm.org/about.html
#######################################################################

country_codes_DHS <- read_excel("data-raw/country_codes_DHS.xlsx")
FP_2030_countries <- c("Afghanistan","Benin","Burkina Faso","Cameroon",
                       "Congo", "Democratic Republic of the Congo", "Côte d'Ivoire",
                       "Ethiopia", "Ghana","Guinea","India","Kenya", "Liberia", "Madagascar",
                       "Malawi","Mali", "Mozambique", "Myanmar", "Nepal", "Niger", "Nigeria", "Pakistan",
                       "Philippines", "Rwanda", "Senegal", "Sierra Leone", "Togo", "Tanzania", "Uganda", "Zimbabwe")

country_codes <- country_codes_DHS %>%
  dplyr::filter(`Country Name` %in% FP_2030_countries) %>%
  dplyr::select(Code) %>%
  unlist() %>%
  as.vector()

# create GADM GEO data list
subnat_geodata <- list()
subnat_geodata[FP_2030_countries[1]] <- raster::getData("GADM", path=datalocation, country=FP_2030_countries[1], level=1)
for(i in 2:length(FP_2030_countries)) {
  tmp_geodata <- raster::getData("GADM", path=datalocation, country=FP_2030_countries[i], level=1)
  subnat_geodata[FP_2030_countries[i]] <- tmp_geodata
}

saveRDS(subnat_geodata, file=paste0(datalocation,"subnat_GADMgeodata.RDS"))

# ---------------------------------------------------------------------------------------------------------------

##################################################################################
# Source: IPUMS GPS data.
# You will need to create an IPUMS account, get the GIS integrated data and store them in a location, eg: data/IPUMS/shapefiles/
# https://www.idhsdata.org/idhs/geog_tools.shtml
#################################################################################

IPUMSlocation = "data/IPUMS/shapefiles/" # Change this to wherever you store your IPUMS data

countries <- c("Burkina Faso",
               "Cameroon",
               "Ethiopia",
               "Ghana",
               "Guinea",
               "India",
               "Liberia",
               "Malawi",
               "Mali",
               "Mozambique",
               "Nepal",
               "Nigeria",
               "Pakistan",
               "Rwanda",
               "Senegal",
               "Tanzania",
               "Uganda",
               "Zimbabwe")

# Start with Benin, then iteratively add to this dataframe in a loop underneath -------------------

c=="Benin"
# Get GEO data
files <- list.files(paste0(IPUMSlocation, tolower(c))) # lowercase file names
shp_file <- files[grep("*.shp", files)[1]] # take .shp file
mapfilename <-paste0(IPUMSlocation, tolower(c) ,"/",shp_file)
tmp_geo1 <-  sf::st_read(mapfilename) # read file
tmp_regs1 <- unique(tmp_geo1$ADMIN_NAME) # Admin 1 province names
tmp_country1 <- unique(tmp_geo1$CNTRY_NAME) # Country name

# Match names of GPS data to IPUMS GIS integrated names
tmp_geo1 <- tmp_geo1 %>%
  rowwise() %>%
  filter(ADMIN_NAME != "Waterbodies") %>% # remove lakes from Ethiopia data
  filter(ADMIN_NAME != "Waterbody") %>% # remove lakes from Tanzania
  filter(ADMIN_NAME != "Unknown") %>% # remove unknown from Mali
  mutate(ADMIN_NAME = case_when(CNTRY_NAME == "Benin" & ADMIN_NAME == "Zou and Collines" ~ "Zou and Collins", # Benin
                                TRUE ~ as.character(ADMIN_NAME)))

# Update geometry of merged regions
# https://gis.stackexchange.com/questions/321281/using-sf-to-combine-polygons-that-share-borders
tmp_geo1 <- tmp_geo1[sf::st_is_valid(tmp_geo1)==TRUE,] # remove problem geometries

nc1 <- tmp_geo1 %>%
  group_by(ADMIN_NAME) %>%
  dplyr::distinct(geometry) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  mutate(Country=tmp_country1)

for(c in countries) { # Reepat process for all other countries
  if(c %in% c("Senegal", "Uganda")) {
    sf_use_s2(FALSE) } else {
      sf_use_s2(TRUE)
    }

  # Get GEO data
  files <- list.files(paste0(location, tolower(c)))
  shp_file <- files[grep("*.shp", files)[1]] # take .shp file
  mapfilename <-paste0(IPUMSlocation, tolower(c) ,"/",shp_file)
  tmp_geo2 <-  sf::st_read(mapfilename)
  tmp_regs2 <- unique(tmp_geo2$ADMIN_NAME)
  tmp_country2 <- unique(tmp_geo2$CNTRY_NAME)

  # Match names of GPS data to IPUMS GIS integrated names
  tmp_geo2 <- tmp_geo2 %>%
    rowwise() %>%
    filter(ADMIN_NAME != "Waterbodies") %>% # remove lakes from Ethiopia data
    filter(ADMIN_NAME != "Waterbody") %>% # remove lakes from Tanzania
    filter(ADMIN_NAME != "Unknown") %>% # remove unknown from Mali
    mutate(ADMIN_NAME = case_when(CNTRY_NAME == "Benin" & ADMIN_NAME == "Zou and Collines" ~ "Zou and Collins", # Benin
                                  CNTRY_NAME == "Burkina Faso" & ADMIN_NAME == "Boucle du Mouhoun" ~ "Boucle de Mouhoun",       # Burkina Faso
                                  CNTRY_NAME == "Burkina Faso" & ADMIN_NAME == "Centre-Sud" ~ "Centre including Ouagadougou",
                                  CNTRY_NAME == "Burkina Faso" & ADMIN_NAME == "Centre-Nord"  ~ "North",
                                  CNTRY_NAME == "Burkina Faso" & ADMIN_NAME == "Hauts-Bassins" ~ "Hauts Basins",
                                  CNTRY_NAME == "Burkina Faso" & ADMIN_NAME == "Est" ~ "West",
                                  CNTRY_NAME == "Burkina Faso" & ADMIN_NAME == "Centre" ~ "Ouagadougou",
                                  CNTRY_NAME == "Cameroon" & ADMIN_NAME == "Nord, Adamoua, Extr\xe8me Nord" ~ "Adamaoua, Nord, and Extrême-Nord",
                                  CNTRY_NAME == "Cameroon" & ADMIN_NAME %in% c("Centre, Sud", "Est", "Ouest", "Littoral\r\n") ~ "Centre, Sud, Est, Ouest, Littoral, Yaoundé, Douala",
                                  CNTRY_NAME == "Cameroon" & ADMIN_NAME %in% c("Sud Ouest", "Nord Ouest") ~ "Nord-Ouest and Sud-Ouest",
                                  CNTRY_NAME == "Ethiopia" & ADMIN_NAME == "Oromia, Somali" ~ "Oromia",
                                  CNTRY_NAME == "Ethiopia" & ADMIN_NAME == "Southern Nations, Nationalities, and Peoples (SNNP)" ~ "Southern Nations, Nationalities and Peoples",
                                  CNTRY_NAME == "Ghana" & ADMIN_NAME == "Brong Ahafo" ~ "Brong-Ahafo",  # Ghana
                                  CNTRY_NAME == "Ghana" & ADMIN_NAME %in% c("Northern","Upper East","Upper West") ~ "Upper W,E and Northern",  # Ghana
                                  CNTRY_NAME == "Guinea" & ADMIN_NAME %in% c("Boke", "Kindia", "Labe", "Mamou") ~ "Boké, Kindia, Labé, Mamou",  # Guinea
                                  CNTRY_NAME == "Guinea" & ADMIN_NAME %in% c("Faranah","Kankan","Nzerekore") ~ "Faranah, Kankan, and N'Zérékoré",
                                  CNTRY_NAME == "India" & ADMIN_NAME == "Bihar, Jharkhand" ~ "Bihar and Jharkhand",
                                  CNTRY_NAME == "Liberia" & ADMIN_NAME %in% c("Bomi, Bong, Grand Bassa, Margibi, Montserrado", "Nimba", "Lofa") ~ "North Central",  # Liberia
                                  CNTRY_NAME == "Liberia" & ADMIN_NAME == "Grad Cape Mount" ~ "North Western",
                                  CNTRY_NAME == "Liberia" & ADMIN_NAME == "Grand Gedeh, Rivercess, Sinoe" ~ "South Eastern A",
                                  CNTRY_NAME == "Liberia" & ADMIN_NAME == "Grand Kru, Maryland, River Gee" ~ "South Eastern B",
                                  CNTRY_NAME == "Malawi" & ADMIN_NAME %in% c("Dedza", "Dowa", "Kasungu", "Lilongwe, Lilongwe City", "Mchinji", "Nkhotakota", "Ntchisi", "Salima", "Ntcheu") ~ "Central", # Malawi
                                  CNTRY_NAME == "Malawi" & ADMIN_NAME %in% c("Chitipa", "Karonga", "Likoma", "Mzimba", "Nkhata Bay", "Rumphi", "Mzimba, Mzuzu City", "Nkhata Bay, Likoma") ~ "Northern",
                                  CNTRY_NAME == "Malawi" & ADMIN_NAME %in% c("Balaka", "Blantyre, Blantyre City", "Chikwawa","Chiradzulu","Machinga","Mangochi","Mulanje","Mwanza, Neno", "Nsanje","Thyolo","Phalombe","Zomba, Zomba City","Neno") ~ "Southern",
                                  CNTRY_NAME == "Mali" & ADMIN_NAME %in% c("Sikasso", tmp_regs2[2]) ~ "Sikasso and Ségou", # issue with UTF8 and region name
                                  CNTRY_NAME == "Mali" & ADMIN_NAME %in% c("Mopti", "Tombouctou", "Gao, Kidal") ~ "Mopti, Tombouctou, Gao and Kidal",
                                  CNTRY_NAME == "Mali" & ADMIN_NAME %in% c("Kayes", "Koulikoro") ~ "Kayes and Koulikoro",
                                  CNTRY_NAME == "Mozambique" & ADMIN_NAME=="Maputo city" ~ "City of Maputo", # Mozambique
                                  CNTRY_NAME == "Mozambique" & ADMIN_NAME=="Maputo province" ~ "Maputo Provincia",
                                  CNTRY_NAME == "Nepal" & ADMIN_NAME %in% c("Mahakali", "Seti") ~ "Far-western", # Nepal
                                  CNTRY_NAME == "Nepal" & ADMIN_NAME %in% c("Karnali", "Bheri", "Rapti") ~ "Mid-western",
                                  CNTRY_NAME == "Nepal" & ADMIN_NAME %in% c("Dhawalagiri", "Lumbini", "Gandaki") ~ "Western",
                                  CNTRY_NAME == "Nepal" & ADMIN_NAME %in% c("Narayani", "Bagmati", "Janakpur") ~ "Central",
                                  CNTRY_NAME == "Nepal" & ADMIN_NAME %in% c("Sagarmatha", "Koshi", "Mechi") ~ "Eastern",
                                  CNTRY_NAME == "Nigeria" & ADMIN_NAME %in% c("Kwara", "Federal Capital Territory Abuja", "Benue", "Kogi", "Nasarawa", "Niger", "Plateau") ~ "North Central",
                                  CNTRY_NAME == "Nigeria" & ADMIN_NAME %in% c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe") ~ "North East",
                                  CNTRY_NAME == "Nigeria" & ADMIN_NAME %in% c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara") ~ "North West",
                                  CNTRY_NAME == "Nigeria" & ADMIN_NAME %in% c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo") ~ "South East",
                                  CNTRY_NAME == "Nigeria" & ADMIN_NAME %in% c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers") ~ "South South",
                                  CNTRY_NAME == "Nigeria" & ADMIN_NAME %in% c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo") ~ "South West",
                                  CNTRY_NAME == "Pakistan" & ADMIN_NAME == "Fata" ~ "Federally Administered Tribal Areas (FATA)" , # Pakistan
                                  CNTRY_NAME == "Pakistan" & ADMIN_NAME == "Sind" ~ "Sindh" ,
                                  CNTRY_NAME == "Pakistan" & ADMIN_NAME == "Sind" ~ "Sindhh",
                                  CNTRY_NAME == "Pakistan" & ADMIN_NAME ==  "Punjab, Islamabad" ~ "Punjab (includes Islamabad Capital Territory)" ,
                                  CNTRY_NAME == "Pakistan" & ADMIN_NAME == "Baluchistan" ~ "Balochistan",
                                  CNTRY_NAME == "Pakistan" & ADMIN_NAME == "Northern areas" ~ "Gilgit Baltistan",
                                  CNTRY_NAME == "Pakistan" & ADMIN_NAME == "North-West Frontier Province" ~ "Khyber Pakhtunkhwa",
                                  CNTRY_NAME == "Rwanda" & ADMIN_NAME == "Byumba, Kigali Ngali, Kigali Ville, Kibungo, Umutara" ~ "Kigali", # Rwanda
                                  CNTRY_NAME == "Rwanda" & ADMIN_NAME %in% c("Gitarama", "Butare", "Gikongoro") ~ "South",
                                  CNTRY_NAME == "Rwanda" & ADMIN_NAME %in% c("Cyangugu", "Kibuye", "Gisenyi") ~ "West",
                                  CNTRY_NAME == "Rwanda" & ADMIN_NAME %in% c("Ruhengeri") ~ "North",
                                  CNTRY_NAME == "Senegal" & ADMIN_NAME %in% c("Dakar", "Thies") ~ "Dakar, Thiès (West)",
                                  CNTRY_NAME == "Senegal" & ADMIN_NAME %in% c("Kaolack, Fatick, Kaffrine", "Diourbel") ~ "Diourbel, Fatick, Kaolack, Louga, Kaffrine (Center)",
                                  CNTRY_NAME == "Senegal" & ADMIN_NAME %in% c("Saint Louis, Louga, Matam", "Tambacounda, Kedougou") ~ "Saint-Louis, Tambacounda, Matam, Kedougou (North East)",
                                  CNTRY_NAME == "Senegal" & ADMIN_NAME %in% c("Kolda, Sedhiou", "Ziguinchor") ~ "Kolda, Ziguinchor, Sedhiou (South)",
                                  CNTRY_NAME == "Tanzania" & ADMIN_NAME %in% c("Arusha, Manyara", "Kilimanjaro") ~ "Arusha, Kilimanjaro, and Manyara (Northern Highlands)",
                                  CNTRY_NAME == "Tanzania" & ADMIN_NAME %in% c("Dar es Salaam", "Morogoro", "Pemba North", "Pemba South", "Pwani", "Tanga", "Zanzibar Town/West", "Zanzibar North", "Zanzibar South") ~ "Dar Es Salaam, Morogoro, Pemba, Pwani, Tanga, Town West, and Zanzibar (Coastal)",
                                  CNTRY_NAME == "Tanzania" & ADMIN_NAME %in% c("Dodoma", "Singida") ~ "Dodoma and Singida (Central)",
                                  CNTRY_NAME == "Tanzania" & ADMIN_NAME %in% c("Geita, Kagera, Mwanza, Shinyanga, Simiyu","Tabora", "Mara", "Kigoma") ~ "Geita, Kagera, Kigoma, Mara, Mwanza, Shinyanga, Simiyu, Tabora (Lake)",
                                  CNTRY_NAME == "Tanzania" & ADMIN_NAME %in% c("Iringa, Njombe", "Mbeya", "Katavi, Rukwa") ~ "Iringa, Myeba, Nijombe, Katavi, and Rukwa (Southern Highlands)",
                                  CNTRY_NAME == "Tanzania" & ADMIN_NAME %in% c("Lindi", "Mtwara","Ruvuma") ~ "Lindi, Mtwara, and Ruvuma (South)",
                                  CNTRY_NAME == "Uganda" & ADMIN_NAME %in% c("Kamuli, Kaliro, Buyende", "Jinja", "Bugiri, Iganga, Mayuge, Namutumba, Luuka, Namayingo", "Busia, Tororo, Butaleja","Pallisa, Budaka, Kibuku","Mbale, Sironko, Bududa, Manafwa, Bulambuli", "Katakwi, Soroti, Kaberamaido, Amuria, Serere","Kumi, Bukedea, Ngora") ~ "Bugosa, Bukedi, Bugishu, Teso",
                                  CNTRY_NAME == "Uganda" & ADMIN_NAME %in% c("Hoima", "Masindi, Buliisa, Kiryandongo", "Kibaale, Kagadi, Kakumiro", "Kabarole, Kamwenge, Kyenjojo, Kyegegwa", "Bushenyi, Mbarara, Ntungamo, Ibanda, Isingiro, Kiruhura, Buhweju, Mitooma, Rubirizi, Sheema","Kabale, Rubanda" ,"Kisoro","Rukungiri, Kanungu") ~ "Bunyoro, Tooro, Ankole, Kigezi",
                                  CNTRY_NAME == "Uganda" & ADMIN_NAME %in% c("Kampala", "Mukono, Kayunga, Buikwe, Buvuma", "Masaka, Ssembabule, Bukomansimbi, Kalungu, Lwengo", "Mpigi, Wakiso, Butambala, Gomba","Mukono, Kayunga, Buikwe, Buvuma","Kalangala", "Kiboga, Kyankwanzi" ,"Luwero, Nakasongola, Nakaseke","Rakai, Lyantonde","Mubende, Mityana") ~ "Central 1, Central 2, and Kampala",
                                  CNTRY_NAME == "Uganda" & ADMIN_NAME %in% c("Lira, Amolatar, Dokolo, Alebtong, Otuke","Apac, Oyam, Kole", "Kitgum, Pader, Agago, Lamwo","Gulu, Amuru, Nwoya, Omoro", "Kotido, Moroto, Nakapiripirit, Abim, Kaabong, Amudat, Napak", "Arua, Yumbe, Koboko, Maracha", "Adjumani, Moyo","Nebbi, Zombo") ~ "Lango, Acholi, Karamoja, West Nile",
                                  TRUE ~ as.character(ADMIN_NAME)))

  # Update geometry of merged regions
  # https://gis.stackexchange.com/questions/321281/using-sf-to-combine-polygons-that-share-borders
  tmp_geo2 <- tmp_geo2[sf::st_is_valid(tmp_geo2)==TRUE,] # remove problem geometries

  nc2 <- tmp_geo2 %>%
    group_by(ADMIN_NAME) %>%
    dplyr::distinct(geometry) %>%
    dplyr::summarise(geometry = st_union(geometry)) %>%
    mutate(Country=tmp_country2)

  if(c=="Cameroon") {
    nc2$ADMIN_NAME[2] <- "Nord, Adamoua, Extreme Nord" # replace issue with UTF8 character
  }

  nc1 <- bind_rows(nc1, nc2)
}

saveRDS(nc1, file=paste0(datalocation,"global_provincial_sf.RDS"))

# Names of regions and countries we have in the data
#country_region_order <- readRDS("~/PhD/subnational_estimates/country_region_order.RDS")

nc1 <- readRDS(paste0(datalocation,"global_provincial_sf.RDS")) %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  filter(!Country %in% c("Tanzania","Madagascar", "Niger", "Rwanda", "Liberia", "Nigeria")) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME))

subnat_geodata <- readRDS(paste0(datalocation,"subnat_GADMgeodata.RDS"))

# Load GADM data for countries missing from IPUMS dataset ----------------------
afg_gadm <- subnat_geodata$Afghanistan %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME=="Hilmand" ~ "Helmand",
                                       ADMIN_NAME=="Hirat" ~ "Herat",
                                       ADMIN_NAME=="Kunar" ~ "Kunarha",
                                       ADMIN_NAME=="Panjshir" ~ "Panjsher",
                                       ADMIN_NAME == "Sari Pul" ~ "Sar-E-Pul",
                                       ADMIN_NAME == "Uruzgan" ~ "Urozgan",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  dplyr::mutate(Country_region = paste0(Country, "_", ADMIN_NAME))

mdg_gadm <- subnat_geodata$Madagascar %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME == "Antananarivo" ~ "Analamanga, Vakinankaratra, Itasy, Bongolava (Antananarivo)",
                                       ADMIN_NAME == "Toliary" ~ "Atsimo Andrefana, Androy, Anosy, Menabe (Toliary)",
                                       ADMIN_NAME == "Toamasina" ~ "Atsinanana, Analanjirofo, Alaotra Mangoro (Toamasina)",
                                       ADMIN_NAME == "Mahajanga"  ~ "Boeny, Sofia, Betsiboka, Melaky (Mahajanga)",
                                       ADMIN_NAME ==  "Antsiranana" ~ "Diana, Sava (Antsiranana)",
                                       ADMIN_NAME == "Fianarantsoa" ~ "Haute Matsiatra, Anamoroni'i Mania, Vatovavy Fitovinany, Ihorombe, Atsimo Atsinanana (Fianarantsoa)"))

tnz_gadm <- subnat_geodata$Tanzania %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Arusha", "Manyara", "Kilimanjaro") ~ "Arusha, Kilimanjaro, and Manyara (Northern Highlands)",
                                       ADMIN_NAME %in% c("Dar es Salaam", "Morogoro", "Pemba North", "Pemba South", "Pwani", "Tanga", "Zanzibar West", "Zanzibar North", "Zanzibar South and Central") ~ "Dar Es Salaam, Morogoro, Pemba, Pwani, Tanga, Town West, and Zanzibar (Coastal)",
                                       ADMIN_NAME %in% c("Dodoma", "Singida") ~ "Dodoma and Singida (Central)",
                                       ADMIN_NAME %in% c("Geita", "Kagera", "Mwanza", "Shinyanga", "Simiyu", "Tabora", "Mara", "Kigoma") ~ "Geita, Kagera, Kigoma, Mara, Mwanza, Shinyanga, Simiyu, Tabora (Lake)",
                                       ADMIN_NAME %in% c("Iringa", "Njombe", "Mbeya", "Katavi", "Rukwa") ~ "Iringa, Myeba, Nijombe, Katavi, and Rukwa (Southern Highlands)",
                                       ADMIN_NAME %in% c("Lindi", "Mtwara","Ruvuma") ~ "Lindi, Mtwara, and Ruvuma (South)")) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country="Tanzania")

ngr_gadm <- subnat_geodata$Niger %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME=="Dosso" ~ "Dossa",
                                     ADMIN_NAME %in% c("Tahoua", "Agadez") ~ "Tahoua/Agadez",
                                     ADMIN_NAME=="Tillabéry" ~ "Tillaberi",
                                     ADMIN_NAME %in% c("Zinder", "Diffa") ~ "Zinder/Diffa",
                                     TRUE ~ as.character(ADMIN_NAME))) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Niger")

rwd_gadm <- subnat_geodata$Rwanda %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, VARNAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME=="Amajyaruguru" ~ "North",
                                       ADMIN_NAME=="Amajyepfo" ~ "South",
                                       ADMIN_NAME=="Iburasirazuba" ~ "East",
                                       ADMIN_NAME=="Iburengerazuba" ~ "West",
                                       ADMIN_NAME=="Umujyi wa Kigali" ~ "Kigali",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Rwanda")

lbr_gadm <- subnat_geodata$Liberia %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, VARNAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Bomi", "Bong", "GrandBassa", "Margibi", "Montserrado", "Nimba", "Lofa") ~ "North Central",  # Liberia
                                       ADMIN_NAME %in% c("Grad Cape Mount") ~ "North Western",
                                       ADMIN_NAME %in% c( "GrandGedeh", "River Cess", "Sinoe") ~ "South Eastern A",
                                       ADMIN_NAME %in% c("GrandKru", "Maryland", "River Gee") ~ "South Eastern B",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Liberia")

nga_gadm <- subnat_geodata$Nigeria %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, VARNAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Kwara", "Federal Capital Territory", "Benue", "Kogi", "Nasarawa", "Niger", "Plateau") ~ "North Central",
                                       ADMIN_NAME %in% c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe") ~ "North East",
                                       ADMIN_NAME %in% c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara") ~ "North West",
                                       ADMIN_NAME %in% c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo") ~ "South East",
                                       ADMIN_NAME %in% c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers") ~ "South South",
                                       ADMIN_NAME %in% c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo") ~ "South West",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Nigeria")

nc2 <- bind_rows(nc1,afg_gadm)
nc2 <- bind_rows(nc2,mdg_gadm)
nc2 <- bind_rows(nc2,tnz_gadm)
nc2 <- bind_rows(nc2,ngr_gadm)
nc2 <- bind_rows(nc2,rwd_gadm)
nc2 <- bind_rows(nc2,lbr_gadm)
nc2 <- bind_rows(nc2,nga_gadm)

# Split multipolygons into polygons -------------
nc3 <- sf::st_cast(nc2, "POLYGON", do_split = FALSE)

nc3 <- nc3 %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(!Country_region)

# Fix UTF-8 characters
nc3$ADMIN_NAME[which(nc3$ADMIN_NAME=="Cameroon_Nord, Adamoua, Extreme Nord")] <- "Cameroon_Adamaoua, Nord, and Extrême-Nord"
nc3$ADMIN_NAME[grep("Mozambique_Zamb", nc3$ADMIN_NAME)] <- "Mozambique_Zambezia"

roworder <- paste0(nc3$Country, "_", nc3$ADMIN_NAME)

#country_region_order <- paste0(index_country_subnat_tbl$Country, "_", index_country_subnat_tbl$Region)

country_region_order <- readRDS("country_region_order.RDS") # all FP2030 countries
country_region_order <- readRDS("country_region_order_LOO.RDS") # out-of-sample validation countries

# Keep cols that we have data for in both GPS and FP source
if(length(setdiff(country_region_order, roworder))==0) {
  keepcols <- country_region_order
} else {
  keepcols <- country_region_order[-which(country_region_order %in% setdiff(country_region_order, roworder))]
}

nc3 <- nc3 %>%
  dplyr::mutate(Country_province = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::filter(Country_province %in% keepcols) %>%
  dplyr::arrange(Country_province, by=keepcols)

# Get neighbourhood adjacency matrix
coords = st_coordinates(st_centroid(st_geometry(nc3)))
queen_nb = spdep::poly2nb(nc3, row.names=nc3$ADM1NAME, queen=TRUE)

# Create neighbourhood adjancency matrix
mcadd = mstconnect(nc3,queen_nb)

plot(st_geometry(nc3), border = "grey")
plot(queen_nb, coords, add = T, col="blue",lwd=2)
plot(mcadd, coords, add = T, lwd=2, col="red")

tmp_nb1 <- spdep::nb2mat(mcadd, style="B")
rownames(tmp_nb1) <- paste0(nc3$Country,"_",nc3$ADMIN_NAME)
colnames(tmp_nb1) <- paste0(nc3$Country,"_",nc3$ADMIN_NAME)
saveRDS(tmp_nb1, file=paste0(datalocation, "global_provincial_neighbouradj.RDS"))

##################################################################################
# Get Kenya neighbours for local run -----------------------------------------
#################################################################################

subnat_geodata <- readRDS(paste0(datalocation, "subnat_geodata.RDS"))

# Load GADM data for missing countries
kenya_gadm <- subnat_geodata$Kenya %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME))

# Get neighbourhood adjacency matrix
coords = st_coordinates(st_centroid(st_geometry(kenya_gadm)))
queen_nb = spdep::poly2nb(kenya_gadm, row.names=kenya_gadm$ADM1NAME, queen=TRUE)

# Create neighbourhood adjancency matrix
mcadd = mstconnect(kenya_gadm,queen_nb)

plot(st_geometry(kenya_gadm), border = "grey")
plot(queen_nb, coords, add = T, col="blue",lwd=2)
plot(mcadd, coords, add = T, lwd=2, col="red")

tmp_nb1 <- spdep::nb2mat(mcadd, style="B")
rownames(tmp_nb1) <- kenya_gadm$ADMIN_NAME
colnames(tmp_nb1) <- kenya_gadm$ADMIN_NAME
saveRDS(tmp_nb1, file=paste0(datalocation, "kenya_admin2_neighbouradj_GADM.RDS"))

