library(tidyverse)
library(sf)

# Note 1. Obtaining the raw shape files for this calculation
###########################################################################################################################################
# In order to calculate the local neighbours using IPUMS data, you must register with IPUMS and download their GIS intergrated geodata.
# See this link for access to the data : https://www.idhsdata.org/idhs/gis1st.shtml
# In the code below, I store the IPUMS shapefiles that I download from the link above, in the location "data-raw/IPUMS/shapefiles/".
# These raw shapefiles are not found in the package due to distribution restrictions.
###########################################################################################################################################

# Note 2. Using IPUMS vs. geodata for boundaries
###########################################################################################################################################
# Due to the nature of integrated boundaries over time, some countries use will use the boundaries as per the IPUMS data.
# Other countries may use the geodata boundaries if they have more consistent boundaries over time.
# An example of how to calculate boundaries using geodata, can be found in get_local_neighbours_from_GEODATA.R in the data-raw/local_neighbours folder.
###########################################################################################################################################

countries <- c("Benin", "Burkina Faso",  "Cameroon", "Ethiopia", "Ghana", "Malawi", "Mali", "Mozambique", "Nepal", "Pakistan", "Zimbabwe",  "Senegal", "Uganda")

for(c in countries) {
  if(c %in% c("Senegal", "Uganda")) {
    sf::sf_use_s2(FALSE) } else {
      sf::sf_use_s2(TRUE)
    }

  # Get GEO data
  files <- list.files(paste0("data-raw/IPUMS/shapefiles/", tolower(c)))
  shp_file <- files[grep("*.shp", files)[1]] # take .shp file
  mapfilename <- paste0("data-raw/IPUMS/shapefiles/", tolower(c) ,"/",shp_file)
  tmp_geo <-  sf::st_read(mapfilename)
  tmp_regs <- unique(tmp_geo$ADMIN_NAME)
  tmp_country <- unique(tmp_geo$CNTRY_NAME)

  # Match names of GPS data to IPUMS GIS integrated names
  tmp_geo <- tmp_geo %>%
    dplyr::rowwise() %>%
    dplyr::filter(ADMIN_NAME != "Waterbodies") %>% # remove lakes from Ethiopia data
    dplyr::filter(ADMIN_NAME != "Waterbody") %>% # remove lakes from Tanzania
    dplyr::filter(ADMIN_NAME != "Unknown") %>% # remove unknown from Mali
    dplyr::mutate(ADMIN_NAME = case_when(CNTRY_NAME == "Benin" & ADMIN_NAME == "Zou and Collines" ~ "Zou and Collins", # Benin
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
                                  CNTRY_NAME == "Mali" & ADMIN_NAME %in% c("Sikasso", tmp_regs[2]) ~ "Sikasso and Ségou", # issue with UTF8 and region name
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
  tmp_geo <- tmp_geo[sf::st_is_valid(tmp_geo)==TRUE,] # remove problem geometries

  nc2 <- tmp_geo %>%
    dplyr::group_by(ADMIN_NAME) %>%
    dplyr::distinct(geometry) %>%
    dplyr::summarise(geometry = st_union(geometry))

  # Create neighbourhood adjancency matrix
  tmp_nb1 <- spdep::nb2mat(spdep::poly2nb(nc2), style="B")
  rownames(tmp_nb1) <- as.character(nc2$ADMIN_NAME)
  colnames(tmp_nb1) <- as.character(nc2$ADMIN_NAME)

  saveRDS(tmp_nb1, file=paste0("data-raw/local-neighbours/",c,"_neighbouradj.RDS"))
}
