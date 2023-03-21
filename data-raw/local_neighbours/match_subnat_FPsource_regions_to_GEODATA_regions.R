library(tidyverse)
library(sf)

########################################################
# USING GADM DATA ----------------------
########################################################

subnat_geodata <- readRDS("data-raw/local_neighbours/subnat_geodata.RDS") # All the countries GEODATA in one dataframe

# Load GADM data for missing countries
afg_gadm <- subnat_geodata$Afghanistan %>%
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME=="Hilmand" ~ "Helmand",
                                       ADMIN_NAME=="Hirat" ~ "Herat",
                                       ADMIN_NAME=="Kunar" ~ "Kunarha",
                                       ADMIN_NAME=="Panjshir" ~ "Panjsher",
                                       ADMIN_NAME == "Sari Pul" ~ "Sar-E-Pul",
                                       ADMIN_NAME == "Uruzgan" ~ "Urozgan",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME))

mdg_gadm <- subnat_geodata$Madagascar %>%
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME == "Antananarivo" ~ "Analamanga, Vakinankaratra, Itasy, Bongolava (Antananarivo)",
                                       ADMIN_NAME == "Toliary" ~ "Atsimo Andrefana, Androy, Anosy, Menabe (Toliary)",
                                       ADMIN_NAME == "Toamasina" ~ "Atsinanana, Analanjirofo, Alaotra Mangoro (Toamasina)",
                                       ADMIN_NAME == "Mahajanga"  ~ "Boeny, Sofia, Betsiboka, Melaky (Mahajanga)",
                                       ADMIN_NAME ==  "Antsiranana" ~ "Diana, Sava (Antsiranana)",
                                       ADMIN_NAME == "Fianarantsoa" ~ "Haute Matsiatra, Anamoroni'i Mania, Vatovavy Fitovinany, Ihorombe, Atsimo Atsinanana (Fianarantsoa)"))

tnz_gadm <- subnat_geodata$Tanzania %>%
  st_as_sf() %>%
  st_make_valid() %>%
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
  st_as_sf() %>%
  st_make_valid() %>%
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
  st_as_sf() %>%
  st_make_valid() %>%
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
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, VARNAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Bong", "Nimba", "Lofa") ~ "North Central",
                                       ADMIN_NAME %in% c("GrandBassa", "Margibi", "Montserrado") ~ "South Central, Monrovia", # Liberia
                                       ADMIN_NAME %in% c("Grand Cape Mount", "Bomi", "Gbapolu") ~ "North Western",
                                       ADMIN_NAME %in% c( "GrandGedeh", "River Cess", "Sinoe") ~ "South Eastern A",
                                       ADMIN_NAME %in% c("GrandKru", "Maryland", "River Gee") ~ "South Eastern B",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Liberia")

nga_gadm <- subnat_geodata$Nigeria %>%
  st_as_sf() %>%
  st_make_valid() %>%
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

india_gadm <- subnat_geodata$India %>%
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, VARNAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Andaman and Nicobar", "Telangana", "Andhra Pradesh") ~ "Andhra Pradesh, Telangana, Andaman and Nicobar Islands",
                                       ADMIN_NAME %in% c("Bihar", "Jharkhand") ~ "Bihar and Jharkhand",
                                       ADMIN_NAME =="NCT of Delhi" ~ "Delhi",
                                       ADMIN_NAME %in% c("Gujarat", "Dadra and Nagar Haveli", "Daman and Diu") ~ "Gujarat, Dadra and Nagar Haveli, Daman and Diu",
                                       ADMIN_NAME %in% c("Kerala", "Lakshadweep") ~ "Kerala and Lakshadweep",
                                       ADMIN_NAME %in% c("Madhya Pradesh", "Chhattisgarh") ~ "Madhya Pradesh and Chhattisgarh",
                                       ADMIN_NAME %in% c("Punjab", "Chandigarh") ~ "Punjab and Chandigarh",
                                       ADMIN_NAME %in% c("Uttar Pradesh", "Uttarakhand") ~ "Uttar Pradesh and Uttarakhand",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="India")

cam_gadm <- subnat_geodata$Cameroon %>%
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when( ADMIN_NAME %in% c("Centre", "Est", "Littoral", "Sud", "Ouest") ~ "Centre, Sud, Est, Ouest, Littoral, Yaoundé, Douala",
                                        ADMIN_NAME %in% c("Nord-Ouest", "Sud-Ouest") ~ "Nord-Ouest and Sud-Ouest",
                                        ADMIN_NAME %in% c("Nord", "Adamoua", "Extrême-Nord") ~  "Adamaoua, Nord, and Extrême-Nord",
                                        TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Cameroon")

guin_gadm <- subnat_geodata$Guinea %>%
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when( ADMIN_NAME %in% c("Boké",  "Kindia", "Labé", "Mamou") ~ "Boké, Kindia, Labé, Mamou",
                                        ADMIN_NAME %in% c("Conakry") ~ "Conakry",
                                        ADMIN_NAME %in% c("Faranah", "Kankan", "Nzérékoré") ~ "Faranah, Kankan, and N'Zérékoré",
                                        TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Guinea")

ken_gadm <- subnat_geodata$Kenya %>%
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when( ADMIN_NAME=="Elgeyo-Marakwet" ~ "Elgeyo Marakwet",
                                        ADMIN_NAME=="Trans Nzoia"  ~ "Trans-Nzoia",
                                        TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME))

mal_gadm <- subnat_geodata$Malawi %>%
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Dedza", "Dowa", "Kasungu", "Lilongwe", "Mchinji", "Nkhotakota", "Ntchisi", "Salima", "Ntcheu") ~ "Central", # Malawi
                                       ADMIN_NAME %in% c("Chitipa", "Karonga", "Likoma", "Mzimba", "Nkhata Bay", "Rumphi", "Mzimba") ~ "Northern",
                                       ADMIN_NAME %in% c("Balaka", "Blantyre", "Chikwawa","Chiradzulu","Machinga","Mangochi","Mulanje","Mwanza", "Neno", "Nsanje","Thyolo","Phalombe","Zomba","Neno") ~ "Southern",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Malawi")

sen_gadm <- subnat_geodata$Senegal %>% # HERE
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Dakar", "Thiès") ~ "Dakar, Thiès (West)",
                                       ADMIN_NAME %in% c("Kaolack", "Fatick", "Kaffrine", "Diourbel") ~ "Diourbel, Fatick, Kaolack, Louga, Kaffrine (Center)",
                                       ADMIN_NAME %in% c("Saint-Louis", "Louga", "Matam", "Tambacounda", "Kédougou") ~ "Saint-Louis, Tambacounda, Matam, Kedougou (North East)",
                                       ADMIN_NAME %in% c("Kolda", "Sédhiou", "Ziguinchor") ~ "Kolda, Ziguinchor, Sedhiou (South)",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Senegal")

uganda_district_to_subnational <- readxl::read_excel("data-raw/local_neighbours/uganda_district_to_subnational.xlsx", col_names = FALSE)

ugd_gadm <- subnat_geodata$Uganda %>% # HERE
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% as.vector(unlist(uganda_district_to_subnational[which(uganda_district_to_subnational$...1=="Eastern Region"),2])) ~ "Bugosa, Bukedi, Bugishu, Teso", # c("Kamuli", "Kaliro", "Buyende", "Jinja", "Bugiri", "Iganga", "Mayuge", "Namutumba", "Luuka", "Namayingo", "Busia", "Tororo", "Butaleja","Pallisa", "Budaka", "Kibuku","Mbale", "Sironko", "Bududa", "Manafwa", "Bulambuli", "Katakwi", "Soroti", "Kaberamaido", "Amuria", "Serere","Kumi", "Bukedea", "Ngora") ~ "Bugosa, Bukedi, Bugishu, Teso",
                                       ADMIN_NAME %in% c(as.vector(unlist(uganda_district_to_subnational[which(uganda_district_to_subnational$...1=="Western Region"),2])),"Lake Albert", "Kibale") ~ "Bunyoro, Tooro, Ankole, Kigezi", # c("Buhweju","Buliisa","Bundibugyo","Bushenyi", "Hoima", "Ibanda", "Isingiro", "Kabale", "Kabarole", "Kamwenge", "Kanungu" ,"Kasese", "Kibaale","Kiruhura", "Kiryandongo","Kisoro", "Kyegegwa", "Kyenjojo",  "Masindi", "Mbarara", "Mitooma", "Ntoroko", "Ntungamo", "Rubirizi", "Rukungiri",  "Sheema") ~ "Bunyoro, Tooro, Ankole, Kigezi",
                                       ADMIN_NAME %in% as.vector(unlist(uganda_district_to_subnational[which(uganda_district_to_subnational$...1=="Central Region"),2])) ~ "Central 1, Central 2, and Kampala", # c("Kampala", "Mukono", "Kayunga", "Buikwe", "Buvuma", "Masaka", "Ssembabule", "Bukomansimbi", "Kalungu", "Lwengo", "Mpigi", "Wakiso", "Butambala", "Gomba","Mukono", "Kayunga", "Buikwe", "Buvuma","Kalangala", "Kiboga", "Kyankwanzi" ,"Luwero", "Nakasongola", "Nakaseke","Rakai", "Lyantonde","Mubende", "Mityana") ~ "Central 1, Central 2, and Kampala",
                                       ADMIN_NAME %in% as.vector(unlist(uganda_district_to_subnational[which(uganda_district_to_subnational$...1=="Northern Region"),2])) ~ "Lango, Acholi, Karamoja, West Nile", # c("Lira", "Amolatar", "Dokolo", "Alebtong", "Otuke","Apac", "Oyam", "Kole", "Kitgum", "Pader", "Agago", "Lamwo","Gulu", "Amuru", "Nwoya", "Omoro", "Kotido", "Moroto", "Nakapiripirit", "Abim", "Kaabong", "Amudat", "Napak", "Arua", "Yumbe", "Koboko", "Maracha", "Adjumani", "Moyo","Nebbi", "Zombo") ~ "Lango, Acholi, Karamoja, West Nile",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Uganda")

eth_gadm <- subnat_geodata$Ethiopia %>%
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Ethiopia")

mali_gadm <- subnat_geodata$Mali %>% # HERE
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Dakar", "Thiès") ~ "Dakar, Thiès (West)",
                                       ADMIN_NAME %in% c("Kayes", "Koulikoro") ~ "Kayes and Koulikoro",
                                       ADMIN_NAME %in% c("Mopti", "Timbuktu", "Gao", "Kidal") ~ "Mopti, Tombouctou, Gao and Kidal",
                                       ADMIN_NAME %in% c("Sikasso","Ségou") ~ "Sikasso and Ségou",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Mali")

mali_gadm <- subnat_geodata$Mali %>% # HERE
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Dakar", "Thiès") ~ "Dakar, Thiès (West)",
                                       ADMIN_NAME %in% c("Kayes", "Koulikoro") ~ "Kayes and Koulikoro",
                                       ADMIN_NAME %in% c("Mopti", "Timbuktu", "Gao", "Kidal") ~ "Mopti, Tombouctou, Gao and Kidal",
                                       ADMIN_NAME %in% c("Sikasso","Ségou") ~ "Sikasso and Ségou",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Mali")

moz_gadm <- subnat_geodata$Mozambique %>% # HERE
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Maputo City") ~ "City of Maputo",
                                       ADMIN_NAME %in% c("Maputo") ~ "Maputo Provincia",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Mozambique")

ghana_gadm <- subnat_geodata$Ghana %>% # HERE
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Brong Ahafo") ~ "Brong-Ahafo",
                                       ADMIN_NAME %in% c("Northern", "Upper East", "Upper West") ~ "Upper W,E and Northern",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Ghana")

nepal_gadm <- subnat_geodata$Nepal %>% # HERE
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when( ADMIN_NAME %in% c("East") ~ "Eastern",
                                        ADMIN_NAME %in% c("Far-Western") ~ "Far-western",
                                        ADMIN_NAME %in% c("Mid-Western") ~ "Mid-western",
                                        ADMIN_NAME %in% c("West") ~ "Western",
                                        TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Nepal")

pakis_gadm <- subnat_geodata$Pakistan %>% # HERE
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("F.A.T.A.") ~ "Federally Administered Tribal Areas (FATA)",
                                       ADMIN_NAME %in% c("Baluchistan") ~ "Balochistan",
                                       ADMIN_NAME %in% c("N.W.F.P.") ~ "Khyber Pakhtunkhwa",
                                       ADMIN_NAME %in% c("Northern Areas") ~ "Gilgit Baltistan",
                                       ADMIN_NAME %in% c("Punjab") ~ "Punjab (includes Islamabad Capital Territory)",
                                       ADMIN_NAME %in% c("Sind") ~ "Sindh",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Pakistan")

ben_gadm <- subnat_geodata$Benin %>% # HERE
  st_as_sf() %>%
  st_make_valid() %>%
  dplyr::select(NAME_0, NAME_1, geometry) %>%
  dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
  dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Atakora", "Donga") ~ "Atacora and Donga",
                                       ADMIN_NAME %in% c("Atlantique", "Littoral") ~ "Atlantique and Littoral",
                                       ADMIN_NAME %in% c("Borgou", "Alibori") ~ "Borgu and Alibori",
                                       ADMIN_NAME %in% c("Mono", "Kouffo") ~ "Mono and Couffo",
                                       ADMIN_NAME %in% c("Ouémé", "Plateau") ~ "Ouémé and Plateau",
                                       ADMIN_NAME %in% c("Zou" , "Collines") ~ "Zou and Collins",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
  mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
  dplyr::group_by(ADMIN_NAME) %>%
  dplyr::summarise() %>%
  dplyr::mutate(Country ="Benin")


nc2 <- bind_rows(afg_gadm,mdg_gadm)
nc2 <- bind_rows(nc2,tnz_gadm)
nc2 <- bind_rows(nc2,ngr_gadm)
nc2 <- bind_rows(nc2,rwd_gadm)
nc2 <- bind_rows(nc2,lbr_gadm)
nc2 <- bind_rows(nc2,nga_gadm)
nc2 <- bind_rows(nc2,india_gadm)
nc2 <- bind_rows(nc2,cam_gadm)
nc2 <- bind_rows(nc2,guin_gadm)
nc2 <- bind_rows(nc2,ken_gadm)
nc2 <- bind_rows(nc2,mal_gadm)
nc2 <- bind_rows(nc2,sen_gadm)
nc2 <- bind_rows(nc2,ugd_gadm)
nc2 <- bind_rows(nc2,eth_gadm)
nc2 <- bind_rows(nc2,mali_gadm)
nc2 <- bind_rows(nc2,moz_gadm)
nc2 <- bind_rows(nc2,ghana_gadm)
nc2 <- bind_rows(nc2,nepal_gadm)
nc2 <- bind_rows(nc2,pakis_gadm)
nc2 <- bind_rows(nc2,ben_gadm)

for(c in unique(nc2$Country)) {
  gadm_tmp <- nc2 %>% filter(Country==c)

  # Create neighbourhood adjancency matrix
  tmp_nb1 <- spdep::nb2mat(spdep::poly2nb(gadm_tmp), style="B")
  rownames(tmp_nb1) <- as.character(gadm_tmp$ADMIN_NAME)
  colnames(tmp_nb1) <- as.character(gadm_tmp$ADMIN_NAME)
  saveRDS(tmp_nb1, file=paste0("data-raw/local_neighbours/adjacency_matrices/",c,"_neighbouradj.RDS"))
}


my_countries <- c("Burkina Faso",
                  "Zimbabwe")

for(c in my_countries) {
  gadm_tmp <- subnat_geodata[[c]]

  gadm_tmp <-  gadm_tmp %>%
    st_as_sf() %>%
    st_make_valid() %>%
    dplyr::select(NAME_0, NAME_1, geometry) %>%
    dplyr::rename(Country=NAME_0, ADMIN_NAME = NAME_1) %>%
    mutate(Country_region = paste0(Country, "_", ADMIN_NAME)) %>%
    dplyr::group_by(ADMIN_NAME) %>%
    dplyr::summarise() %>%
    dplyr::mutate(Country = c)

  # Create neighbourhood adjancency matrix
  tmp_nb1 <- spdep::nb2mat(spdep::poly2nb(gadm_tmp), style="B")
  rownames(tmp_nb1) <- as.character(gadm_tmp$ADMIN_NAME)
  colnames(tmp_nb1) <- as.character(gadm_tmp$ADMIN_NAME)
  saveRDS(tmp_nb1, file=paste0("data-raw/local_neighbours/adjacency_matrices/",c,"_neighbouradj.RDS"))
}
