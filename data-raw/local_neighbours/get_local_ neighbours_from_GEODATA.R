## Load your library ----------
library(mcmsupply)
library(dplyr)
library(geodata)
library(sf)
library(spdep)

country = "Nigeria"
my_geodata <- geodata::gadm(country=country, level=1, path="data-raw/local_neighbours/geodata")

my_geodata <- my_geodata %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  dplyr::select(COUNTRY, NAME_1, geometry) %>%
  dplyr::rename(ADMIN_NAME = NAME_1)

# These are manual adjustments to the regional name so that they match the subnational_FPsource_data subnational region names.
if(country=="Afghanistan") {
  my_geodata <- my_geodata %>%
    sf::st_as_sf() %>%
    sf::st_make_valid() %>%
    dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME=="Hilmand" ~ "Helmand",
                                         ADMIN_NAME=="Hirat" ~ "Herat",
                                         ADMIN_NAME=="Kunar" ~ "Kunarha",
                                         ADMIN_NAME=="Panjshir" ~ "Panjsher",
                                         ADMIN_NAME == "Sari Pul" ~ "Sar-E-Pul",
                                         ADMIN_NAME == "Uruzgan" ~ "Urozgan",
                                         TRUE ~ as.character(ADMIN_NAME)))
}

if(country=="Madagascar") {
  my_geodata <- my_geodata %>%
    sf::st_as_sf() %>%
    sf::st_make_valid() %>%
    dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME == "Antananarivo" ~ "Analamanga, Vakinankaratra, Itasy, Bongolava (Antananarivo)",
                                         ADMIN_NAME == "Toliary" ~ "Atsimo Andrefana, Androy, Anosy, Menabe (Toliary)",
                                         ADMIN_NAME == "Toamasina" ~ "Atsinanana, Analanjirofo, Alaotra Mangoro (Toamasina)",
                                         ADMIN_NAME == "Mahajanga"  ~ "Boeny, Sofia, Betsiboka, Melaky (Mahajanga)",
                                         ADMIN_NAME ==  "Antsiranana" ~ "Diana, Sava (Antsiranana)",
                                         ADMIN_NAME == "Fianarantsoa" ~ "Haute Matsiatra, Anamoroni'i Mania, Vatovavy Fitovinany, Ihorombe, Atsimo Atsinanana (Fianarantsoa)",
                                         TRUE ~ as.character(ADMIN_NAME)))

}

if(country=="Tanzania") {
  my_geodata <- my_geodata %>%
    sf::st_as_sf() %>%
    sf::st_make_valid() %>%
    dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Arusha", "Manyara", "Kilimanjaro") ~ "Arusha, Kilimanjaro, and Manyara (Northern Highlands)",
                                         ADMIN_NAME %in% c("Dar es Salaam", "Morogoro", "Pemba North", "Pemba South", "Pwani", "Tanga", "Zanzibar West", "Zanzibar North", "Zanzibar South and Central") ~ "Dar Es Salaam, Morogoro, Pemba, Pwani, Tanga, Town West, and Zanzibar (Coastal)",
                                         ADMIN_NAME %in% c("Dodoma", "Singida") ~ "Dodoma and Singida (Central)",
                                         ADMIN_NAME %in% c("Geita", "Kagera", "Mwanza", "Shinyanga", "Simiyu", "Tabora", "Mara", "Kigoma") ~ "Geita, Kagera, Kigoma, Mara, Mwanza, Shinyanga, Simiyu, Tabora (Lake)",
                                         ADMIN_NAME %in% c("Iringa", "Njombe", "Mbeya", "Katavi", "Rukwa") ~ "Iringa, Myeba, Nijombe, Katavi, and Rukwa (Southern Highlands)",
                                         ADMIN_NAME %in% c("Lindi", "Mtwara","Ruvuma") ~ "Lindi, Mtwara, and Ruvuma (South)",
                                         TRUE ~ as.character(ADMIN_NAME))) %>%
    dplyr::group_by(ADMIN_NAME) %>%
    dplyr::summarise()

}

if(country=="Niger") {
  my_geodata <- my_geodata %>%
    sf::st_as_sf() %>%
    sf::st_make_valid() %>%
    dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME=="Dosso" ~ "Dossa",
                                         ADMIN_NAME %in% c("Tahoua", "Agadez") ~ "Tahoua/Agadez",
                                         ADMIN_NAME=="Tillabéry" ~ "Tillaberi",
                                         ADMIN_NAME %in% c("Zinder", "Diffa") ~ "Zinder/Diffa",
                                         TRUE ~ as.character(ADMIN_NAME))) %>%
    dplyr::group_by(ADMIN_NAME) %>%
    dplyr::summarise()

}

if(country=="Rwanda") {
  my_geodata <- my_geodata %>%
    sf::st_as_sf() %>%
    sf::st_make_valid() %>%
    dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME=="Amajyaruguru" ~ "North",
                                         ADMIN_NAME=="Amajyepfo" ~ "South",
                                         ADMIN_NAME=="Iburasirazuba" ~ "East",
                                         ADMIN_NAME=="Iburengerazuba" ~ "West",
                                         ADMIN_NAME=="Umujyi wa Kigali" ~ "Kigali",
                                         TRUE ~ as.character(ADMIN_NAME))) %>%
    dplyr::group_by(ADMIN_NAME) %>%
    dplyr::summarise()
}

if(country=="Liberia") {
  my_geodata <- my_geodata %>%
    sf::st_as_sf() %>%
    sf::st_make_valid() %>%
    dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Bomi", "Bong", "GrandBassa", "Margibi", "Montserrado", "Nimba", "Lofa") ~ "North Central",  # Liberia
                                       ADMIN_NAME %in% c("Grad Cape Mount") ~ "North Western",
                                       ADMIN_NAME %in% c( "GrandGedeh", "River Cess", "Sinoe") ~ "South Eastern A",
                                       ADMIN_NAME %in% c("GrandKru", "Maryland", "River Gee") ~ "South Eastern B",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
    dplyr::group_by(ADMIN_NAME) %>%
    dplyr::summarise()
}

if(country=="Nigeria") {
  my_geodata <- my_geodata %>%
    sf::st_as_sf() %>%
    sf::st_make_valid() %>%
    dplyr::mutate(ADMIN_NAME = case_when(ADMIN_NAME %in% c("Kwara", "Federal Capital Territory", "Benue", "Kogi", "Nasarawa", "Niger", "Plateau") ~ "North Central",
                                       ADMIN_NAME %in% c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe") ~ "North East",
                                       ADMIN_NAME %in% c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara") ~ "North West",
                                       ADMIN_NAME %in% c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo") ~ "South East",
                                       ADMIN_NAME %in% c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers") ~ "South South",
                                       ADMIN_NAME %in% c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo") ~ "South West",
                                       TRUE ~ as.character(ADMIN_NAME))) %>%
    dplyr::group_by(ADMIN_NAME) %>%
    dplyr::summarise()
}

# Get neighbourhood adjacency matrix
coords = sf::st_coordinates(sf::st_centroid(sf::st_geometry(my_geodata)))
queen_nb = spdep::poly2nb(my_geodata, row.names=my_geodata$ADMIN_NAME, queen=TRUE)

# Create neighbourhood adjancency matrix if there are any islands
mcadd = mcmsupply::mstconnect(my_geodata,queen_nb)

tmp_nb1 <- spdep::nb2mat(mcadd, style="B")
rownames(tmp_nb1) <- my_geodata$ADMIN_NAME
colnames(tmp_nb1) <- my_geodata$ADMIN_NAME

saveRDS(tmp_nb1, file=paste0("data-raw/local_neighbours/",country,"_neighbouradj.RDS"))

