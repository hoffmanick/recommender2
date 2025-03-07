
library(tidyverse)
library(stringr)
library(jsonlite)
setwd("C:/Users/Blois Lab Generic/Documents/whichDB_auto/src/assets/")

wideTable = read.csv("recommender_table_wide030525.csv")
wideTable[is.na(wideTable)] = 0



dbid = c(3, 4, 5, 6, 7, 8, 19, 22, 26, 
         28, 2, 10, 16, 17, 36, 24, 32, 
         13, 38, 37, 15, 31, 41, 42,
         14, 12, 20, 25, 27, 33, 39, 40, 18)

dbnames = c("European Pollen Database", "Indo-Pacific Pollen Database", 
            "Latin American Pollen Database", 
            "North American Pollen Database", "Pollen Database of Siberia and the Russian Far East", 
            "Canadian Pollen Database", "Japanese Pollen Database", "Chinese Pollen Database",
            "Deep-Time Palynology Database", 
            "Alpine Palynological Database", "African Pollen Database", "FAUNMAP", "MioMap", "Alaskan Archaeofaunas", 
            "PaleoVertebrates of Latin America", "ANTIGUA", "Faunal Isotope Database", "Academy of Natural Sciences at Drexel University", 
            "Tropical South American Diatom Database", "St. Croix Watershed Research Station of the Science Museum of Minnesota", 
            "North American Non-Marine Ostracode Database Project (NANODe)",
            "Neotoma Ostracode Database", "Nonmarine Ostracod Distribution in Europe Database", "East Asian Nonmarine Ostracod Database", 
            "NDSU Insect Database", "North American Plant Macrofossil Database", "Neotoma Midden Database", "Neotoma Testate Amoebae Database", 
            "Neotoma Biomarker Database", "Neotoma Charcoal Data", "Marine Dinoflagellates Database", "Sedimentary aDNA Database", 
            "French Institute of Pondicherry Palynology and Paleoecology Database")

nameid_key = as.data.frame(dbid) %>% cbind(as.data.frame(dbnames))
names(nameid_key) = c("dbid","dbname")


names(wideTable)[1] = "Database"

wideTable = wideTable %>% left_join(nameid_key,by=join_by(Database == dbname))


longTable = wideTable %>% pivot_longer(cols=starts_with("t_"), names_to = "Time") %>% 
  dplyr::filter(value!=0) %>% mutate(marginal_t = case_when(value == 5 ~ "yes", value == 1 ~ "no")) %>% dplyr::select(!value) %>%
  pivot_longer(cols=starts_with("pl_"),names_to = "RegionName") %>% dplyr::filter(value!=0) %>% mutate(marginal_r = case_when(value == 5 ~ "yes", value == 1 ~ "no")) %>%
  dplyr::select(!value) %>%
  pivot_longer(cols=starts_with("d_"),names_to = "Dataset") %>% dplyr::filter(value!=0) %>% mutate(marginal_d = case_when(value == 5 ~ "yes", value == 1 ~ "no")) %>%
  dplyr::select(!value) %>% 
  mutate(marginal = case_when(marginal_d == "yes" | marginal_t == "yes" | marginal_r == "yes" ~ "yes", TRUE ~ "no")) %>%
  select(!c(marginal_t,marginal_d,marginal_r))



longTable = longTable %>%  
  group_by(Time, RegionName, Dataset,marginal) %>%
  summarize(
    Database = {
      db_values <- unique(Database)
      if (length(db_values) > 1) {
        paste(paste(db_values[-length(db_values)], collapse = ", "), "or", db_values[length(db_values)])
      } else {
        db_values
      }
    },
    dbids = paste0(unique(dbid),collapse=","),
    .groups = "drop"
  )

longTable = longTable %>% mutate(Time = case_when(
                      Time == 't_pre.Quaternary' ~ 'pre-Quaternary',
                      Time == 't_modern..post.1850.AD.' ~ "modern (post 1850 AD)",
                      Time == 't_Quaternary' ~ "Quaternary"
)) %>% mutate(RegionName = str_replace_all(str_sub(RegionName,start=4), "\\.", " ")) %>%
  mutate(Dataset = str_replace_all(str_sub(Dataset,start=3), "\\.", " ")) %>%
  mutate(Dataset = case_when(Dataset == "x ray fluorescence diffraction" ~ "x-ray fluorescence/diffraction", 
                             Dataset == "loss on ignition" ~ "loss-on-ignition", TRUE ~ Dataset))


regions = c("noraf", "subsah", "indone", "malay", "myan", "thai", "vietn", "philip", "timor", "singa", "brune", "cambo", "lao", 
                     "turkmen", "taji", "afghan", "uzbek", "kazakh", "kyrgyzs", "arabia", "turk", "iran", "georg", "azerb", "armeni",
                     "russia", "india", "mong", "chin", "japan", "korea", "taiwan", "cent", "ala", "us", "can", "green", "mex", "caribb",
                     "arct", "atl", "pacoc", "indianoc", "southoc", "eurbig", "oceania", "southam") %>% as.data.frame()
regionnames = c("North Africa", "Sub-Saharan Africa", "Indonesia", "Malaysia", "Myanmar", 
                "Thailand", "Vietnam", "Philippines", 
                "East Timor", "Singapore", "Brunei", 
                "Cambodia", "Laos", "Turkmenistan", 
                "Tajikistan", "Afghanistan", "Uzbekistan", 
                "Kazakhstan", "Kyrgyzstan", "Arabia", "Turkey", 
                "Iran", "Georgia", "Azerbaijan", "Armenia", "Russia", 
                "Indian subcontinent", "Mongolia", "China", "Japan", 
                "Korea", "Taiwan", "Central America", "Alaska", 
                "Continental US", "Canada", "Greenland", "Mexico",
                "Caribbean", "Arctic Ocean", "Atlantic Ocean", 
                "Pacific Ocean", "Indian Ocean", "Southern Ocean", "Europe","Oceania","South America") %>%
  as.data.frame() %>% cbind(regions)
names(regionnames) = c("RegionName","Region")

longTable = left_join(longTable,regionnames) %>% mutate(RegionName = case_when(
  RegionName == "Asia outside of China  Japan  and the far East" ~ "Asia outside of China, Japan, and the far East",
  TRUE ~ RegionName))


recommender_JSON = jsonlite::toJSON(longTable, pretty=TRUE)

jsonlite::write_json(longTable, path = "recommender.json",pretty=TRUE)