
library(tidyverse)
library(stringr)
library(jsonlite)
setwd("C:/Users/Blois Lab Generic/Documents/whichDB_auto/src/assets/")

wideTable = read.csv("recommender_table_wide.csv")
wideTable[is.na(wideTable)] = 0


longTable = wideTable %>% pivot_longer(cols=starts_with("t_"), names_to = "Time") %>% 
  dplyr::filter(value!=0) %>% dplyr::select(!value) %>%
  pivot_longer(cols=starts_with("pl_"),names_to = "RegionName") %>% dplyr::filter(value!=0) %>% dplyr::select(!value) %>%
  pivot_longer(cols=starts_with("d_"),names_to = "Dataset") %>% dplyr::filter(value!=0) %>% dplyr::select(!value)


longTable = longTable %>%  
  group_by(Time, RegionName, Dataset) %>%
  summarize(
    Database = {
      db_values <- unique(Database)
      if (length(db_values) > 1) {
        paste(paste(db_values[-length(db_values)], collapse = ", "), "or", db_values[length(db_values)])
      } else {
        db_values
      }
    },
    .groups = "drop"
  )

longTable = longTable %>% mutate(Time = case_when(
                      Time == 't_pre.Quaternary' ~ 'pre-Quaternary',
                      Time == 't_modern..post.1850.AD.' ~ "modern (post 1850 AD)",
                      Time == 't_Quaternary' ~ "Quaternary"
)) %>% mutate(RegionName = str_replace_all(str_sub(RegionName,start=4), "\\.", " ")) %>%
  mutate(Dataset = str_replace_all(str_sub(Dataset,start=3), "\\.", " ")) %>%
  mutate(Dataset = case_when(Dataset == "x ray fluorescence diffraction" ~ "x-ray fluorescence/diffraction", TRUE ~ Dataset))


regions = c("afr", "arct",'far','chin','jap','rest',"atl","eurbig",
            'cent', 'ala', 'us', 'can',"oceania", "pacoc","southam") %>% as.data.frame()
regionnames = c("Africa", "the Arctic", "the far East", "China", "Japan", "Asia outside of China  Japan  and the far East", "the Atlantic"
                , "Europe", "Central America", "Alaska", "the continental United States", "Canada", "Oceania", "the Pacific","South America") %>%
  as.data.frame() %>% cbind(regions)

names(regionnames) = c("RegionName","Region")

longTable = left_join(longTable,regionnames) %>% mutate(RegionName = case_when(
  RegionName == "Asia outside of China  Japan  and the far East" ~ "Asia outside of China, Japan, and the far East",
  TRUE ~ RegionName))


recommender_JSON = toJSON(longTable,pretty=TRUE)

write_json(longTable, path = "recommender.json", pretty = TRUE)