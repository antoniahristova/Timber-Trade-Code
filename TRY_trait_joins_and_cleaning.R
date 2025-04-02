
library(tidyverse)
library(dplyr)
library(readr)
library(magrittr)

#code to a) select only the species of interest from all the available records for a trait on TRY
#b) to average where there are multiple listings 
#c) to consider the coverage of various other traits to feed the imputation 
#note - unit standardisation is conducted manually upon the exported csvs

#join framework: 

#library(dplyr)
#name of dataset ofter join <- dataset with names%>%
#left_join(dataset with data, by = names of columns)
#write.csv(name of dataset after join, "name of file.csv")

BGCI_traded_species <- read.csv("C:/Users/ah2255/Documents/part_2_project/BGCI_Traded_Timber.csv")

#fibre length 

TRY_fibre_length <- read.delim2("C:/Users/ah2255/Documents/part_2_project/fibre_length.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)

fibre_length_join <- BGCI_traded_species %>%
  left_join(TRY_fibre_length, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_fibre_length")) %>% 
  rename(fibre_length = OrigValueStr, Original_Unit = OrigUnitStr)

#writing out csv to standardise units manually 

write.csv(fibre_length_join, "C:/Users/ah2255/Documents/part_2_project/outputs/fibre_length_join.csv")


#now averaging for fibre length across the dataset: 

fibre_length_join <- read.csv(fibre_length_join, "C:/Users/ah2255/Documents/part_2_project/outputs/fibre_length_join.csv")

fibre_length_join <- fibre_length_join %>%
  mutate(fibre_length = as.numeric(fibre_length))

fibre_length_join_averaged <- fibre_length_join %>%
  group_by(Taxon) %>%
  summarise(avg_fibre_length = mean(fibre_length, na.rm = TRUE))

write.csv(fibre_length_join_averaged, "C:/Users/ah2255/Documents/part_2_project/outputs/fibre_length_join_averaged.csv")


#leaf carbon:nitrogen ratio

TRY_leaf_CN <- read.delim2("C:/Users/ah2255/Documents/part_2_project/leaf_CN_ratio_raw.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)

leaf_CN_join <- BGCI_traded_species %>% 
  left_join(TRY_leaf_CN, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_leaf_CN")) %>% 
  rename(leaf_CN = OrigValueStr, Original_Unit = OrigUnitStr)


write.csv(leaf_CN_join, "C:/Users/ah2255/Documents/part_2_project/outputs/leaf_CN_ratio_join.csv")

#re-importing to average following manual unit standardisation 

leaf_CN_join <- read.csv("C:/Users/ah2255/Documents/part_2_project/outputs/leaf_CN_ratio_join.csv")

library(dplyr)

leaf_CN_join <- leaf_CN_join %>%
  mutate(leaf_CN = as.numeric(leaf_CN))

leaf_CN_join_averaged <- leaf_CN_join %>%
  group_by(Taxon) %>%
  summarise(avg_leaf_CN = mean(leaf_CN, na.rm = TRUE))


write.csv(leaf_CN_join_averaged, "C:/Users/ah2255/Documents/part_2_project/outputs/leaf_CN_join_averaged.csv")


#wood density

TRY_wood_density <- read.delim2("C:/Users/ah2255/Documents/part_2_project/TRY_Wood_Density.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)

wood_density_join <- BGCI_traded_species %>% 
  left_join(TRY_wood_density, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_wood_density")) %>% 
  rename(wood_density = OrigValueStr, Original_Unit = OrigUnitStr)

write.csv(wood_density_join, "C:/Users/ah2255/Documents/part_2_project/outputs/wood_density_join.csv")

wood_density_join <- read.csv("C:/Users/ah2255/Documents/part_2_project/outputs/wood_density_join_units.csv")

wood_density_join <- wood_density_join %>%
  mutate(wood_density = as.numeric(wood_density))

wood_density_averaged <- wood_density_join %>%
  group_by(Taxon) %>%
  summarise(avg_wood_density = mean(wood_density, na.rm = TRUE))

write.csv(wood_density_averaged, "C:/Users/ah2255/Documents/part_2_project/outputs/wood_density_join_averaged.csv")








