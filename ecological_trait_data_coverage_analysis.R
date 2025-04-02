#coverage analysis 


CITES_dispuwidth <- CITES_species_no_cacti %>%
  left_join(TRY_dispu_width, by = c("Taxon" = "SpeciesName")) %>% dplyr::select(-AccSpeciesName) %>%
  rename(dispu_width = OrigValueStr, Original_Unit = OrigUnitStr)

view(CITES_dispuwidth)
#coverage of the CITES-listed species is 0

#importing the cleaned dispersal syndrome dataset: 

cleaned_dispersal_syndromes <- read.csv("C:/Users/ah2255/Documents/part_2_project/outputs/dispersal_syndromes_of_traded_species.csv")

CITES_dispsyn <- CITES_species_no_cacti %>%
  left_join(cleaned_dispersal_syndromes, by = c("Taxon" = "Taxon")) %>%
  rename(disp_syn = cleaned_disp)

view(CITES_dispsyn)


woody_traded_no_CITES_disp_syn <- woody_and_traded_no_CITES %>%
  left_join(cleaned_dispersal_syndromes, by = c("Taxon" = "Taxon")) %>%
  rename(disp_syn = cleaned_disp)

view(woody_traded_no_CITES_disp_syn)  
  
#writing out to csv to remove duplicates: 

write.csv(woody_traded_no_CITES_disp_syn, "C:/Users/ah2255/Documents/part_2_project/outputs/tradednocitesdispsyn.csv")

#plant lifespan/longevity analysis 

#importing the TRY data: 


TRY_Lifespan <- read.delim2("C:/Users/ah2255/Documents/part_2_project/TRY_Lifespan.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)



lifespan_CITES <- CITES_species_no_cacti %>% 
  left_join(TRY_Lifespan, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_Lifespan")) %>% 
  rename(Lifespan = OrigValueStr, Original_Unit = OrigUnitStr)

view(lifespan_CITES)

write.csv(lifespan_CITES, "C:/Users/ah2255/Documents/part_2_project/outputs/lifespanCITES.csv")

#woody and traded join

lifespan_traded <- woody_and_traded_no_CITES %>% 
  left_join(TRY_Lifespan, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_Lifespan")) %>% 
  rename(Lifespan = OrigValueStr, Original_Unit = OrigUnitStr)

view(lifespan_traded)

write.csv(lifespan_traded, "C:/Users/ah2255/Documents/part_2_project/outputs/lifespanTraded.csv")


#reproductive dry mass

TRY_reprodry <- read.delim2("C:/Users/ah2255/Documents/part_2_project/TRY_Reproductive_dry.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)

reprodry_CITES <- CITES_species_no_cacti %>% 
  left_join(TRY_reprodry, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_reprodry")) %>% 
  rename(Reprodrymass = OrigValueStr, Original_Unit = OrigUnitStr)

view(reprodry_CITES)

reprodry_traded <- woody_and_traded_no_CITES %>% 
  left_join(TRY_reprodry, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_reprodry")) %>% 
  rename(Reprodrymass = OrigValueStr, Original_Unit = OrigUnitStr)

view(reprodry_traded)

write.csv(reprodry_traded, "C:/Users/ah2255/Documents/part_2_project/outputs/reprodrytraded.csv")

#relative growth rate 


TRY_RGR <- read.delim2("C:/Users/ah2255/Documents/part_2_project/TRY_RGR.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)

RGR_CITES <- CITES_species_no_cacti %>% 
  left_join(TRY_RGR, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_RGR")) %>% 
  rename(RGR = OrigValueStr, Original_Unit = OrigUnitStr)

view(RGR_CITES)


#woody and traded RGR 


RGR_traded <- woody_and_traded_no_CITES %>% 
  left_join(TRY_RGR, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_RGR")) %>% 
  rename(RGR = OrigValueStr, Original_Unit = OrigUnitStr)

view(RGR_traded)


write.csv(RGR_traded, "C:/Users/ah2255/Documents/part_2_project/outputs/RGRtraded.csv")

#climate preference 

TRY_climate <- read.delim2("C:/Users/ah2255/Documents/part_2_project/TRY_climate.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)


climate_CITES <- CITES_species_no_cacti %>% 
  left_join(TRY_climate, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_climate")) %>% 
  rename(climate_type = OrigValueStr, Original_Unit = OrigUnitStr)

view(climate_CITES)

climate_traded <- woody_and_traded_no_CITES %>% 
  left_join(TRY_climate, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_climate")) %>% 
  rename(climate_type = OrigValueStr, Original_Unit = OrigUnitStr)

view(climate_traded)

#climate preference

TRY_climate_preference <- read.delim2("C:/Users/ah2255/Documents/part_2_project/TRY_climate_preference.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)

climate_preference_CITES <- CITES_species_no_cacti %>% 
  left_join(TRY_climate_preference, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_climate_preference")) %>% 
  rename(climate_preference = OrigValueStr, Original_Unit = OrigUnitStr)

view(climate_preference_CITES)


write.csv(climate_preference_CITES, "C:/Users/ah2255/Documents/part_2_project/outputs/CITES_climate_preference.csv")

#woody and traded 

climate_preference_traded <- woody_and_traded_no_CITES %>% 
  left_join(TRY_climate_preference, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_climate_preference")) %>% 
  rename(climate_preference = OrigValueStr, Original_Unit = OrigUnitStr)

view(climate_preference_traded)


write.csv(climate_preference_traded, "C:/Users/ah2255/Documents/part_2_project/outputs/traded_climate_preference.csv")

#tolerance to human impact 

TRY_human_impact <- read.delim2("C:/Users/ah2255/Documents/part_2_project/TRY_human_impact.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)

human_impact_CITES <- CITES_species_no_cacti %>% 
  left_join(TRY_human_impact, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_human_impact")) %>% 
  rename(human_impact = OrigValueStr, Original_Unit = OrigUnitStr)

view(human_impact_CITES)

#woody and traded 


human_impact_traded <- woody_and_traded_no_CITES %>% 
  left_join(TRY_human_impact, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY_human_impact")) %>% 
  rename(human_impact = OrigValueStr, Original_Unit = OrigUnitStr)

view(human_impact_traded)

write.csv(human_impact_traded, "C:/Users/ah2255/Documents/part_2_project/outputs/human_impact_traded.csv")

#plotting coverage discrepancies 

coverage_data <- read.csv("C:/Users/ah2255/Documents/part_2_project/coverage.csv")


  
library(ggplot2)

coverage_plot <- ggplot(coverage_data, aes(x = Trait, y = percent_coverage, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), expand = c(0, 0)) + 
  scale_fill_manual(values = c("Traded" = "#8eb9ff", "CITES" = "red"),
                    labels = c("Traded" = "General Traded", "CITES" = "CITES-Listed")) + 
  scale_x_discrete(labels = c("Climate_preference" = "Climate Preference", 
                              "Dispersal_syndrome" = "Dispersal Syndrome", 
                              "Width_of_Dispersal_Unit" = "Width of Dispersal Unit", 
                              "Germination_rate" = "Germination Rate", 
                              "Plant_Lifespan" = "Lifespan", 
                              "Reproductive_mass" = "Reproductive Mass", 
                              "RGR" = "Relative Growth Rate")) +
  labs(
    x = "Trait",
    y = "Percentage Coverage",
    fill = "Dataset"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),    
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(t = 20)),  
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.position.inside = c(0.5, 0.85)
  )

coverage_plot

ggsave(filename = "coveragechart.png", plot = coverage_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")
