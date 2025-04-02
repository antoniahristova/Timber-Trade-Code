#original cleaning code provided by Oscar Morton,
#Updated and edited by Antonia Hristova 


library(tidyverse)
install.packages("dplyr") 
library(dplyr)
install.packages("readr") 
library(readr)
install.packages("magrittr")
library(magrittr)



CITES_MASTER <- list.files(path= "C:/Users/ah2255/Documents/part_2_project/Trade_database_download_v2024.1/Trade_database_download_v2024.1", full.names= TRUE, pattern= "*.csv") %>%
  lapply(read_csv, na = "", col_types = cols(Unit = col_character(), Import.permit.RandomID = col_character(), 
                                             Export.permit.RandomID = col_character(), Origin.permit.RandomID = col_character(), Purpose = col_character())) %>%
  bind_rows

#Removal of all re-exports as per published CITES guidelines 
#removal of all reports where the origin is stated and is not the same as the exporter 
#avoids double counting i.e. where a trade is passing through multiple countries 

CITES_TRUE <- CITES_MASTER %>% filter(Origin == Exporter | is.na(Origin)) 
CITES_Plants <- CITES_TRUE %>% filter(is.na(Class))


### Sub-setting the data into wild source and commercial purpose, wild source as defined in Morton et al 2022 ###
#list of codes at https://trade.cites.org/cites_trade_guidelines/en-CITES_Trade_Database_Guide.pdf

CITES_Plants$WildSource <- if_else(CITES_Plants$Source %in% c("W", "X", "R"), "Yes", "No")

CITES_Plants_Source <- CITES_Plants %>% filter(WildSource == 'Yes')

#Commercial purpose defined by the lab, have justified the inclusion of Personal (Code P) in addition to commercial (Code T)
#when capturing aspects of the pet trade, as done in previous studies e.g. Bush et al 2014 but likely not relevant for plants
#only need to include code "T" therefore 

CITES_Plants$PurposeC <- if_else(CITES_Plants$Purpose %in% c("T"), "Commercial", "Not Commercial")

#filtering to include only wild and commercial (shrinks data set substantially)

CITES_Wild_Com <- CITES_Plants %>% filter(WildSource == "Yes", PurposeC == "Commercial")

unique(CITES_Wild_Com$Unit)
unique(CITES_Wild_Com$Term) 
CITES_Wild_Com <- CITES_Plants %>% filter(WildSource == "Yes", PurposeC == "Commercial", Year %in% c(2000:2023))

#creates CSV from the filtered data 

write.csv(CITES_Wild_Com, "C:/Users/ah2255/Documents/part_2_project/outputs/cites_wild_com.csv", na = "")

#Tidying and removing species with uncertain naming and those not listed in the appendices 

CITES_Wild_Com <- CITES_Plants %>% 
  filter(WildSource == "Yes", PurposeC == "Commercial", Year %in% c(2000:2023), 
         !grepl("Plantae", Taxon), !grepl("spp", Taxon),!grepl("hybrid", Taxon))

CITES_Wild_Com <- CITES_Plants %>% 
  filter(WildSource == "Yes", PurposeC == "Commercial", Year %in% c(2000:2023),
         !grepl("Plantae", Taxon), !grepl("spp", Taxon), !grepl("hybrid", Taxon), !Appendix == "N") 



CITES_Wild_Com %>% group_by(Reporter.type) %>% tally()

CITES_Wild_Com_Exp <- CITES_Wild_Com %>% filter(Reporter.type == "E", Year %in% c(2000:2023), 
                                                !grepl("spp", Taxon), !grepl("Plantae", Taxon), 
                                                !grepl("hybrid", Taxon), !Appendix == "N",
                                                WildSource == "Yes", PurposeC == "Commercial") 

CITES_Wild_Com_Exp <- CITES_Wild_Com_Exp %>% filter(!Term %in% c("cultures", "plywood", "raw corals", "gall", "transformed woods"))

#The code filters the CITES_Wild_Com_Exp data frame, excluding rows where the Term column contains any of the specified terms (e.g., "cultures", "plywood", etc.)

#Focusing on terms and units that could be converted into timber volumes 
#%>% is a pipe function - passes the output of the prior function to the next 

unique(CITES_Wild_Com_Exp$Term)
unique(CITES_Wild_Com_Exp$Unit)

CITES_Timber_Terms <- CITES_Wild_Com_Exp %>% filter(Term %in% c('sawn wood', 'logs', 'chips', 'timber', 'timber pieces',
                                                                'veneer', 'wood product'), Unit %in% c('m3', 'cm3', 'kg', 'g', 'mg'))

unique(CITES_Timber_Terms$Family)
unique(CITES_Timber_Terms$Taxon)

#The species that are traded in woody terms and have units that are volumes or masses
#can be converted to volumes 
Wood_Species <- CITES_Timber_Terms %>% distinct(Family, Taxon)

#Adding column to identify if density is required

CITES_Density <- CITES_Timber_Terms %>% filter(Unit %in% c('kg', 'g', 'mg')) %>%
  distinct(Family, Taxon) %>% mutate(Comment = "Needs density")

Wood_Species1 <- left_join(Wood_Species, CITES_Density) %>% 
  mutate(Comment = ifelse(is.na(Comment), "Already recorded in volume units", Comment))



#Further filtering down to just species that are being traded using the list from Kew and BGCI
#I am using the BGCI list of identified woody species
#I am then using the Kew list of identified commercially traded timber species 
#Relevant lists: 
#https://www.kew.org/sites/default/files/2019-02/CITES%20and%20Timber_Second%20Edition.pdf 
#https://tools.bgci.org/global_tree_search.php 

Kew_List <- data.table::fread("C:/Users/ah2255/Documents/part_2_project/Kew_CITES_Species_List.csv") %>% mutate_all(na_if,"")
BGCI_Tree <- read.csv ("C:/Users/ah2255/Documents/part_2_project/BGCI_Tree_List.csv") %>% 
  mutate(BGCI_TREE = "Yes") %>% select(TaxonName, BGCI_TREE) %>% rename(Taxon = 1)

Wood_Kew_Check <- Wood_Species1 %>% mutate(Kew_Genus = case_when(grepl("Taxus", Taxon) ~ "Yes",
                                                                 grepl("Swietenia", Taxon) ~ "Yes",
                                                                 grepl("Guibourtia", Taxon) ~ "Yes",
                                                                 grepl("Guaiacum", Taxon) ~ "Yes",
                                                                 grepl("Gonystylus", Taxon) ~ "Yes",
                                                                 grepl("Diospyros", Taxon) ~ "Yes",
                                                                 grepl("Cedrela", Taxon) ~ "Yes",
                                                                 grepl("Gyrinops", Taxon) ~ "Yes",
                                                                 grepl("Aquilaria", Taxon) ~ "Yes")) %>%
 
  
  #This then looks for matches in the species 
  left_join(filter(Kew_List, !is.na(Species_list)) %>% select(Species_list) %>% mutate(Kew_Species = "Yes"),
            by = c("Taxon" = "Species_list")) %>%
  #match the BGCI list of tree species
  left_join(BGCI_Tree) %>%
  ## Class any species as a timber providing tree if it appears on both lists 
  mutate(Timber_species = case_when(BGCI_TREE == "Yes" | Kew_Genus == "Yes" | Kew_Species == "Yes" ~ "Yes"))

#so the above code accounts for entire genuses and also individual species in the CITES data

## Extract the list of woody/timber species (There are 58)
## Species that are lost are two dalbergia (e.g. liana), tree ferns, some cacti and an aloe.
## NOTE - some cacti species are retained as wood providing, and the bitter aloe is retained (all present on the BGCI list)
CITES_Timber_Checked <- Wood_Kew_Check %>% filter(Timber_species == "Yes")

#species needing density 

Density_needed_sp <- CITES_Timber_Checked %>% filter(Comment == "Needs density")

#Reading in of wood density data from TRY 
#Reference for the TRY database is: Kattge, J, Boenisch, G, Diaz, S, et al. TRY plant trait database - enhanced coverage and open access. Glob Change Biol. 2020; 26: 119-188. https://doi.org/10.1111/gcb.14904

write.csv(CITES_Timber_Checked, "C:/Users/ah2255/Documents/part_2_project/outputs/cites_timber_checked.csv")



TRY <- read.delim2("C:/Users/ah2255/Documents/part_2_project/TRY_Wood_Density.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)

#retrieving the available wood density from TRY and tidying

Wood_dens_join <- Density_needed_sp %>% 
  mutate(Taxon = ifelse(Taxon == "Osyris lanceolata", "Osyris arborea", Taxon)) %>%
  left_join(TRY, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY")) %>% 
  rename(Density = OrigValueStr, Original_Unit = OrigUnitStr)


#Using the BIOMASS library to find genus and family level estimates of unknown species identity 
library(BIOMASS)

#Getting the species list down to 10 NA sp

NA_dens <- Wood_dens_join %>% filter(is.na(Density))

#making separate columns for taxonomic hierarchy 

Sp_for_BIOMASS <- NA_dens %>% separate(Taxon, c("Genus", "Species"), " ") %>%
  unite("Taxon", 1:2, na.rm = TRUE, remove = FALSE, sep = " ")

#Getting the wood density 

Missing_Wood_Dens <- getWoodDensity(family = Sp_for_BIOMASS$Family,
                                    genus = Sp_for_BIOMASS$Genus, species = Sp_for_BIOMASS$Species) %>%
  rename(Density = meanWD, SD_Density = sdWD, Resolution = levelWD, Family = family) %>% 
  unite("Taxon", 2:3, na.rm = TRUE, remove = TRUE, sep = " ") %>%
  select(Family, Taxon, Density, SD_Density, Resolution) %>% mutate(Original_Unit = "g/cm3")

#note - remember to remove the aloe ferox value as it is estimated at the dataset level as opposed to genus or family 

Full_Dens_Table_Raw <- left_join(Wood_dens_join, Missing_Wood_Dens, by = c("Family", "Taxon")) %>% 
  mutate(Density.x = as.numeric(as.character(Density.x)),
         Density = coalesce(Density.x, Density.y),
         Original_Unit = coalesce(Original_Unit.x, Original_Unit.y)) %>%
  select(-Density.x, -Density.y, -Original_Unit.y, - Original_Unit.x) %>%
  mutate(Source = ifelse(is.na(Source), "BIOMASS", Source),
         Resolution = ifelse(is.na(Resolution), "Species", Resolution)) 
    
 
unique(Full_Dens_Table_Raw$Original_Unit)

 
#Tidying up unit names and standardising everything to the g/cm3 unit 

Std_Full_Dens_Table_Raw <- Full_Dens_Table_Raw %>% mutate(Clean_units = case_when(Original_Unit %in% c("g/cm^3", "g/cm3", "g cm-3") ~ "g/cm3",
                                                                                  Original_Unit == "mg / mm3" ~ "mg/mm3",
                                                                                  Original_Unit == "kg/m3" ~ "kg/m3",
                                                                                  Original_Unit == "t m3" ~ "t/m3")) %>%
  mutate(Dens_std = case_when(Clean_units %in% c("g/cm3", "t/m3", "mg/mm3") ~ Density,
                              Clean_units == "kg/m3" ~ Density/1000),
         Unit_std = "g/cm3")


#Pulling out the TRY list and taking the mean and standard deviation of the multiple observations 

TRY_Sp <- Std_Full_Dens_Table_Raw %>% filter(Source == "TRY") %>% 
  group_by(Taxon, Family, Comment, Kew_Genus, Kew_Species, BGCI_TREE, Timber_species, Source, Resolution, Unit_std) %>%
  summarise(Dens_stdm = mean(Dens_std), SD_Density = sd(Dens_std))

BIOMASS_Sp <- Std_Full_Dens_Table_Raw %>% filter(Source == "BIOMASS") %>% select(-Density, -Original_Unit, -Clean_units) %>% rename(Dens_stdm = Dens_std)

#joining the datasets together 

All_Dens <- rbind(TRY_Sp, BIOMASS_Sp) %>% mutate(SD_Density = ifelse(is.na(SD_Density), 0, SD_Density))

# NOTE Dalbergia lactea is only given family level estimates from Biomass so we go back to TRY and calculate our own genus level estimate
Dalbergia_Genus_Est <- TRY %>% filter(grepl("Dalbergia", SpeciesName)) %>% 
  summarise(Dens_stdm = mean(as.numeric(as.character(OrigValueStr))),
            SD_Density = sd(as.numeric(as.character(OrigValueStr))))

All_Dens <- All_Dens %>% mutate(Source = ifelse(Taxon == "Dalbergia lactea", "TRY", Source),
                                Resolution = ifelse(Taxon == "Dalbergia lactea", "Genus", Resolution),
                                Dens_stdm = ifelse(Taxon == "Dalbergia lactea", Dalbergia_Genus_Est$Dens_stdm, Dens_stdm),
                                SD_Density = ifelse(Taxon == "Dalbergia lactea", Dalbergia_Genus_Est$SD_Density, SD_Density))


write.csv(All_Dens, "C:/Users/ah2255/Documents/part_2_project/outputs/Wood_Density_Master_E.csv")
All_Dens <-  data.table::fread("C:/Users/ah2255/Documents/part_2_project/outputs/Wood_Density_Master_E.csv") 

#CITES Unit and Term Codes 
#X records of timber species in trade 
CITES_TRUE_Timber <- CITES_Timber_Terms %>% filter(Taxon %in% CITES_Timber_Checked$Taxon)
unique(CITES_TRUE_Timber$Term)
unique(CITES_TRUE_Timber$Unit) 

## For species already in Volume units we want everything to be in m3
CITES_Vol_to_Vol <- CITES_TRUE_Timber %>% filter(Unit %in% c("m3", "cm3")) %>%
  mutate(Volume = case_when(Unit == "m3" ~ Quantity,
                            Unit == "cm3" ~ Quantity/1000000), ## to m3
         Vol_Unit = "m3")

CITES_Mass_to_Vol1 <- CITES_TRUE_Timber %>% filter(Unit %in% c("kg", "g")) %>%
  mutate(Quantity_g = case_when(Unit == "kg" ~ Quantity*1000, ## to g
                                Unit == "g" ~ Quantity))

## Add the density data to mass trade records
CITES_Mass_to_Vol2 <- left_join(CITES_Mass_to_Vol1, select(ungroup(All_Dens), Taxon, Dens_stdm)) %>%
  mutate(Volume_cm3 = Quantity_g/Dens_stdm) %>% ## v = m/d, cm3 = g/gcm3
  mutate(Volume = Volume_cm3/1000000, ## to m3
         Vol_Unit = "m3") 

## Now 168922 volume records
CITES_vol <- rbind(select(CITES_Vol_to_Vol, Id, Year, Appendix, Taxon, Order, Family, Genus, Term, Quantity, Unit, Importer, Exporter, Purpose, Source,
                          Reporter.type, WildSource, PurposeC, Volume, Vol_Unit),
                   select(CITES_Mass_to_Vol2, Id, Year, Appendix, Taxon, Order, Family, Genus, Term, Quantity, Unit, Importer, Exporter, Purpose, Source,
                          Reporter.type, WildSource, PurposeC, Volume, Vol_Unit))



#### Reporter Type ####

CITES_Volplot <- CITES_vol %>% filter(Year %in% c(2000:2023)) %>% group_by(Year) %>% tally()
CITES_Volplot %>% group_by(Year) %>% tally() %>% 
  ggplot(aes(Year, n)) + geom_bar(stat = 'identity', width = 1, alpha = 0.5) +
  geom_bar(data = CITES_Volplot, stat = 'identity', width = 1, fill = "dodgerblue", alpha = 0.5) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  #annotate(x = 1980, y=110000, "text", label = "b.", size = 8) +
  xlab("Year") + ylab("Records") +
  theme_classic(base_size = 14) + 
  theme(axis.text = element_text(angle = 45, hjust = 1, vjust = -1))

## Importer reported has more records
CITES_vol %>% group_by(Reporter.type) %>% tally()

unique(CITES_vol$Taxon)

#### Original listing ####


## extract species in trade taxonomic information focus on Importer reported trade
## 53 species
(CITES_Species <- CITES_vol %>% filter(Reporter.type == "E", Year %in% c(2000:2023), !grepl("spp", Taxon), !Appendix == "N",
                                       WildSource == "Yes", PurposeC == "Commercial") %>%
   group_by(Taxon) %>% slice(1) %>% select(Order, Family, Genus, Taxon) %>% ungroup()) 

CITES_vol_Exp <- CITES_vol %>% filter(Reporter.type == "E", Year %in% c(2000:2023), !grepl("spp", Taxon), !Appendix == "N",
                                      WildSource == "Yes", PurposeC == "Commercial") 

unique(CITES_vol_Exp$Taxon)

#Species timeseries -this code uses the cites listings database to extract the time periods the species were listed for and add the actual trade and 0's to
#that.

# Read in the cites historic listings data
Historic_CITES <- data.table::fread("C:/Users/ah2255/Documents/part_2_project/History_of_CITES_Listings_2024.csv") %>% 
  mutate(Year = format(as.Date(EffectiveAt, format="%d/%m/%Y"),"%Y"))

#Get the unique listings from the listing data/
# This tidies and gets the first year a species is CITES listed (the start of its possible time series)
First_listing <- Historic_CITES %>% group_by(Order, Family, Genus, FullName, Year, Appendix) %>% tally() %>%
  mutate(FullName = ifelse(FullName == Order, NA, FullName),
         FullName = ifelse(FullName == Family, NA, FullName),
         FullName = ifelse(FullName == Genus, NA, FullName)) %>%
  group_by(Order, Family, Genus, FullName) %>% slice_min(Year) %>% ungroup() %>% rename(Taxon = FullName) %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

s <- select(First_listing, Order) %>% distinct() %>% mutate(score = 1)

t <- left_join(select(CITES_Species, Order), s, by = "Order")


# First match all species level CITES listings
FL_SP <- First_listing %>% filter(!is.na(Taxon)) %>% select(Taxon, Year)

Sp_join <- left_join(CITES_Species, FL_SP, by = "Taxon")

sp_done <- Sp_join %>% filter(!is.na(Year))

# Second match at genus level
genus_to_match <- Sp_join %>% filter(is.na(Year)) %>% select(-Year)
# get all the genus level appendix listings
FL_Genus <- First_listing %>% filter(is.na(Taxon), !is.na(Genus)) %>% select(Genus, Year) 
Genus_join <- left_join(genus_to_match, FL_Genus, by = "Genus")
Genus_done <- Genus_join %>% filter(!is.na(Year))

#third match at family level
FL_Family <- First_listing %>% filter(is.na(Taxon), is.na(Genus), !is.na(Family)) %>% select(Family, Year)
Fam_to_match <- Genus_join %>% filter(is.na(Year)) %>% select(-Year)
Fam_join <- left_join(Fam_to_match, FL_Family, by = "Family")
Fam_done <- Fam_join %>% filter(!is.na(Year))

#fourth match at order level 

FL_Order <- First_listing %>% filter(is.na(Taxon), is.na(Genus), is.na(Family), !is.na(Order)) %>% select(Order, Year)
Order_to_match <- Fam_join %>% filter(is.na(Year)) %>% select(-Year) 
Order_join <- left_join(Order_to_match, FL_Order, by = "Order")

Order_done <- Order_join %>% filter(!is.na(Year))

All_sp_fl <- rbind(sp_done,Genus_done,Fam_done, Order_done)

All_sp_fl %>% filter(Year >=1999)

# Attach species FL to CITES db ####
FL_CITES_Species <- All_sp_fl %>% rename(FL_year = Year) %>% select(Taxon, FL_year) ## 53 sp

CITES_vol_Exp <- left_join(CITES_vol_Exp, FL_CITES_Species, by = "Taxon")
CITES_vol_Exp %>% filter(is.na(FL_year))


#### Remove CITES Deletion ####

## extract the true cites deletions
## We considered removing these, but that would bias the data against "success stories". Therefore we
## include them for the period in 2000 - 2018 that they were listed. Therefore a species could have a time series 2000 - 2007.

CITES_Deletions <- Historic_CITES %>% filter(ChangeType == "DELETION" & IsCurrent == "TRUE") %>%
  select(Year, FullName, ChangeType) %>% 
  rename(Taxon = FullName, Year_DEL = Year) %>%
  group_by(Taxon) %>% arrange(Taxon, Year_DEL) %>% slice_max(Year_DEL)

Del_sp <- CITES_Deletions$Taxon

CITES_vol_Exp %>% filter(Taxon %in% Del_sp) %>% summarise(n = (unique(Taxon))) 


## Add the deletions to the trade database as a separate year of deletion column.
## Later we will use this as an end point for all these series.
## All other species that had not been deleted will have full time series to 2018, therefore we can add the
## final year (2018) to the species without a deletion year.
CITES_vol_Exp <- left_join(CITES_vol_Exp, CITES_Deletions, by = "Taxon") %>%
  mutate(Year_DEL = ifelse(is.na(Year_DEL), 2023, Year_DEL))


write.csv(CITES_vol_Exp, "C:/Users/ah2255/Documents/part_2_project/outputs/Exp_reported_vol.csv", na = "")

## Get all trades to volumes per species per year per Exporter
CITES_vol_Exp_Sum <- CITES_vol_Exp %>% 
  ## Exporter ONLY, In our study period, No aggregate of species - why? Ask about including importer information
  filter(Reporter.type == "E", Year %in% c(2000:2023),WildSource == "Yes", PurposeC == "Commercial", !Appendix == "N") %>% 
  ## Group and tally per species/year
  group_by(Year, Taxon, Family, Order, FL_year, Year_DEL, Vol_Unit, Exporter) %>% 
  tally(Volume) %>%
  mutate(FL_year = as.numeric(FL_year), Year_DEL = as.numeric(Year_DEL))


#Custom time series for each species potential presence in trade 
#Time series is corrected for each species time of listing and (if relevant) time of removal from the appendices

Species_timeframe <- CITES_vol_Exp_Sum %>% group_by(Taxon, Family, Order, Vol_Unit, Exporter) %>% 
  summarise(Year = seq(from = min(FL_year), to = max(Year_DEL), length.out = max(Year_DEL) - min(FL_year) + 1))

Manual_check <- Species_timeframe %>% group_by(Taxon) %>% filter(Year == max(Year)) %>% filter(Year != 2020)

unique(CITES_vol_Exp_Sum$Taxon)
## Expand the data set to fill missing years between 2000 - 2023
CITES_Exp_Timber_Vol <- left_join(Species_timeframe, CITES_vol_Exp_Sum) %>% select(-FL_year, -Year_DEL) %>%
  rename(Vol = n) %>% mutate(Vol = ifelse(is.na(Vol), 0, Vol)) %>%
  filter(Year > 1999)

#creating a plot of this 

ggplot(CITES_Exp_Timber_Vol, aes(Year, Vol))  + geom_point() + facet_wrap(~Taxon, scales = "free")

sum(CITES_Exp_Timber_Vol$Vol)

write.csv(CITES_Exp_Timber_Vol, "C:/Users/ah2255/Documents/part_2_project/outputs/Processed_CITES_2023.csv", na = "")

library(rredlist) #IUCN data for timber species 

CITES_Exp_Timber_Vol <- data.table::fread("C:/Users/ah2255/Documents/part_2_project/outputs/Processed_CITES_2023.csv", 
                                          na.strings = "")
Sp_List <- CITES_Exp_Timber_Vol %>% group_by(Taxon) %>% tally() %>% select(Taxon) %>% as.data.frame()
CITES_Exp_Timber_Vol %>% filter(Year > 1999) %>% summarise(n_distinct(Taxon))

#remember to remove aloe ferox 
NA_update <- Sp_List %>% 
  mutate(IUCNName = case_when(Taxon == "Aloe ferox" ~ "Aloe ferox", ## Cape aloe not assessed
                              Taxon == "Bulnesia sarmientoi" ~"Gonopterodendron sarmientoi", ## Synonym
                              Taxon == "Dalbergia bariensis" ~"Dalbergia oliveri", ## Synonym 
                              Taxon == "Dipteryx panamensis" ~"Dipteryx oleifera", ## Not assessed
                              Taxon == "Gyrinops caudata" ~"Gyrinops caudata", ## Not assessed
                              Taxon == "Gyrinops ledermanii" ~"Gyrinops ledermanii", ## Not assessed
                              Taxon == "Gyrinops versteegii" ~"Dipteryx  oleifera", ## newly accepted
                              TRUE ~ Taxon))

apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"

## Dummy data frame for the loops
df <- data.frame(IUCNName = character(),
                 Year = character(),
                 IUCN_code = character(),
                 IUCN_cat = character())

for(i in 1:nrow(NA_update)){
  ## incorporate 2s delay between each query
  Sys.sleep(2)
  ## Progress update
  cat('Species=',i, '\n')
  ## get historical data from website
  sp <- NA_update$IUCNName[i]
  iucnHistory <- rl_history(name=sp, key=apikey)
  # IF species cannot be found
  if (length(iucnHistory$result) == 0){ 
    spDf <- data.frame(IUCNName = sp,
                       Year = NA,
                       IUCN_code = NA,
                       IUCN_cat = NA)
    df <- rbind(df, spDf)
    # cat('Check ', sp, '\n')
  } else { 
    spdf <- data.frame(IUCNName = sp,
                       Year = iucnHistory$result$year,
                       IUCN_code = iucnHistory$result$code,
                       IUCN_cat = iucnHistory$result$category)
    df <- rbind(df, spdf)
  }
}

## 3 species after we checked names that have not been assessed.
df %>% filter(is.na(Year))

df_all <- left_join (df, NA_update)

write.csv(df_all, "C:/Users/ah2255/Documents/part_2_project/outputs/Timber_IUCN_Assess_With_NA.csv", na = "")

#should work up to here 


#### Cleaning pre 2000 IUCN codes ####

## A variety of codes are used especially older version codes
unique(df_all$IUCN_code)
length(unique(df_all$Taxon)) ## 53


## Therefore remove these
Historic_IUCN <- df_all %>%
  ## remove I (interdeterminate) codes as all I species were assessed the same year again.
  ## Set Not assessed species as not assessed in 2000 otherwise they are also NA for Year and get removed later when they shouldnt
  mutate(Clean_code = case_when(IUCN_code == "LR/lc" ~ "LC",
                                IUCN_code == "LR/cd" ~ "NT",
                                IUCN_code == "LR/nt" ~ "NT",
                                IUCN_code == "V" ~ "VU",
                                IUCN_code == "nt" ~ "NT",
                                IUCN_code == "E" ~ "EN",
                                is.na(IUCN_code) ~ "NE", 
                                TRUE ~ IUCN_code)) %>%
  ungroup() %>%
  filter(Clean_code != "I") %>%
  select(IUCNName, Taxon, Year, Clean_code) %>% distinct()

length(unique(Historic_IUCN$Taxon)) ## 53
## Check species that are assessed multiple times in one year
Historic_IUCN %>% group_by(Taxon, Year) %>% filter(n() > 1)

## Many species assessed more than once in 1998, manually check date and clean
Historic_IUCN <- Historic_IUCN %>% filter(!(Taxon == "Cedrela odorata" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Dalbergia latifolia" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Guaiacum officinale" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Guaiacum sanctum" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Pericopsis elata" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Swietenia humilis" & Year == 1998 & Clean_code == "NT"))

## No EX and EW species
Historic_IUCN %>% filter(Clean_code %in% c("EX", "EW"))


## Check removal and conversion left only the post 2001 framework
unique(Historic_IUCN$Clean_code)

## Backbone of values 2000 - 2020
backbone <- expand.grid(Year = as.integer(1975:2023), Taxon = unique(Historic_IUCN$Taxon))

## left join this and create your unrolled status
## Some species in trade before being IUCN assessed these are the NA values
Historic_IUCN$Year <- as.integer(Historic_IUCN$Year)
backbone$Year <- as.integer(backbone$Year)

## Here we add the backbone of species and dates to the IUCN data
df_new <- left_join(backbone, Historic_IUCN) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(Clean_code , .direction = "down") %>% 
  fill(IUCNName, .direction = "down") %>% 
  fill(IUCNName, .direction = "up") %>% 
  fill(Taxon, .direction = "down") %>% 
  fill(Taxon, .direction = "up") %>% 
  filter(Year %in% c(1999:2023)) %>% ungroup() %>%
  mutate(Year = as.numeric(Year)) %>% 
  select(Year, Taxon, Clean_code, IUCNName, Taxon) %>% 
  mutate(Clean_code = replace_na(Clean_code, "NE"),
         IUCNName = ifelse(is.na(IUCNName), Taxon, IUCNName))


length(unique(df_new$Taxon))

df_new <-df_new %>% mutate (Threat = case_when (Clean_code %in% c("CR", "EN", "VU") ~ "threatened",
                                                Clean_code%in% c("LC", "NT","NE")~ "not threatened"))


CITES_IUCN_Timber <- left_join(CITES_Exp_Timber_Vol, df_new)


write.csv(CITES_IUCN_Timber, "C:/Users/ah2255/Documents/part_2_project/outputs/CITES_IUCN_Timber_WITH_EXPORTERS.csv", na = "")

CITES_IUCN_Timber_Sum <- CITES_IUCN_Timber %>% group_by(Taxon, Family, Order, Vol_Unit,
                                                        Year,  Clean_code, IUCNName, Threat) %>%
  tally(Vol) %>% rename("Vol" = "n")

write.csv(CITES_IUCN_Timber_Sum, "C:/Users/ah2255/Documents/part_2_project/outputs/CITES_IUCN_Timber_E.csv", na = "")








