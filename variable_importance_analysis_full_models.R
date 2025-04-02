
library(dplyr)
library(MASS)
library(tidyr)
library(caret)
library(ggplot2)
#importing the dataframe 

full_dataframe <- read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\full_dataframe_for_glm.csv")


full_dataframe$avg_wood_density <- scale(full_dataframe$avg_wood_density)
full_dataframe$comp_strength <- scale(full_dataframe$comp_strength)
full_dataframe$avg_fibre_length <- scale(full_dataframe$avg_fibre_length)
full_dataframe$H.cos <- scale(full_dataframe$H.cos)
full_dataframe$H.sin <- scale(full_dataframe$H.sin)
full_dataframe$S <- scale(full_dataframe$S)
full_dataframe$L <- scale(full_dataframe$L)

#model just colour 

glm_colour_CITES <- glm(CITES ~  H.cos:H.sin + S + L + 
                                 H.cos:L + H.sin:L + H.cos:S + H.sin:S + L:S,
                                data = full_dataframe,
                                family = binomial(link = "logit"))

summary(glm_colour_CITES)

#variable importance of colour 


var_imp_colour_CITES <- varImp(glm_colour_CITES, scale = TRUE)
print(var_imp_colour_CITES)


#colour variable importance plot 

var_imp_colour_CITES <- read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\colour_variable_importance_CITES.csv")


library(ggplot2)


var_imp_colour_CITES_plot <- ggplot(var_imp_colour_CITES, aes(x = reorder(var, -var.imp), y = var.imp, fill = var.imp)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#8fd0e3", high = "#2d697a") +
  theme_classic() + 
  labs(y = "Importance", x = " ") +
  scale_x_discrete(labels = c("L" = "Lightness", 
                              "S" = "Saturation",
                              "H.cos:H.sin" = "Hue(Cos Element):Hue(Sin Element)", 
                              "S:L" = "Saturation:Lightness", 
                              "H.cos:L" = "Hue(Cos Element):Lightness", 
                              "H.sin:L" = "Hue(Sin Element):Lightness",
                              "H.cos:S" = "Hue(Cos Element):Saturation",
                              "H.sin:S" = "Hue(Sin Element):Saturation")) +
  theme(axis.text.x = element_text(angle = 63, hjust = 1),
        legend.position = "none") + 
  theme(axis.text = element_text(size = 17)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) + 
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0))

var_imp_colour_CITES_plot



ggsave(filename = "var_imp_colour_CITES_plot.png", plot = var_imp_colour_CITES_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")



#model just functional traits 


glm_traits_CITES <- glm(CITES ~ avg_wood_density + comp_strength + avg_fibre_length + 
                          avg_wood_density:comp_strength + comp_strength:avg_fibre_length + avg_wood_density:avg_fibre_length,
                         data = full_dataframe,
                         family = binomial(link = "logit"))

summary(glm_traits_CITES)


#variable importance of functional traits 

var_imp_traits_CITES <- varImp(glm_traits_CITES, scale = TRUE)
print(var_imp_traits_CITES)

#functional trait variable importance plot 

var_imp_func_traits_CITES <- read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\functional_traits_variable_importance_CITES.csv")


library(ggplot2)


var_imp_func_traits_plot_CITES <- ggplot(var_imp_func_traits_CITES, aes(x = reorder(var, -var.imp), y = var.imp, fill = var.imp)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#8fd0e3", high = "#2d697a") +
  theme_classic() + 
  labs(y = "Importance", x = " ") +
  scale_x_discrete(labels = c("avg_wood_density" = "Wood Density", 
                              "comp_strength" = "Compressive Strength", 
                              "avg_fibre_length" = "Fibre Length",
                              "avg_wood_density:comp_strength" = "Wood Density:Compressive Strength",
                              "comp_strength:avg_fibre_length" = "Fibre Length:Compressive Strength", 
                              "avg_wood_density:avg_fibre_length" = "Wood Density:Fibre Length")) +
  theme(axis.text.x = element_text(angle = 63, hjust = 1),
        legend.position = "none") + 
  theme(axis.text = element_text(size = 17)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) + 
  scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) 

var_imp_func_traits_plot_CITES


ggsave(filename = "var_imp_func_traits_plot_CITES.png", plot = var_imp_func_traits_plot_CITES , path = "C:/Users/ah2255/Documents/part_2_project/outputs")


##################################################################################################

#repeating for volume analysis 


cleaned_traits_and_volume <-read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\averaged_traits_and_volume.csv")


library(MASS)

cleaned_traits_and_volume$avg_wood_density <- scale(cleaned_traits_and_volume$avg_wood_density)
cleaned_traits_and_volume$comp_strength <- scale(cleaned_traits_and_volume$comp_strength)
cleaned_traits_and_volume$avg_fibre_length <- scale(cleaned_traits_and_volume$avg_fibre_length)
cleaned_traits_and_volume$H.cos <- scale(cleaned_traits_and_volume$H.cos)
cleaned_traits_and_volume$H.sin <- scale(cleaned_traits_and_volume$H.sin)
cleaned_traits_and_volume$S <- scale(cleaned_traits_and_volume$S)
cleaned_traits_and_volume$L <- scale(cleaned_traits_and_volume$L)


glm_within_CITES_just_colour <- glm(logvol ~ H.cos:H.sin + S + L + 
                          H.cos:L + H.sin:L + H.cos:S + H.sin:S + L:S,
                        data = cleaned_traits_and_volume,
                        family = gaussian(link = "identity"))

summary(glm_within_CITES_just_colour)

#variable importance 

var_imp_logvol_colour <- varImp(glm_within_CITES_just_colour, scale = TRUE)
print(var_imp_logvol_colour)


#plotting 

var_imp_logvol_colour <- read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\colour_variable_importance_logvol.csv")



var_imp_plot_logvol_colour <- ggplot(var_imp_logvol_colour, aes(x = reorder(var, -var.imp), y = var.imp, fill = var.imp)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#a3e6bf", high = "#2d7a4d") +
  theme_classic() + 
  labs(y = "Importance", x = " ") +
  scale_x_discrete(labels = c("S" = "Saturation", 
                              "L" = "Lightness", 
                              "H.cos:H.sin" = "Hue(Cos Element):Hue(Sin Element)", 
                              "S:L" = "Saturation:Lightness", 
                              "H.cos:L" = "Hue(Cos Element):Lightness", 
                              "H.sin:L" = "Hue(Sin Element):Lightness",
                              "H.cos:S" = "Hue(Cos Element):Saturation",
                              "H.sin:S" = "Hue(Sin Element):Saturation")) +
  theme(axis.text.x = element_text(angle = 63, hjust = 1),
        legend.position = "none") + 
  theme(axis.text = element_text(size = 17)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) + 
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) 

var_imp_plot_logvol_colour


ggsave(filename = "var_imp_plot_logvol_colour.png", plot = var_imp_plot_logvol_colour, path = "C:/Users/ah2255/Documents/part_2_project/outputs")

#################################

#just functional traits 

glm_within_CITES_just_functional <- glm(logvol ~ avg_wood_density + comp_strength + avg_fibre_length +
                                    avg_wood_density:comp_strength + comp_strength:avg_fibre_length + avg_wood_density:avg_fibre_length,
                                  data = cleaned_traits_and_volume,
                                  family = gaussian(link = "identity"))

summary(glm_within_CITES_just_functional)

#variable importance 

var_imp_logvol_func_traits <- varImp(glm_within_CITES_just_functional, scale = TRUE)
print(var_imp_logvol_func_traits)

func_traits_var_imp <- read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\func_traits_importance_logvol.csv")


#plot 


var_imp_plot_CITES <- ggplot(func_traits_var_imp, aes(x = reorder(var, -var.imp), y = var.imp, fill = var.imp)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#a3e6bf", high = "#2d7a4d") +
  theme_classic() + 
  labs(y = "Importance", x = " ") +
  scale_x_discrete(labels = c("avg_wood_density" = "Wood Density", 
                              "comp_strength" = "Compressive Strength", 
                              "avg_fibre_length" = "Fibre Length",
                              "avg_wood_density:comp_strength" = "Wood Density:Compressive Strength",
                              "comp_strength:avg_fibre_length" = "Fibre Length:Compressive Strength", 
                              "avg_wood_density:avg_fibre_length" = "Wood Density:Fibre Length")) +
  theme(axis.text.x = element_text(angle = 63, hjust = 1),
        legend.position = "none") + 
  theme(axis.text = element_text(size = 17)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) + 
  scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) 

var_imp_plot_CITES


ggsave(filename = "func_traits_logvol.png", plot = var_imp_plot_CITES, path = "C:/Users/ah2255/Documents/part_2_project/outputs")


print("CITES Listing Model")
print("Traded Volume Model")




############################################







