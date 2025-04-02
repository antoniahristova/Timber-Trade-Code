#step one - creation of a dataframe with all of the data for the glm and a binary 0/1 for CITES/non CITES

full_dataframe_for_glm_no_colour <- read.csv("C:/Users/ah2255/Documents/part_2_project/imputation_four_rf.csv")
hsl_for_colour <- read.csv("C:/Users/ah2255/Documents/part_2_project/outputs/hsl_for_colour.csv")


#join to include the colour data too: 

library(dplyr)

full_dataframe_for_glm <- full_dataframe_for_glm_no_colour %>%
  left_join(hsl_for_colour %>% select(Taxon, H, S, L), by = "Taxon")

write.csv(full_dataframe_for_glm, "C:/Users/ah2255/Documents/part_2_project/outputs/full_dataframe_for_glm.csv")
#manual addition of CITES/notCITES column

full_dataframe_for_glm_final <- read.csv("C:/Users/ah2255/Documents/part_2_project/outputs/full_dataframe_for_glm.csv")


library(dplyr)
library(MASS)


glm_just_density <- glm(CITES ~ avg_wood_density,
                        data = full_dataframe_for_glm_final,
                        family = binomial(link = logit))

summary(glm_just_density)

glm_just_compressive <- glm(CITES ~ comp_strength,
                            data = full_dataframe_for_glm_final,
                            family = binomial(link = logit))


summary (glm_just_compressive)

glm_just_fibre_length <- glm(CITES ~ avg_fibre_length,
                        data = full_dataframe_for_glm_final,
                        family = binomial(link = logit))

summary(glm_just_fibre_length)

glm_just_hue <- glm(CITES ~ H,
                    data = full_dataframe_for_glm_final,
                    family = binomial(link = logit))

summary(glm_just_hue)

glm_just_saturation <- glm(CITES ~ S,
                           data = full_dataframe_for_glm_final,
                           family = binomial(link = logit))

summary(glm_just_saturation)

glm_just_lightness <- glm(CITES ~ L,
                          data = full_dataframe_for_glm_final,
                          family = binomial(link = logit))

summary(glm_just_lightness)



##########################################################################
glm_just_lightness

numbers <- seq(0, 1, 0.001)
predicted <- predict(glm_just_lightness,
                     newdata = data.frame(L = numbers),
                     type = "link", se = TRUE )

model_se <- data.frame(L = numbers,
                       estimate = boot::inv.logit(predicted$fit),
                       confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                       confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_plot <- ggplot(model_se, aes(L, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#E3F2FD") +
  geom_line() +
  theme_classic() + 
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Lightness", y = "P(CITES Listed)")

model_plot


ggsave(filename = "Lightness.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")


###############################



numbers <- seq(0, 1, 0.001)
predicted <- predict(glm_just_saturation,
                     newdata = data.frame(S = numbers),
                     type = "link", se = TRUE )

model_se <- data.frame(S = numbers,
                       estimate = boot::inv.logit(predicted$fit),
                       confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                       confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_plot <- ggplot(model_se, aes(S, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#E3F2FD") +
  geom_line() +
  theme_classic() + 
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(0, 1.01), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Saturation", y = "P(CITES Listed)")

model_plot


ggsave(filename = "Saturation.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")


########################################################################



numbers <- seq(0, 110, 1)
predicted <- predict(glm_just_compressive,
                     newdata = data.frame(comp_strength = numbers),
                     type = "link", se = TRUE )

model_se <- data.frame(comp_strength = numbers,
                       estimate = boot::inv.logit(predicted$fit),
                       confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                       confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_plot <- ggplot(model_se, aes(comp_strength, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#E3F2FD") +
  geom_line() +
  theme_classic() + 
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(0, 110), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Compressive Strength", y = "P(CITES Listed)")

model_plot


ggsave(filename = "Compressive_strength.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")

######################################


numbers <- seq(380, 3100, 100)
predicted <- predict(glm_just_fibre_length,
                     newdata = data.frame(avg_fibre_length = numbers),
                     type = "link", se = TRUE )

model_se <- data.frame(avg_fibre_length = numbers,
                       estimate = boot::inv.logit(predicted$fit),
                       confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                       confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_plot <- ggplot(model_se, aes(avg_fibre_length, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#E3F2FD") +
  geom_line() +
  theme_classic() + 
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(380, 3100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Fibre Length", y = "P(CITES Listed)")

model_plot


ggsave(filename = "fibre_length.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")

#####################################################


density_plot <- ggplot(full_dataframe_for_glm_final, aes(avg_wood_density, CITES))+
  theme_classic() +
  geom_jitter()

density_plot


  
numbers <- seq(0, 1.5, 0.01)
predicted <- predict(glm_just_density,
                     newdata = data.frame(avg_wood_density = numbers),
                     type = "link", se = TRUE )

model_se <- data.frame(avg_wood_density = numbers,
                       estimate = boot::inv.logit(predicted$fit),
                       confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                       confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_plot <- ggplot(model_se, aes(avg_wood_density, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#E3F2FD") +
  geom_line() +
  theme_classic() + 
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(0, 1.54), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Wood Density", y = "P(CITES Listed)")

model_plot


ggsave(filename = "density.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")


###############################################################l







