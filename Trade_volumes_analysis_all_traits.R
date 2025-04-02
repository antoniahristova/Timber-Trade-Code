 

CITES_trade_volumes <- read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\Exp_reported_vol.csv")
full_dataframe_CITES_GLM <- read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\full_dataframe_for_glm.csv")

#joining with the other data - using the trade volumes as the framework 


full_traits_and_volume <- CITES_trade_volumes %>% 
left_join(full_dataframe_CITES_GLM, by = c("Taxon" = "Taxon"), relationship = "many-to-many") 

write.csv(full_traits_and_volume, "C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\full_traits_and_volume.csv")

library(dplyr)

full_traits_and_volume <- full_traits_and_volume %>% distinct(X.x, .keep_all = TRUE)

write.csv(full_traits_and_volume, "C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\full_traits_and_volume.csv")

##########################################################

cleaned_traits_and_volume <- read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\full_traits_and_volume.csv")

library(dplyr)

cleaned_traits_and_volume <- cleaned_traits_and_volume %>%
  group_by(Taxon) %>% 
  summarize(Volume = sum(Volume, na.rm = TRUE), .groups = "drop")

write.csv(cleaned_traits_and_volume, "C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\averaged_traits_and_volume.csv")

cleaned_traits_and_volume <-read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\averaged_traits_and_volume.csv")


#converting to z scores to standardise: 
library(MASS)

cleaned_traits_and_volume$avg_wood_density <- scale(cleaned_traits_and_volume$avg_wood_density)
cleaned_traits_and_volume$comp_strength <- scale(cleaned_traits_and_volume$comp_strength)
cleaned_traits_and_volume$avg_fibre_length <- scale(cleaned_traits_and_volume$avg_fibre_length)
cleaned_traits_and_volume$H.cos <- scale(cleaned_traits_and_volume$H.cos)
cleaned_traits_and_volume$H.sin <- scale(cleaned_traits_and_volume$H.sin)
cleaned_traits_and_volume$S <- scale(cleaned_traits_and_volume$S)
cleaned_traits_and_volume$L <- scale(cleaned_traits_and_volume$L)


########################################################

#P(traded) plots 

#GLM just for Saturation - using non-scaled data as there is just one predictor 


##############################

library(ggplot2)

plot(cleaned_traits_and_volume$logvol, cleaned_traits_and_volume$S)


CITES_glm_just_saturation <- glm(logvol ~ S,
                                 data = cleaned_traits_and_volume,
                                 family = gaussian(link = "identity"))

summary(CITES_glm_just_saturation)



numbers <- seq(0, 1, 0.01)
newdata <- data.frame(S = numbers)


predicted <- predict(CITES_glm_just_saturation, newdata = newdata, type = "link", se.fit = TRUE)

model_se <- data.frame(
  S = numbers,
  estimate = predicted$fit,  
  confidence_high = predicted$fit + 1.96 * predicted$se.fit,
  confidence_low = predicted$fit - 1.96 * predicted$se.fit
)



model_plot <- ggplot(model_se, aes(S, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#a8d7a7", alpha = 0.5) +
  geom_line(color = "black") +
  theme_classic() +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 6.01)) +
  labs(x = "Saturation", y = "Predicted Volume (Log)")

model_plot


ggsave(filename = "satCITESplot.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")


###################################################################

#just lightness 


library(ggplot2)


CITES_glm_just_lightness <- glm(logvol ~ L,
                                 data = cleaned_traits_and_volume,
                                 family = gaussian(link = "identity"))

summary(CITES_glm_just_lightness)


numbers <- seq(0, 1, 0.01)
newdata <- data.frame(L = numbers)


predicted <- predict(CITES_glm_just_lightness, newdata = newdata, type = "link", se.fit = TRUE)


model_se <- data.frame(
  L = numbers,
  estimate = predicted$fit,  
  confidence_high = predicted$fit + 1.96 * predicted$se.fit,  
  confidence_low =  predicted$fit - 1.96 * predicted$se.fit   
)



model_plot <- ggplot(model_se, aes(L, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#a8d7a7", alpha = 0.5) +
  geom_line(color = "black") +
  theme_classic() +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(0.1, 1.03), expand = c(0, 0)) +
  labs(x = "Lightness", y = "Predicted Volume (Log)")

model_plot


ggsave(filename = "lightnessCITESplot.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")

#################################################

#just density 



library(ggplot2)


CITES_glm_just_density <- glm(logvol ~ avg_wood_density,
                                data = cleaned_traits_and_volume,
                                family = gaussian(link = "identity"))

summary(CITES_glm_just_density)


numbers <- seq(0, 1.5, 0.01)
newdata <- data.frame(avg_wood_density = numbers)


predicted <- predict(CITES_glm_just_density, newdata = newdata, type = "link", se.fit = TRUE)


model_se <- data.frame(
  avg_wood_density = numbers,
  estimate = predicted$fit,  
  confidence_high = predicted$fit + 1.96 * predicted$se.fit,  
  confidence_low =  predicted$fit - 1.96 * predicted$se.fit  
)


model_plot <- ggplot(model_se, aes(avg_wood_density, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#a8d7a7", alpha = 0.5) +
  geom_line(color = "black") +
  theme_classic() +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(0, 1.53), expand = c(0, 0)) +
  labs(x = "Wood Density", y = "Predicted Volume (Log)")

model_plot


ggsave(filename = "DensityCITESplot.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")


#############################

#just compressive strength 

library(ggplot2)


CITES_glm_just_compressive <- glm(logvol ~ comp_strength,
                              data = cleaned_traits_and_volume,
                              family = gaussian(link = "identity"))


summary(CITES_glm_just_compressive)

numbers <- seq(0, 110, 1)
newdata <- data.frame(comp_strength = numbers)


predicted <- predict(CITES_glm_just_compressive, newdata = newdata, type = "link", se.fit = TRUE)


model_se <- data.frame(
  comp_strength = numbers,
  estimate = predicted$fit,  
  confidence_high = predicted$fit + 1.96 * predicted$se.fit,  
  confidence_low = predicted$fit - 1.96 * predicted$se.fit   
)


model_plot <- ggplot(model_se, aes(comp_strength, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#a8d7a7", alpha = 0.5) +
  geom_line(color = "black") +
  theme_classic() +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(0, 110), expand = c(0, 0)) +
  labs(x = "Compressive Strength", y = "Predicted Volume (Log)")

model_plot


ggsave(filename = "CompressiveCITESplot.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")

######################


#just fibre length 


library(ggplot2)


CITES_glm_just_fibre <- glm(logvol ~ avg_fibre_length,
                                  data = cleaned_traits_and_volume,
                                  family = gaussian(link = "identity"))

summary(CITES_glm_just_fibre)


numbers <- seq(380, 3100, 100)
newdata <- data.frame(avg_fibre_length = numbers)


predicted <- predict(CITES_glm_just_fibre, newdata = newdata, type = "link", se.fit = TRUE)


model_se <- data.frame(
  avg_fibre_length = numbers,
  estimate = predicted$fit,  
  confidence_high = predicted$fit + 1.96 * predicted$se.fit,  
  confidence_low = predicted$fit - 1.96 * predicted$se.fit   
)


model_plot <- ggplot(model_se, aes(avg_fibre_length, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#a8d7a7", alpha = 0.5) +
  geom_line(color = "black") +
  theme_classic() +
  scale_x_continuous(limits = c(380, 3100), expand = c(0, 0)) +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) +
  labs(x = "Fibre Length", y = "Predicted Volume (Log)")

model_plot


ggsave(filename = "FibreCITESplot.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")

###########################################################################

#hue GAM 

#########################################################


library(mgcv)
library(dplyr)
library(ggplot2)


cleaned_traits_and_volume <- cleaned_traits_and_volume %>%
  mutate(H_shifted = ifelse(H > 350, H - 360, H))  # Shift 354 to -6 for plotting 


GAM_hue <- gam(logvol ~ s(H_shifted, bs = "cc"), 
               data = cleaned_traits_and_volume, 
               family = gaussian(link = "identity"))

summary(GAM_hue)

numbers <- seq(-10, 40, length.out = 100)  # -10 ensures 354 is included


predicted <- predict(GAM_hue,
                     newdata = data.frame(H_shifted = numbers),
                     type = "link", se = TRUE)


model_se <- data.frame(
  H_shifted = numbers,
  estimate = predicted$fit,
  confidence_high = predicted$fit + 1.96 * predicted$se.fit,
  confidence_low = predicted$fit - 1.96 * predicted$se.fit
)



model_plot <- ggplot(model_se, aes(H_shifted, estimate)) +
  scale_x_continuous(
    breaks = c(-10, 0, 10, 20, 30, 40),  
    labels = c("350", "0", "10", "20", "30", "40"),
    expand = c(0, 0),
    limits = c(-10, 40.7)
  ) +
  geom_rect(aes(xmin = -10, xmax = 0, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf404b") +
  geom_rect(aes(xmin = 0, xmax = 11, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf4b40") +
  geom_rect(aes(xmin = 11, xmax = 17, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf5e40") +
  geom_rect(aes(xmin = 17, xmax = 20, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf6c40") +
  geom_rect(aes(xmin = 17, xmax = 23, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf6a40") +
  geom_rect(aes(xmin = 23, xmax = 29, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf7740") +
  geom_rect(aes(xmin = 29, xmax = 37, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf8640") +
  geom_rect(aes(xmin = 37, xmax = 40, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf9140") +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#ece7e0", alpha = 0.5) +
  geom_line(color = "black") +
  theme_classic() +
  theme(axis.text = element_text(size = 19)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) + 
  labs(x = "Hue", y = "Predicted Log Volume")




model_plot

ggsave(filename = "gammaCITES.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")





################################################################

#first deriv plot 
library(gratia)
library(ggplot2)

pred.dat <- data.frame(H = 0:360)

pred.dat$H_shifted <- ifelse(pred.dat$H > 350, pred.dat$H - 360, pred.dat$H)

fd <- fderiv(GAM_hue, newdata = pred.dat, term = "H_shifted")


ci <- confint(fd, type = "confidence")


fd.plot1 <- cbind(pred.dat, ci)


slope_plot <- ggplot(fd.plot1, aes(x = H_shifted, y = est)) +  
  scale_x_continuous(limits = c(-9, 40.7),
                     breaks = c(-9, 0, 10, 20, 30, 40),  
                     labels = c("350", "0", "10", "20", "30", "40"),
                     expand = c(0, 0)) +
  geom_rect(aes(xmin = -9, xmax = 0, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf404b") +
  geom_rect(aes(xmin = 0, xmax = 11, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf4b40") +
  geom_rect(aes(xmin = 11, xmax = 17, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf5e40") +
  geom_rect(aes(xmin = 17, xmax = 20, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf6c40") +
  geom_rect(aes(xmin = 17, xmax = 23, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf6a40") +
  geom_rect(aes(xmin = 23, xmax = 29, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf7740") +
  geom_rect(aes(xmin = 29, xmax = 37, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf8640") +
  geom_rect(aes(xmin = 37, xmax = 40, ymin =-Inf, ymax = Inf), colour = NA, fill = "#bf9140") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "#ffffff") +
  geom_line(color = "red", linewidth = 1) +  
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "dashed") +  
  theme_classic() +
  theme(axis.text = element_text(size = 19)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) + 
  labs(x = "Hue", y = "First Derivative") 




slope_plot


ggsave(filename = "gammafirstdev.png", plot = slope_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")


