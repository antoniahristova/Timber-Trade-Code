install.packages("mgcv")
library(mgcv)
library(dplyr)
library(ggplot2)

full_dataframe <- read.csv("C:\\Users\\ah2255\\Documents\\part_2_project\\outputs\\full_dataframe_for_glm.csv")

GAM_hue <- gam(CITES ~s(H, bs = "cc"), data =
                 full_dataframe, family = binomial())

summary(GAM_hue)




numbers <- seq(0, 360, 1)
predicted <- predict(GAM_hue,
                     newdata = data.frame(H = numbers),
                     type = "link", se = TRUE )

model_se <- data.frame(H = numbers,
                       estimate = boot::inv.logit(predicted$fit),
                       confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                       confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_plot <- ggplot(model_se, aes(H, estimate)) +
  geom_rect(aes(xmin = 0, xmax = 10, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c2523d") +
  geom_rect(aes(xmin = 10, xmax = 20, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c25e3d") +
  geom_rect(aes(xmin = 20, xmax = 30, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c2743d") +
  geom_rect(aes(xmin = 30, xmax = 40, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c28b3d") +
  geom_rect(aes(xmin = 40, xmax = 50, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c2a13d") +
  geom_rect(aes(xmin = 50, xmax = 60, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c2b73d") +
  geom_rect(aes(xmin = 60, xmax = 70, ymin =-Inf, ymax = Inf), colour = NA, fill = "#b7c23d") +
  geom_rect(aes(xmin = 70, xmax = 80, ymin =-Inf, ymax = Inf), colour = NA, fill = "#a1c23d") +
  geom_rect(aes(xmin = 80, xmax = 90, ymin =-Inf, ymax = Inf), colour = NA, fill = "#8bc23d") +
  geom_rect(aes(xmin = 90, xmax = 100, ymin =-Inf, ymax = Inf), colour = NA, fill = "#74c23d") +
  geom_rect(aes(xmin = 100, xmax = 110, ymin =-Inf, ymax = Inf), colour = NA, fill = "#5ec23d") +
  geom_rect(aes(xmin = 110, xmax = 120, ymin =-Inf, ymax = Inf), colour = NA, fill = "#48c23d") +
  geom_rect(aes(xmin = 120, xmax = 130, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc248") +
  geom_rect(aes(xmin = 130, xmax = 140, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc25e") +
  geom_rect(aes(xmin = 140, xmax = 150, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc274") +
  geom_rect(aes(xmin = 150, xmax = 160, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc28b") +
  geom_rect(aes(xmin = 160, xmax = 170, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc2a1") +
  geom_rect(aes(xmin = 170, xmax = 180, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc2b7") +
  geom_rect(aes(xmin = 180, xmax = 190, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3db7c2") +
  geom_rect(aes(xmin = 190, xmax = 200, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3da1c2") +
  geom_rect(aes(xmin = 200, xmax = 210, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3d8bc2") +
  geom_rect(aes(xmin = 210, xmax = 220, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3d74c2") +
  geom_rect(aes(xmin = 220, xmax = 230, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3d5ec2") +
  geom_rect(aes(xmin = 230, xmax = 240, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3d48c2") +
  geom_rect(aes(xmin = 240, xmax = 250, ymin =-Inf, ymax = Inf), colour = NA, fill = "#483dc2") +
  geom_rect(aes(xmin = 250, xmax = 260, ymin =-Inf, ymax = Inf), colour = NA, fill = "#5e3dc2") +
  geom_rect(aes(xmin = 260, xmax = 270, ymin =-Inf, ymax = Inf), colour = NA, fill = "#743dc2") +
  geom_rect(aes(xmin = 270, xmax = 280, ymin =-Inf, ymax = Inf), colour = NA, fill = "#8b3dc2") +
  geom_rect(aes(xmin = 280, xmax = 290, ymin =-Inf, ymax = Inf), colour = NA, fill = "#a13dc2") +
  geom_rect(aes(xmin = 290, xmax = 300, ymin =-Inf, ymax = Inf), colour = NA, fill = "#b73dc2") +
  geom_rect(aes(xmin = 300, xmax = 310, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23db7") +
  geom_rect(aes(xmin = 310, xmax = 320, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23da1") +
  geom_rect(aes(xmin = 310, xmax = 320, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23da1") +
  geom_rect(aes(xmin = 320, xmax = 330, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23d8b") +
  geom_rect(aes(xmin = 330, xmax = 340, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23d74") +
  geom_rect(aes(xmin = 340, xmax = 350, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23d5e") +
  geom_rect(aes(xmin = 350, xmax = 360, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23d48") +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#ece7e0", alpha = 0.5) +
  geom_line() +
  theme_classic() + 
  theme(axis.text = element_text(size = 19)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)),  
        axis.title.y = element_text(margin = margin(r = 20))) + 
  scale_x_continuous(limits = c(0, 360), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Hue", y = "P(CITES Listed)") 

model_plot


ggsave(filename = "gamma.png", plot = model_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")


#gratia package 

install.packages("gratia")


library(ggplot2)
library(gratia)


pred.dat = data.frame(H = 1:360)
fd <- fderiv(GAM_hue, newdata = pred.dat, term = "H")
ci <- confint(fd, type = "confidence")
fd.plot1 <- cbind(pred.dat, ci)

slope_plot <- ggplot(fd.plot1, aes(x = H, y = est)) +
  geom_rect(aes(xmin = 0, xmax = 10, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c2523d") +
  geom_rect(aes(xmin = 10, xmax = 20, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c25e3d") +
  geom_rect(aes(xmin = 20, xmax = 30, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c2743d") +
  geom_rect(aes(xmin = 30, xmax = 40, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c28b3d") +
  geom_rect(aes(xmin = 40, xmax = 50, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c2a13d") +
  geom_rect(aes(xmin = 50, xmax = 60, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c2b73d") +
  geom_rect(aes(xmin = 60, xmax = 70, ymin =-Inf, ymax = Inf), colour = NA, fill = "#b7c23d") +
  geom_rect(aes(xmin = 70, xmax = 80, ymin =-Inf, ymax = Inf), colour = NA, fill = "#a1c23d") +
  geom_rect(aes(xmin = 80, xmax = 90, ymin =-Inf, ymax = Inf), colour = NA, fill = "#8bc23d") +
  geom_rect(aes(xmin = 90, xmax = 100, ymin =-Inf, ymax = Inf), colour = NA, fill = "#74c23d") +
  geom_rect(aes(xmin = 100, xmax = 110, ymin =-Inf, ymax = Inf), colour = NA, fill = "#5ec23d") +
  geom_rect(aes(xmin = 110, xmax = 120, ymin =-Inf, ymax = Inf), colour = NA, fill = "#48c23d") +
  geom_rect(aes(xmin = 120, xmax = 130, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc248") +
  geom_rect(aes(xmin = 130, xmax = 140, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc25e") +
  geom_rect(aes(xmin = 140, xmax = 150, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc274") +
  geom_rect(aes(xmin = 150, xmax = 160, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc28b") +
  geom_rect(aes(xmin = 160, xmax = 170, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc2a1") +
  geom_rect(aes(xmin = 170, xmax = 180, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3dc2b7") +
  geom_rect(aes(xmin = 180, xmax = 190, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3db7c2") +
  geom_rect(aes(xmin = 190, xmax = 200, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3da1c2") +
  geom_rect(aes(xmin = 200, xmax = 210, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3d8bc2") +
  geom_rect(aes(xmin = 210, xmax = 220, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3d74c2") +
  geom_rect(aes(xmin = 220, xmax = 230, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3d5ec2") +
  geom_rect(aes(xmin = 230, xmax = 240, ymin =-Inf, ymax = Inf), colour = NA, fill = "#3d48c2") +
  geom_rect(aes(xmin = 240, xmax = 250, ymin =-Inf, ymax = Inf), colour = NA, fill = "#483dc2") +
  geom_rect(aes(xmin = 250, xmax = 260, ymin =-Inf, ymax = Inf), colour = NA, fill = "#5e3dc2") +
  geom_rect(aes(xmin = 260, xmax = 270, ymin =-Inf, ymax = Inf), colour = NA, fill = "#743dc2") +
  geom_rect(aes(xmin = 270, xmax = 280, ymin =-Inf, ymax = Inf), colour = NA, fill = "#8b3dc2") +
  geom_rect(aes(xmin = 280, xmax = 290, ymin =-Inf, ymax = Inf), colour = NA, fill = "#a13dc2") +
  geom_rect(aes(xmin = 290, xmax = 300, ymin =-Inf, ymax = Inf), colour = NA, fill = "#b73dc2") +
  geom_rect(aes(xmin = 300, xmax = 310, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23db7") +
  geom_rect(aes(xmin = 310, xmax = 320, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23da1") +
  geom_rect(aes(xmin = 310, xmax = 320, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23da1") +
  geom_rect(aes(xmin = 320, xmax = 330, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23d8b") +
  geom_rect(aes(xmin = 330, xmax = 340, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23d74") +
  geom_rect(aes(xmin = 340, xmax = 350, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23d5e") +
  geom_rect(aes(xmin = 350, xmax = 360, ymin =-Inf, ymax = Inf), colour = NA, fill = "#c23d48") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#ece7e0", alpha = 0.5) + 
  geom_line(color = "red", linewidth = 1) +  
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "dashed") + 
  scale_x_continuous(limits = c(0, 360), expand = c(0, 0)) +
  labs(x = "Hue", y = "First Derivative") +
  theme_classic() +
  theme(axis.text = element_text(size = 19)) +
  theme(axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 20)), 
        axis.title.y = element_text(margin = margin(r = 20))) +
  scale_x_continuous(limits = c(0,40))

slope_plot




ggsave(filename = "gammafirstdevALL.png", plot = slope_plot, path = "C:/Users/ah2255/Documents/part_2_project/outputs")

