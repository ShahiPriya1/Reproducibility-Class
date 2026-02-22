#Notes of Data Visualization Part 2

library(tidyverse)
install.packages("ggpubr")
install.packages("ggrepel")
library(ggpubr)
library(ggrepel)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Stitching
bac <- read.csv("BacterialAlpha.csv", na.strings = "na")
bac$Time_Point <- as.factor(bac$Time_Point)
bac$Crop <- as.factor(bac$Crop)
bac$Crop <- factor(bac$Crop, levels = c("Soil", "Cotton", "Soybean"))

str(bac)
    
#Bacterial evenness 
bac.even <- ggplot(bac, aes(x = Time_Point, y = even, color = Crop)) +  # Define aesthetics: x-axis as Time.Point, y-axis as even, and color by Crop
  geom_boxplot(position = position_dodge(0.85)) +  # Add boxplots with dodged positions to avoid overlap
  geom_point(position = position_jitterdodge(0.05)) +  # Add jittered points to show individual data points, avoiding overlap
  ylab("Pielou's evenness") +  # Label the y-axis
  xlab("Hours post sowing") +  # Label the x-axis
  scale_color_manual(values = cbbPalette, name = "", labels = c("Soil no seeds", "Cotton spermosphere", "Soybean spermosphere")) +  # Manually set colors and labels for the Crop variable
  theme_classic()   # Use a classic theme for the plot
bac.even   

#plot B
bac.even <- ggplot(bac, aes(x = Time_Point, y = even, color = Crop)) +  
  geom_boxplot(position = position_dodge()) +  
  geom_point(position = position_jitterdodge(0.05)) +
  xlab("Time")+
  ylab ("Pielous's evenness")+
  scale_color_manual(values = cbbPalette)+
  theme_classic()
#plot A
bac.nosoil <- subset(bac, Crop != "Soil")
water.imbibed <- ggplot(bac.nosoil, aes(Time_Point, 1000 * Water_Imbibed, color = Crop)) +  # Define aesthetics: x-axis as Time.Point, y-axis as Water_Imbibed (converted to mg), and color by Crop
  geom_jitter(width = 0.5, alpha = 0.5) +  # Add jittered points to show individual data points with some transparency
  stat_summary(fun = mean, geom = "line", aes(group = Crop)) +  # Add lines representing the mean value for each Crop group
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +  # Add error bars representing the standard error of the mean
  xlab("Hours post sowing") +  # Label the x-axis
  ylab("Water Imbibed (mg)") +  # Label the y-axis
  scale_color_manual(values = c(cbbPalette[[2]], cbbPalette[[3]]), name = "", labels = c("", "")) +  # Manually set colors for the Crop variable
  theme_classic() +  # Use a classic theme for the plot
  theme(strip.background = element_blank(), legend.position = "none") +  # Customize theme: remove strip background and position legend to the right
  facet_wrap(~Crop, scales = "free")  # Create separate panels for each Crop, allowing free scales
water.imbibed

#Plot C
water.imbibed.cor <- ggplot(bac.nosoil, aes(y = even, x = 1000 * Water_Imbibed, color = Crop)) +  # Define aesthetics: y-axis as even, x-axis as Water_Imbibed (converted to mg), and color by Crop
  geom_point(aes(shape = Time_Point)) +  # Add points with different shapes based on Time.Point
  geom_smooth(se = FALSE, method = lm) +  # Add a linear model smooth line without confidence interval shading
  xlab("Water Imbibed (mg)") +  # Label the x-axis
  ylab("Pielou's evenness") +  # Label the y-axis
  scale_color_manual(values = c(cbbPalette[[2]], cbbPalette[[3]]), name = "", labels = c("Cotton", "Soybean")) +  # Manually set colors for the Crop variable
  scale_shape_manual(values = c(15, 16, 17, 18), name = "", labels = c("0 hrs", "6 hrs", "12 hrs", "18 hrs")) +  # Manually set shapes for the Time.Point variable
  theme_classic() +  # Use a classic theme for the plot
  theme(strip.background = element_blank(), legend.position = "none") +
  facet_wrap(~Crop, scales = "free")  # Create separate panels for each Crop, allowing free scales

water.imbibed.cor

Figure2 <- ggarrange(water.imbibed, bac.even, water.imbibed.cor, labels = "auto", nrow = 3,
                     ncol = 1, legend = FALSE)
Figure2

#For factorial ANOVA type designs
bac.even + 
  stat_compare_means(method = "anova") # apply an anova to the groups

#Example with pvalues as significance levels
bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.format")

# Example with * as significance levels
bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.signif")

#Example with combined pvalue and * to indicate significance
bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "{p.adj.format}{p.adj.signif}")

#Displaying correlation data
water.imbibed.cor + 
  stat_cor()

#plus regression line
water.imbibed.cor + 
  stat_cor(label.y = 0.7) +
  stat_regline_equation()

#Specific point labeling

diff.abund <- read.csv("diff_abund.csv")

str(diff.abund)

#Make a volcano plot for Soybean vs. Soil

diff.abund$log10_pvalue <- -log10(diff.abund$p_CropSoybean)
diff.abund.label <- diff.abund[diff.abund$log10_pvalue > 30,]

#Make the plot

ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  theme_classic() + 
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean, label = Label))

volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean, label = Label)) + 
  scale_color_manual(values = cbbPalette, name = "Significant") +
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano

#different shapes and color
volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue)) + 
  geom_point(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue), color = "red", shape = 17, size = 4) +
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, label = Label), color = "red") + 
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano

