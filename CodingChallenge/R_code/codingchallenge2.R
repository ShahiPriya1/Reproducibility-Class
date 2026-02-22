library(tidyverse)
library(ggpubr)
install.packages("ggpubr")
install.packages("ggrepel")
library(ggrepel)
install.packages(ggplot)                
library(ggplot2)                
library(ggplot)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mycotoxin <- read.csv(file.choose(), na.strings = "na")
head(mycotoxin)
mycotoxin$Treatment <- as.factor(mycotoxin$Treatment) 
mycotoxin$Cultivar <- as.factor(mycotoxin$Cultivar)  
str(mycotoxin)



cbbPalette <- c("#56B4E9", "#009E73")

#DON_PLOT1 

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) + # add boxplot layer 
  xlab("") +  
  ylab("DON (ppm)") + # y label 
  geom_point(alpha = 0.6,pch = 21, color = "black", position = position_jitterdodge(0.05)) + 
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete  
  facet_wrap(~Cultivar)+ #faceted by Cultivar 
  theme_classic() # for classic theme  
Plot1

Plot1_bar <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               position = position_dodge(0.9), 
               width = 0.8) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(0.9), 
               width = 0.2) +
  xlab("") +
  ylab("DON (ppm)") +
  scale_fill_manual(values = cbbPalette) +
  facet_wrap(~Cultivar) +
  theme_classic()

Plot1_bar

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +  # Define aesthetics: x-axis as Treatment, y-axis as DON, and fill by Cultivar
  stat_summary(fun = mean, geom = "bar", position = position_dodge(0.9)) +  # Add bars representing mean DON values
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3, 
               position = position_dodge(0.9)) +  # Add error bars representing the standard error of the mean
  xlab("") +  # Label the x-axis
  ylab("DON (ppm)") +  # Label the y-axis
  scale_fill_manual(values = cbbPalette) +  # Manually set fill colors for Cultivar from the cbbPalette
  theme_classic() +  # Use a classic theme for the plot
  facet_wrap(~Cultivar)  # Create separate panels for each Cultivar

Plot1


##Question 2 — Boxplot with distribution points

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) +  # add boxplot layer
  
  geom_point(shape = 21, color = "black",
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.85)) +  # add jittered points
  
  xlab("") +  
  ylab("DON (ppm)") +  
  scale_fill_manual(values = cbbPalette) +  
  theme_classic()

Plot1



#Question 3 — Bar chart (mean ± SE) with distribution points

Plot1_bar <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(0.9),
               width = 0.8) +
  
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position = position_dodge(0.9),
               width = 0.2) +
  
  geom_point(shape = 21, color = "black",
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.9)) +  # add jittered points
  
  xlab("") +
  ylab("DON (ppm)") +
  scale_fill_manual(values = cbbPalette) +
  theme_classic()

Plot1_bar


## — Boxplot with points and facet by Cultivar

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) +  
  geom_point(shape = 21, color = "black",
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.85)) +  
  xlab("") +  
  ylab("DON (ppm)") +  
  scale_fill_manual(values = cbbPalette) +  
  facet_wrap(~Cultivar) +  # facet by cultivar
  theme_classic()

Plot1


#Question 3 — Bar chart (mean ± SE) with points and facet by Cultivar

Plot1_bar <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(0.9),
               width = 0.8) +
  
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position = position_dodge(0.9),
               width = 0.2) +
  
  geom_point(shape = 21, color = "black",
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.9)) +
  
  xlab("") +
  ylab("DON (ppm)") +
  scale_fill_manual(values = cbbPalette) +
  facet_wrap(~Cultivar) +  # facet by cultivar
  theme_classic()

Plot1_bar



#Boxplot with transparent points and facet

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) +  
  geom_point(shape = 21, 
             color = "black",
             alpha = 0.5,   # transparency added
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.85)) +  
  xlab("") +  
  ylab("DON (ppm)") +  
  scale_fill_manual(values = cbbPalette) +  
  facet_wrap(~Cultivar) +  
  theme_classic()

Plot1




## Bar chart (mean ± SE) with transparent points and facet
Plot1_bar <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(0.9),
               width = 0.8) +
  
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position = position_dodge(0.9),
               width = 0.2) +
  
  geom_point(shape = 21, 
             color = "black",
             alpha = 0.5,   # transparency added
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.9)) +
  
  xlab("") +
  ylab("DON (ppm)") +
  scale_fill_manual(values = cbbPalette) +
  facet_wrap(~Cultivar) +
  theme_classic()

Plot1_bar
