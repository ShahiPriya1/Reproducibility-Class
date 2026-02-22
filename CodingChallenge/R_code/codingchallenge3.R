library(tidyverse)
install.packages("ggpubr")
install.packages("ggrepel")
library(ggpubr)
library(ggrepel)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Question 1
mycotoxin <- read.csv(file.choose(), na.strings = "na")
head(mycotoxin)
mycotoxin$Treatment <- as.factor(mycotoxin$Treatment) 
mycotoxin$Cultivar <- as.factor(mycotoxin$Cultivar)  
str(mycotoxin)

cbbPalette <- c("#56B4E9", "#009E73")
#DON_PLOT1 
# change the jitter points transparency to 0.6
Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) + # add boxplot layer 
  xlab("") +  
  ylab("DON (ppm)") + # y label 
  geom_point(alpha = 0.6,pch = 21, color = "black") + 
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete  
  facet_wrap(~Cultivar)+ #faceted by Cultivar 
  theme_classic() 
Plot1

#Question 2
#Factor order
mycotoxin$Treatment = factor(mycotoxin$Treatment, levels = c("NTC","Fg", "Fg + 37", "Fg + 40", "Fg + 70" ))
view(mycotoxin$Treatment)


#Question 3
#Plot X15ADON
Plot2 <- ggplot(mycotoxin, aes(x = Treatment, y = X15ADON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) +  
  xlab("") +  
  ylab("15ADON") +  
  geom_point(alpha = 0.6,pch = 21, color = "black") + 
  scale_fill_manual(values = cbbPalette)+ 
  facet_wrap(~Cultivar)+ 
  theme_classic() 
Plot2

#Plot 3

Plot3<- ggplot(mycotoxin, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) + 
geom_boxplot(position = position_dodge(0.85)) +  
  xlab("") +  
  ylab("Seed Mass (mg)") +  
  geom_point(alpha = 0.6,pch = 21, color = "black") + 
  scale_fill_manual(values = cbbPalette)+ 
  facet_wrap(~Cultivar)+ 
  theme_classic() 
Plot3

#Question4

Figure1 <- ggarrange(Plot1, Plot2, Plot3, labels = "AUTO", nrow = 1, ncol = 3, common.legend = T)
Figure1

#Question5

#Plot 1 pairwise comparisons 
Plot1_pwc <- Plot1 +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format") +  # Add pairwise t-tests
  theme_classic()

Plot1_pwc


#Plot 2 pairwise comparisons
Plot2_pwc <- Plot2 +
  geom_pwc(aes(group=Treatment), method= "t.test", label= "p.adj.format")+
  theme_classic()

Plot2_pwc

#Plot 3 pairwise comparisons
Plot3_pwc = Plot3 + geom_pwc(aes(group=Treatment), method="t.test", label="p.adj.format")+
  theme_classic()

Plot3_pwc

#Combine all three plots with a common legend ----
Combined_pwc =ggarrange(
  Plot1_pwc,
  Plot2_pwc,
  Plot3_pwc, labels = "AUTO", nrow = 1, ncol = 3, common.legend = T)

Combined_pwc
