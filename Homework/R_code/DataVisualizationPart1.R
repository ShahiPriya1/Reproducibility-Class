install.packages("ggplot2")
install.packages("tidyverse")

library(tidyverse)
mtcars
#The line is in the top of the points
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
 
#the points are on the top
ggplot(mtcars, aes(x=wt, y=mpg, sizw+wr)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point() +
  xlab("Weight(tons)") +
  ylab("Miles per gallon")

#Points acc to weight
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(aes(size = wt)) +
  xlab("Weight(tons)") +
  ylab("Miles per gallon")

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(aes(colour = hp)) +
  xlab("Weight(tons)") +
  ylab("Miles per gallon")+
  scale_color_gradient(low = "green", high="grey")


bull.richness <- read.csv("Bull_richness.csv")
ggplot(bull.richness,aes(x=GrowthStage, y=richness, colour = Fungicide)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge())+
  scale_color_manual(values = c("orange","blue")) # to change color
  
bull.richness <- read.csv("Bull_richness.csv") # Filling the color in boxplot
ggplot(bull.richness,aes(x=GrowthStage, y=richness, fill= Fungicide)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge())+
  scale_fill_manual(values = c("orange","blue"))

#SE
ggplot(bull.richness,aes(x=GrowthStage, y=richness, fill= Fungicide)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge")

#confidence limit
ggplot(bull.richness,aes(x=GrowthStage, y=richness, fill= Fungicide)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = "dodge") 

#
ggplot(bull.richness,aes(x=GrowthStage, y=richness, fill= Fungicide)) +
  stat_summary(fun = mean, geom = "line", position = "dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") 

#line with SE
ggplot(bull.richness,aes(x=GrowthStage, y=richness, group= Fungicide, colour = Fungicide)) +
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_se, geom = "errorbar") 

#facetting
ggplot(bull.richness,aes(x=GrowthStage, y=richness, group= Fungicide, colour = Fungicide)) +
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Treatment*Crop)

ggplot(bull.richness,aes(x=GrowthStage, y=richness, group= Fungicide, colour = Fungicide)) +
  stat_summary(fun = mean, geom = "line")+
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Treatment*Crop,scale= "free")






