library(ggplot2)
library(ggpubr)
# DON data
DON_data <- read.csv("MycotoxinData.csv", na.strings = "na")
str(DON_data)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Question 1
DON_plot <- ggplot(DON_data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(outliers = T, outlier.shape = 15) +
  ylab("DON (ppm)") +
  xlab("") +
  geom_jitter(pch = 21, position = position_jitterdodge(), color = "black", alpha = 0.6) + 
  scale_fill_manual(values = c(cbbPalette[[3]], cbbPalette[[4]])) +
  theme_classic() +
  facet_wrap(~Cultivar)
DON_plot

# Question 2
DON_data$Treatment <- factor(DON_data$Treatment, levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70"))

# Question 3
DON_15 <- ggplot(DON_data, aes(x = Treatment, y = X15ADON, fill = Cultivar)) +
  geom_boxplot(outliers = F) +
  ylab("DON (ppm)") +
  xlab("") +
  geom_jitter(pch = 21, position = position_jitterdodge(), color = "black") + 
  scale_fill_manual(values = c(cbbPalette[[3]], cbbPalette[[4]])) +
  theme_classic() +
  facet_wrap(~Cultivar)
DON_15

seedmass <- ggplot(DON_data, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) +
  geom_boxplot(outliers = F) +
  ylab("DON (ppm)") +
  xlab("") +
  geom_jitter(pch = 21, position = position_jitterdodge(), color = "black") + 
  scale_fill_manual(values = c(cbbPalette[[3]], cbbPalette[[4]])) +
  theme_classic() +
  facet_wrap(~Cultivar)
seedmass

# Question 4
ggarrange(DON_plot, DON_15, seedmass, labels = "auto", ncol = 3, nrow = 1, common.legend = T)

# Question 5 - p.adj.format - is the corrected pvalues - what is the default? 
stats_donplot <- DON_plot + 
  geom_pwc(aes(group = Treatment), method = "t_test", label = "{p.adj.format}{p.adj.signif}", p.adjust.method = "fdr")

stats_DON_15 <- DON_15 + 
  geom_pwc(aes(group = Treatment), method = "t_test", label = "{p.adj.format}{p.adj.signif}", p.adjust.method = "fdr")

stats_seedmass <- seedmass + 
  geom_pwc(aes(group = Treatment), method = "t_test", label = "{p.adj.format}{p.adj.signif}", p.adjust.method = "fdr")

ggarrange(stats_donplot, stats_DON_15, stats_seedmass, labels = "auto", ncol = 3, nrow = 1, common.legend = T)

