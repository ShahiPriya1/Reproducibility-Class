install.packages("ggplot2")
library(ggplot2)

sample.data.bac <- read.csv(file.choose(), na.strings = "na")
head(sample.data.bac)
sample.data.bac$Time_Point <- as.factor(sample.data.bac$Time_Point)
sample.data.bac$Crop <- as.factor(sample.data.bac$Crop)
sample.data.bac$Crop <- factor(sample.data.bac$Crop, levels = c("Soil", "Cotton", "Soybean"))

str(sample.data.bac)

####### Bacterial evenness; Figure 2a ########
bac.even <- ggplot(sample.data.bac, aes(x = Time_Point, y = even, color = Crop)) +  # Define aesthetics: x-axis as Time.Point, y-axis as even, and color by Crop
  geom_boxplot(position = position_dodge(0.85)) +  # Add boxplots with dodged positions to avoid overlap
  geom_point(position = position_jitterdodge(0.05)) +  # Add jittered points to show individual data points, avoiding overlap
  ylab("Pielou's evenness") +  # Label the y-axis
  xlab("Hours post sowing") +  # Label the x-axis
  scale_color_manual(values = cbbPalette, name = "", labels = c("Soil no seeds", "Cotton spermosphere", "Soybean spermosphere")) +  # Manually set colors and labels for the Crop variable
  theme_classic()   # Use a classic theme for the plot
bac.even

bac.even <- ggplot(sample.data.bac, 
                   aes(x = Time_Point, y = even, color = Crop)) +
  geom_boxplot(position = position_dodge(0.85)) +
  geom_point(shape = 21, fill = "white", color = "black",
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.85)) +
  ylab("Pielou's evenness") +
  xlab("Hours post sowing") +
  scale_color_manual(
    values = c("#E69F00", "#56B4E9", "#009E73"),  # three distinct colors
    name = "",
    labels = c("Soil no seeds", "Cotton spermosphere", "Soybean spermosphere")
  ) +
  theme_classic()

bac.even



####### Water Imbibition correlate with bacterial evenness; Figure 2b #####

sample.data.bac.nosoil <- subset(sample.data.bac, Crop != "Soil")

water.imbibed <- ggplot(sample.data.bac.nosoil, 
                        aes(Time_Point, 1000 * Water_Imbibed, color = Crop)) +  
  geom_jitter(width = 0.5, alpha = 0.5) +  
  stat_summary(fun = mean, geom = "line", aes(group = Crop)) +  
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +  
  xlab("Hours post sowing") +  
  ylab("Water Imbibed (mg)") +  
  scale_color_manual(
    values = c("#56B4E9", "#009E73"),  # two valid colors
    name = "",
    labels = c("Cotton", "Soybean")
  ) +  
  theme_classic() +  
  theme(strip.background = element_blank(), legend.position = "none") +  
  facet_wrap(~Crop, scales = "free")

water.imbibed



##### Figure 2c #####
water.imbibed.cor <- ggplot(sample.data.bac.nosoil,
                            aes(x = 1000 * Water_Imbibed,
                                y = even,
                                color = Crop)) +
  
  geom_point(aes(shape = Time_Point), size = 3) +   # Points indexed by Time_Point
  
  geom_smooth(method = lm,
              se = FALSE,
              aes(group = Crop)) +   # One regression line per Crop
  
  xlab("Water Imbibed (mg)") +
  ylab("Pielou's evenness") +
  
  scale_color_manual(
    values = c(cbbPalette[[1]], cbbPalette[[2]]),
    name = ""
  ) +
  
  scale_shape_manual(
    values = c(15, 16, 17, 18),
    name = "",
    labels = c("0 hrs", "6 hrs", "12 hrs", "18 hrs")
  ) +
  
  theme_classic() +
  theme(strip.background = element_blank()) +
  
  facet_wrap(~Crop, scales = "free")

water.imbibed.cor




##Figure 2; significance levels added with Adobe or powerpoint #### 

install.packages(ggpubr)
library(ggpubr)
# Arrange multiple ggplot objects into a single figure
figure2 <- ggarrange(
  water.imbibed,  # First plot: water.imbibed
  bac.even,  # Second plot: bac.even
  water.imbibed.cor,  # Third plot: water.imbibed.cor
  labels = "auto",  # Automatically label the plots (A, B, C, etc.)
  nrow = 3,  # Arrange the plots in 3 rows
  ncol = 1,  # Arrange the plots in 1 column
  legend = FALSE  # Do not include a legend in the combined figure
)

figure2


bac.even + 
  stat_compare_means(method = "anova") # apply an anova to the groups



### Example with pvalues as significance levels
bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.format")



### example with * as significance levels
bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.signif")


### Example with combined pvalue and * to indicate significance
bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "{p.adj.format}{p.adj.signif}")


water.imbibed.cor + 
  stat_cor()



water.imbibed.cor + 
  stat_cor(label.y = 0.7) +
  stat_regline_equation()
##,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

"C:\Users\gzb0078\Downloads\diff_abund.csv"
diff.abund <- read.csv(file.choose())
str(diff.abund)
head(diff.abund)


#Make a volcano plot for Soybean vs. Soil

diff.abund$log10_pvalue <- -log10(diff.abund$p_CropSoybean)
diff.abund.label <- diff.abund[diff.abund$log10_pvalue > 30,]


#Make the plot

ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  theme_classic() + 
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean, label = Label))



#Make the plot look nice with our colorblind friendly scheme and labeled x and y axes plus make the legend more easily understandable

volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean, label = Label)) + 
  scale_color_manual(values = cbbPalette, name = "Significant") +
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano


#We can use the same concept if we wanted to emphasize certain points as a different shape or color

volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue)) + 
  geom_point(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue), color = "red", shape = 17, size = 4) +
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, label = Label), color = "red") + 
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano


#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#EXTRA: Adding significance letters to represent multiple pairwise comparisons of an ANOVA
library(lme4)
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
library(emmeans) #version 1.8.7
## Welcome to emmeans.
## Caution: You lose important information if you filter this package's results.
## See '? untidy'
library(multcomp)
## Loading required package: mvtnorm
## Loading required package: survival
## Loading required package: TH.data
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Attaching package: 'TH.data'
## The following object is masked from 'package:MASS':
## 
##     geyser
library(multcompView)
#Read in the data

STAND <- read.csv("raw_data_valent2023_pythium_seedtreatment.csv", na.strings = "na")
#Calculating the mean by groups
ave_stand <- STAND %>%
  filter(days_post_planting != "173 days after planting") %>%
  group_by(Plot, Treatment_name, Rep, days_post_planting) %>%
  dplyr::summarize(
    ave.stand = mean(stand, na.rm=TRUE)) 

## `summarise()` has grouped output by 'Plot', 'Treatment_name', 'Rep'. You can
## override using the `.groups` argument.
#Linear model
lm <- lmer(ave.stand ~ Treatment_name*days_post_planting + (1|Rep), data = ave_stand)
car::Anova(lm)





# Extracting the letters for the bars
sig.diff.letters <- data.frame(Results_lsmeansEC$emmeans$Treatment_name, 
                               Results_lsmeansEC$emmeans$days_post_planting,
                               str_trim(Results_lsmeansEC$emmeans$.group))
colnames(sig.diff.letters) <- c("Treatment_name", 
                                "days_post_planting",
                                "Letters")

# for plotting with letters from significance test
ave_stand2 <- ave_stand %>%
  group_by(Treatment_name, days_post_planting) %>%
  dplyr::summarize(
    ave.stand2 = mean(ave.stand, na.rm=TRUE),
    se = sd(ave.stand)/sqrt(4)) %>%
  left_join(sig.diff.letters) 
## `summarise()` has grouped output by 'Treatment_name'. You can override using
## the `.groups` argument.
## Joining with `by = join_by(Treatment_name, days_post_planting)`
ave_stand$Treatment_name <- factor(ave_stand$Treatment_name, levels = c("Neg. control",
                                                                        "Pos. control",
                                                                        "Intego Suite",
                                                                        "V-10525",
                                                                        "Cruiser Maxx",
                                                                        "Cruiser Maxx + Vayantis",
                                                                        "V-10522",
                                                                        "V-10522 + Lumisena"))

ave_stand$days_post_planting <- factor(ave_stand$days_post_planting, levels = c("8 days after planting",
                                                                                "15 days after planting", 
                                                                                "21 days after planting",
                                                                                "29 days after planting"))
#Final plot with the letters on the bars

### Stand bars ####
ggplot(ave_stand, aes(x = Treatment_name, y = ave.stand)) + 
  stat_summary(fun=mean,geom="bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Number of emerged plants") + 
  geom_jitter(width = 0.02, alpha = 0.5) +
  geom_text(data = ave_stand2, aes(label = Letters, y = ave.stand2+(3*se)), vjust = -0.5) +
  xlab("")+
  theme_classic() +
  theme(
    strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
    strip.text.x = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~days_post_planting)