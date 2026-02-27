# Figure2a

``` r
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.0     ✔ readr     2.1.6
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ lubridate 1.9.4     ✔ tibble    3.3.1
    ## ✔ purrr     1.2.1     ✔ tidyr     1.3.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73")
sample.data.bac <- read.csv("BacterialAlpha.csv", na.strings = "na")
head(sample.data.bac)
```

    ##     Code Crop Time_Point Replicate Water_Imbibed  shannon invsimpson   simpson
    ## 1 S01_13 Soil          0         1            NA 6.624921   210.7279 0.9952545
    ## 2 S02_16 Soil          0         2            NA 6.612413   206.8666 0.9951660
    ## 3 S03_19 Soil          0         3            NA 6.660853   213.0184 0.9953056
    ## 4 S04_22 Soil          0         4            NA 6.660671   204.6908 0.9951146
    ## 5 S05_25 Soil          0         5            NA 6.610965   200.2552 0.9950064
    ## 6 S06_28 Soil          0         6            NA 6.650812   199.3211 0.9949830
    ##   richness      even
    ## 1     3319 0.8171431
    ## 2     3079 0.8232216
    ## 3     3935 0.8046776
    ## 4     3922 0.8049774
    ## 5     3196 0.8192376
    ## 6     3481 0.8155427

``` r
sample.data.bac$Time_Point <- as.factor(sample.data.bac$Time_Point)
sample.data.bac$Crop <- as.factor(sample.data.bac$Crop)
sample.data.bac$Crop <- factor(sample.data.bac$Crop, levels = c("Soil", "Cotton", "Soybean"))

str(sample.data.bac)
```

    ## 'data.frame':    70 obs. of  10 variables:
    ##  $ Code         : chr  "S01_13" "S02_16" "S03_19" "S04_22" ...
    ##  $ Crop         : Factor w/ 3 levels "Soil","Cotton",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Time_Point   : Factor w/ 4 levels "0","6","12","18": 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ Replicate    : int  1 2 3 4 5 6 1 2 3 4 ...
    ##  $ Water_Imbibed: num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ shannon      : num  6.62 6.61 6.66 6.66 6.61 ...
    ##  $ invsimpson   : num  211 207 213 205 200 ...
    ##  $ simpson      : num  0.995 0.995 0.995 0.995 0.995 ...
    ##  $ richness     : int  3319 3079 3935 3922 3196 3481 3250 3170 3657 3177 ...
    ##  $ even         : num  0.817 0.823 0.805 0.805 0.819 ...

``` r
####### Bacterial evenness; Figure 2a ########
bac.even <- ggplot(sample.data.bac, aes(x = Time_Point, y = even, color = Crop)) +  # Define aesthetics: x-axis as Time.Point, y-axis as even, and color by Crop
  geom_boxplot(position = position_dodge(0.85)) +  # Add boxplots with dodged positions to avoid overlap
  geom_point(position = position_jitterdodge(0.05)) +  # Add jittered points to show individual data points, avoiding overlap
  ylab("Pielou's evenness") +  # Label the y-axis
  xlab("Hours post sowing") +  # Label the x-axis
  scale_color_manual(values = cbbPalette, name = "", labels = c("Soil no seeds", "Cotton spermosphere", "Soybean spermosphere")) +  # Manually set colors and labels for the Crop variable
  theme_classic()   # Use a classic theme for the plot
bac.even
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
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
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Figure2b

``` r
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
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Figure2c

``` r
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
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
library(ggpubr)
```

# Combine Figure2

``` r
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
```

    ## `geom_smooth()` using formula = 'y ~ x'

``` r
figure2
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Signficance

``` r
bac.even + 
  stat_compare_means(method = "anova") # apply an anova to the groups
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
### Example with pvalues as significance levels
bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.format")
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
### example with * as significance levels
bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.signif")
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
### Example with combined pvalue and * to indicate significance
bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "{p.adj.format}{p.adj.signif}")
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
water.imbibed.cor + 
  stat_cor()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
water.imbibed.cor + 
  stat_cor(label.y = 0.7) +
  stat_regline_equation()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
diff.abund <- read.csv("diff_abund.csv", na.strings = 'na')
str(diff.abund)
```

    ## 'data.frame':    2375 obs. of  16 variables:
    ##  $ taxon           : chr  "BOTU_1387" "BOTU_1197" "BOTU_2475" "BOTU_1574" ...
    ##  $ lfc_CropCotton  : num  0.016 0.1019 -0.0503 0.1019 0.0791 ...
    ##  $ lfc_CropSoybean : num  -0.305 0.191 -0.0213 0.2592 0.9588 ...
    ##  $ p_CropCotton    : num  0.947 0.572 0.806 0.531 0.846 ...
    ##  $ p_CropSoybean   : num  0.193 0.28 0.915 0.103 0.016 ...
    ##  $ q_CropCotton    : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ q_CropSoybean   : num  1 1 1 1 1 ...
    ##  $ diff_CropCotton : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ diff_CropSoybean: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ Kingdom         : chr  "Bacteria" "Bacteria" "Bacteria" "Bacteria" ...
    ##  $ Phylum          : chr  "Proteobacteria" "Proteobacteria" "Proteobacteria" "Proteobacteria" ...
    ##  $ Class           : chr  "Gammaproteobacteria" "Gammaproteobacteria" "Gammaproteobacteria" "Gammaproteobacteria" ...
    ##  $ Order           : chr  "Legionellales" "Diplorickettsiales" "Diplorickettsiales" "Diplorickettsiales" ...
    ##  $ Family          : chr  "Legionellaceae" "Diplorickettsiaceae" "Diplorickettsiaceae" "Diplorickettsiaceae" ...
    ##  $ Genus           : chr  "Legionella" "Aquicella" "Aquicella" "unidentified" ...
    ##  $ Label           : chr  "BOTU_1387_Legionella" "BOTU_1197_Aquicella" "BOTU_2475_Aquicella" "BOTU_1574_Diplorickettsiaceae" ...

``` r
head(diff.abund)
```

    ##       taxon lfc_CropCotton lfc_CropSoybean p_CropCotton p_CropSoybean
    ## 1 BOTU_1387     0.01600622     -0.30500531    0.9466551   0.192827692
    ## 2 BOTU_1197     0.10186637      0.19098423    0.5724439   0.279743828
    ## 3 BOTU_2475    -0.05031331     -0.02134244    0.8056191   0.915091288
    ## 4 BOTU_1574     0.10186637      0.25924872    0.5310377   0.103462114
    ## 5    BOTU_3     0.07914320      0.95882067    0.8457297   0.016049136
    ## 6  BOTU_145     0.47348726      1.09432567    0.1379480   0.000461554
    ##   q_CropCotton q_CropSoybean diff_CropCotton diff_CropSoybean  Kingdom
    ## 1            1             1           FALSE            FALSE Bacteria
    ## 2            1             1           FALSE            FALSE Bacteria
    ## 3            1             1           FALSE            FALSE Bacteria
    ## 4            1             1           FALSE            FALSE Bacteria
    ## 5            1             1           FALSE            FALSE Bacteria
    ## 6            1             1           FALSE            FALSE Bacteria
    ##           Phylum               Class              Order              Family
    ## 1 Proteobacteria Gammaproteobacteria      Legionellales      Legionellaceae
    ## 2 Proteobacteria Gammaproteobacteria Diplorickettsiales Diplorickettsiaceae
    ## 3 Proteobacteria Gammaproteobacteria Diplorickettsiales Diplorickettsiaceae
    ## 4 Proteobacteria Gammaproteobacteria Diplorickettsiales Diplorickettsiaceae
    ## 5 Proteobacteria Gammaproteobacteria    Pseudomonadales    Pseudomonadaceae
    ## 6 Proteobacteria Gammaproteobacteria    Pseudomonadales    Pseudomonadaceae
    ##          Genus                         Label
    ## 1   Legionella          BOTU_1387_Legionella
    ## 2    Aquicella           BOTU_1197_Aquicella
    ## 3    Aquicella           BOTU_2475_Aquicella
    ## 4 unidentified BOTU_1574_Diplorickettsiaceae
    ## 5  Pseudomonas            BOTU_3_Pseudomonas
    ## 6  Pseudomonas          BOTU_145_Pseudomonas

# Make a volcano plot for Soybean vs. Soil

``` r
#Make a volcano plot for Soybean vs. Soil

diff.abund$log10_pvalue <- -log10(diff.abund$p_CropSoybean)
diff.abund.label <- diff.abund[diff.abund$log10_pvalue > 30,]


#Make the plot

ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  theme_classic() + 
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean, label = Label))
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# Volcano 1

``` r
volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean, label = Label)) + 
  scale_color_manual(values = cbbPalette, name = "Significant") +
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

# Volcano 2

``` r
volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue)) + 
  geom_point(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue), color = "red", shape = 17, size = 4) +
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, label = Label), color = "red") + 
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano
```

![](Homework_DataVis2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
