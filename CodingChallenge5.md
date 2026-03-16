- [Question 1](#question-1)
- [Question 2](#question-2)
- [Question 3](#question-3)
- [Question 4](#question-4)
- [Question 5](#question-5)
- [Question 6](#question-6)

# Question 1

``` r
DiversityData <-read.csv("DiversityData.csv")
Metadata<- read.csv("Metadata.csv")
```

# Question 2

``` r
alpha <- merge(DiversityData, Metadata, by = "Code")
colnames(alpha)
```

    ## [1] "Code"          "shannon"       "invsimpson"    "simpson"      
    ## [5] "richness"      "Crop"          "Time_Point"    "Replicate"    
    ## [9] "Water_Imbibed"

# Question 3

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
alpha_even <- alpha %>%
  mutate(Pielou_evenness = shannon / log(richness))
colnames(alpha_even)
```

    ##  [1] "Code"            "shannon"         "invsimpson"      "simpson"        
    ##  [5] "richness"        "Crop"            "Time_Point"      "Replicate"      
    ##  [9] "Water_Imbibed"   "Pielou_evenness"

# Question 4

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.1     ✔ readr     2.1.6
    ## ✔ ggplot2   4.0.2     ✔ stringr   1.6.0
    ## ✔ lubridate 1.9.4     ✔ tibble    3.3.1
    ## ✔ purrr     1.2.1     ✔ tidyr     1.3.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
alpha_average <- alpha_even %>%
  group_by(Crop, Time_Point) %>%
  summarise(
    Mean.even = mean(Pielou_evenness),
    n = n(),
    sd.dev = sd(Pielou_evenness)
  ) %>%
  mutate(std.err = sd.dev / sqrt(n))
```

    ## `summarise()` has regrouped the output.
    ## ℹ Summaries were computed grouped by Crop and Time_Point.
    ## ℹ Output is grouped by Crop.
    ## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
    ## ℹ Use `summarise(.by = c(Crop, Time_Point))` for per-operation grouping
    ##   (`?dplyr::dplyr_by`) instead.

# Question 5

``` r
alpha_average2 <- alpha_average %>%
  select(Time_Point, Crop, Mean.even) %>%
  pivot_wider(names_from = Crop, values_from = Mean.even) %>%
  mutate(
    diff.cotton.even = Soil - Cotton,
    diff.soybean.even = Soil - Soybean
  )
```

# Question 6

``` r
plot_data <- alpha_average2 %>%
  select(Time_Point, diff.cotton.even, diff.soybean.even) %>%
  pivot_longer(
    c(diff.cotton.even, diff.soybean.even),
    names_to = "diff"
  )

ggplot(plot_data, aes(x = Time_Point, y = value, color = diff)) +
  geom_line() +
  theme_minimal(base_size = 8, base_family = "Arial") + theme(panel.grid = element_blank(), axis.line = element_line(colour = "Black")) + labs(x = "Time (hrs)",
    y = "Difference from soil in Pielou's evenness",
    color = "diff"
  )
```

![](CodingChallenge5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
