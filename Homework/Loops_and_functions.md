# Functions

``` r
library(ggplot2)
library(drc) 
```

    ## Loading required package: MASS

    ## 
    ## 'drc' has been loaded.

    ## Please cite R and 'drc' if used for a publication,

    ## for references type 'citation()' and 'citation('drc')'.

    ## 
    ## Attaching package: 'drc'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     gaussian, getInitial

``` r
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
    ## ✖ dplyr::select() masks MASS::select()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
(5*(32 - 32)/9)
```

    ## [1] 0

``` r
(5*(80 - 32)/9)
```

    ## [1] 26.66667

``` r
F_to_C <- function(fahrenheit_temp){
  celsius <- (5*(fahrenheit_temp - 32)/9)
  return(celsius)
}

# these do the same thing
F_to_C(32)
```

    ## [1] 0

# Anatomy of a function

sample.function \<- function(… variable goes here …){ …. code goes
here…. return(… output …) }

``` r
sample_function <- function(x) {
  if (x < 0) {
    return("Negative value")
  }
  sqrt(x)
}

c_to_f <- function(C) {
  F <- C * (9/5) + 32
  return(F)
}
```

# Iterations

Iterations are something you do over and over again. They are useful for
multiple reasons. In terms of reproducibility, it again, helps reduce
copy and paste errors for something we would like to do over and over
again. It can also helpful for sanity checks through data simulation.

In this example we will cover iterations in the following functions

1.  rep()

2.  seq() & seq_along()

3.  for loops map() \*nested map()

the rep() function The rep() function allows you to repeat elements
easily

``` r
rep("A", 3) # repeats A three times 
```

    ## [1] "A" "A" "A"

``` r
rep(c("A", "B"), 5) # repeats A and B, times 
```

    ##  [1] "A" "B" "A" "B" "A" "B" "A" "B" "A" "B"

``` r
rep(c(1,2,3,4), times = 4) # repeats 1,2,3,4, 4 times 
```

    ##  [1] 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4

``` r
rep(c(1,2,5,2), times = 4, each = 4) # repeats 1 four times, 2 four times, 5 four times, and 2 four times. 
```

    ##  [1] 1 1 1 1 2 2 2 2 5 5 5 5 2 2 2 2 1 1 1 1 2 2 2 2 5 5 5 5 2 2 2 2 1 1 1 1 2 2
    ## [39] 2 2 5 5 5 5 2 2 2 2 1 1 1 1 2 2 2 2 5 5 5 5 2 2 2 2

The seq() command allows you to write sequences of numbers easily

``` r
seq(from = 1, to = 7) # sequence of numbers 1 to 7
```

    ## [1] 1 2 3 4 5 6 7

``` r
seq(from = 0, to = 10, by = 2) # sequence of numbers from 0 to 10 by 2s 
```

    ## [1]  0  2  4  6  8 10

# Combined seq() and rep()

``` r
rep(seq(from = 0, to = 10, by = 2), times = 3, each = 2)
```

    ##  [1]  0  0  2  2  4  4  6  6  8  8 10 10  0  0  2  2  4  4  6  6  8  8 10 10  0
    ## [26]  0  2  2  4  4  6  6  8  8 10 10

The seq_along() function allows you to generate a sequence of numbers
based on non-integer values. This will become very useful when we want
to loop over elements within a dataframe.

``` r
# use the built in LETTERS vector for an example. 
LETTERS
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
seq_along(LETTERS[1:5]) # will return 1,2,3,4,5 not the actual letters. 
```

    ## [1] 1 2 3 4 5

# The for loop

The for loop is classic coding.Almost every coding language has a
version of a for loop.

Lets see a very basic example of how a for loop works

``` r
for (i in 1:10) {
print(i*2)  
}
```

    ## [1] 2
    ## [1] 4
    ## [1] 6
    ## [1] 8
    ## [1] 10
    ## [1] 12
    ## [1] 14
    ## [1] 16
    ## [1] 18
    ## [1] 20

lets see a more complicated example where we can also use our functions
in a loop for example.

``` r
for (i in -30:100){
  result <- F_to_C(i)
  print(result)
}
```

    ## [1] -34.44444
    ## [1] -33.88889
    ## [1] -33.33333
    ## [1] -32.77778
    ## [1] -32.22222
    ## [1] -31.66667
    ## [1] -31.11111
    ## [1] -30.55556
    ## [1] -30
    ## [1] -29.44444
    ## [1] -28.88889
    ## [1] -28.33333
    ## [1] -27.77778
    ## [1] -27.22222
    ## [1] -26.66667
    ## [1] -26.11111
    ## [1] -25.55556
    ## [1] -25
    ## [1] -24.44444
    ## [1] -23.88889
    ## [1] -23.33333
    ## [1] -22.77778
    ## [1] -22.22222
    ## [1] -21.66667
    ## [1] -21.11111
    ## [1] -20.55556
    ## [1] -20
    ## [1] -19.44444
    ## [1] -18.88889
    ## [1] -18.33333
    ## [1] -17.77778
    ## [1] -17.22222
    ## [1] -16.66667
    ## [1] -16.11111
    ## [1] -15.55556
    ## [1] -15
    ## [1] -14.44444
    ## [1] -13.88889
    ## [1] -13.33333
    ## [1] -12.77778
    ## [1] -12.22222
    ## [1] -11.66667
    ## [1] -11.11111
    ## [1] -10.55556
    ## [1] -10
    ## [1] -9.444444
    ## [1] -8.888889
    ## [1] -8.333333
    ## [1] -7.777778
    ## [1] -7.222222
    ## [1] -6.666667
    ## [1] -6.111111
    ## [1] -5.555556
    ## [1] -5
    ## [1] -4.444444
    ## [1] -3.888889
    ## [1] -3.333333
    ## [1] -2.777778
    ## [1] -2.222222
    ## [1] -1.666667
    ## [1] -1.111111
    ## [1] -0.5555556
    ## [1] 0
    ## [1] 0.5555556
    ## [1] 1.111111
    ## [1] 1.666667
    ## [1] 2.222222
    ## [1] 2.777778
    ## [1] 3.333333
    ## [1] 3.888889
    ## [1] 4.444444
    ## [1] 5
    ## [1] 5.555556
    ## [1] 6.111111
    ## [1] 6.666667
    ## [1] 7.222222
    ## [1] 7.777778
    ## [1] 8.333333
    ## [1] 8.888889
    ## [1] 9.444444
    ## [1] 10
    ## [1] 10.55556
    ## [1] 11.11111
    ## [1] 11.66667
    ## [1] 12.22222
    ## [1] 12.77778
    ## [1] 13.33333
    ## [1] 13.88889
    ## [1] 14.44444
    ## [1] 15
    ## [1] 15.55556
    ## [1] 16.11111
    ## [1] 16.66667
    ## [1] 17.22222
    ## [1] 17.77778
    ## [1] 18.33333
    ## [1] 18.88889
    ## [1] 19.44444
    ## [1] 20
    ## [1] 20.55556
    ## [1] 21.11111
    ## [1] 21.66667
    ## [1] 22.22222
    ## [1] 22.77778
    ## [1] 23.33333
    ## [1] 23.88889
    ## [1] 24.44444
    ## [1] 25
    ## [1] 25.55556
    ## [1] 26.11111
    ## [1] 26.66667
    ## [1] 27.22222
    ## [1] 27.77778
    ## [1] 28.33333
    ## [1] 28.88889
    ## [1] 29.44444
    ## [1] 30
    ## [1] 30.55556
    ## [1] 31.11111
    ## [1] 31.66667
    ## [1] 32.22222
    ## [1] 32.77778
    ## [1] 33.33333
    ## [1] 33.88889
    ## [1] 34.44444
    ## [1] 35
    ## [1] 35.55556
    ## [1] 36.11111
    ## [1] 36.66667
    ## [1] 37.22222
    ## [1] 37.77778

This was great, but this only printed to the console, and we cannot
really do anything with those values. What if we wanted to do something
with the result of the iteration?

Step 1. Set a R object to NULL

Step 2. Set your for loop

Step 3. Save the result of your for loop into a dataframe each iteration

Step 4. append one row of the dataframe to the null object each
iteration of the loop.

``` r
celcius.df <- NULL 
for (i in -30:100){   
result_i <- data.frame(F_to_C(i), i)   
celcius.df <- rbind.data.frame(celcius.df, result_i) 
}
```

# A more practical example involving fungicide sensitivity data

Caution… this is challenging. If you find yourself wanting more of a
challenge, do this.

That was cool, but lets see a more practical example.

For this example, we will use the package ‘drc’ and the dataset
EC50.all.csv

``` r
# read in the data
EC50.data <- read.csv("EC50_all.csv")
```

Within this dataset is a list of isolates and fungicide sensitivity
data.

We want to find something called an EC50 or the concentration of a
fungicide that reduces growth by 50% relative to the non-amended
control.

To do this we use a non-linear regression of the radial growth of a
fungus against the log concentration of a fungicide, and find the
half-maximal response.

The exact details of the statistics are not super important for you to
know, but just know that this example studies can include hundreds of
individual isolates to model this EC50 value, and this can be very
cumbersome if we had to write code for every isolate.

We do this using non-linear modeling with the DRC package with a model
called the LL.4 The specifics of the model are not that important. What
is important is that in order to use this model we have to model each
isolate individually and gather the EC50 data.

An example is shown here where we do this on isolate ILSO_5-41c

``` r
isolate1 <- drm(100 * EC50.data$relgrowth[EC50.data$is == "ILSO_5-41c"] ~ 
        EC50.data$conc[EC50.data$is == "ILSO_5-41c"], 
                       fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                  names = c("Slope", "Lower", "Upper", "EC50")), 
                       na.action = na.omit)
    # outputs the summary of the paramters including the estimate, standard
    # error, t-value, and p-value outputs it into a data frame called
    # summary.mef.fit for 'summary of fit'
    summary.fit <- data.frame(summary(isolate1)[[3]])
    # outputs the summary of just the EC50 data including the estimate, standard
    # error, upper and lower bounds of the 95% confidence intervals around the
    # EC50
    EC50 <- ED(isolate1, respLev = c(50), type = "relative", 
        interval = "delta")[[1]]
```

    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1070318  0.0055365 0.0957543 0.1183094

It would not be reproducible to copy and paste each isolate into the
code above to run the model for every isolate. It would also not be very
reproducible to copy and paste the code below since this is prone to
errors.

# The loop

There are then two options: a loop that iterates over isolate names in a
newer way using the function map()

I will demonstrate how to do each option.

First the loop

First, we need to create a vector to loop over containing the names of
our isolates. We will call this ‘nm’ using the function ‘unique’.

``` r
nm <- unique(EC50.data$is)
```

Then we can write a loop using the code above and input our list of
isolates, ‘nm’ by using ‘seq_along’

Then, whenever we want to input our isolate name, we will put
nm\[\[i\]\].

So, as the loop goes along the list of isolate names, it will input the
name of the isolate, calculate EC50, and run the model and output EC50.

``` r
for (i in seq_along(nm)) {
  isolate1 <- drm(100 * EC50.data$relgrowth[EC50.data$is == nm[[i]]] ~ 
        EC50.data$conc[EC50.data$is == nm[[i]]], 
                       fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                  names = c("Slope", "Lower", "Upper", "EC50")), 
                       na.action = na.omit)
    # outputs the summary of the paramters including the estimate, standard
    # error, t-value, and p-value outputs it into a data frame called
    # summary.mef.fit for 'summary of fit'
    summary.fit <- data.frame(summary(isolate1)[[3]])
    # outputs the summary of just the EC50 data including the estimate, standard
    # error, upper and lower bounds of the 95% confidence intervals around the
    # EC50
    EC50 <- ED(isolate1, respLev = c(50), type = "relative", 
        interval = "delta")[[1]]
    EC50
}
```

    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1070318  0.0055365 0.0957543 0.1183094
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.248655   0.028485 0.190633 0.306678
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.167592   0.010197 0.146821 0.188362
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1082677  0.0051459 0.0977858 0.1187495
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.184271   0.036047 0.110846 0.257695
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227432   0.040614 0.144704 0.310160
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.101863   0.003487 0.094760 0.108965
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1102721  0.0033354 0.1034780 0.1170661
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123288   0.014018 0.094735 0.151841
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0998727  0.0044787 0.0907498 0.1089956
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.69465    0.39164 -0.10310  1.49240
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.113975   0.012773 0.087958 0.139993
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.217436   0.027934 0.160536 0.274335
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1432333  0.0093132 0.1242629 0.1622036
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.18336    0.01293 0.15695 0.20977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.186929   0.034023 0.117626 0.256232
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0299288  0.0017812 0.0263007 0.0335569
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.200379   0.020104 0.159429 0.241329
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.30812    0.24033 -0.18142  0.79765
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227103   0.019697 0.186983 0.267224
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20009    0.01448 0.17059 0.22958
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.223966   0.058089 0.105642 0.342290
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.288001   0.074597 0.136052 0.439951
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.369422   0.077015 0.212549 0.526296
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118335   0.011733 0.094404 0.142265
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.189945   0.013146 0.163097 0.216793
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0483296  0.0022658 0.0437143 0.0529448
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.190146   0.027182 0.134779 0.245514
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16580    0.01082 0.14376 0.18784
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.183297   0.017237 0.148187 0.218407
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.130147   0.010705 0.108342 0.151951
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1915200  0.0077369 0.1757605 0.2072795
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123034   0.006696 0.109395 0.136673
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1935594  0.0094277 0.1743559 0.2127629
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.198000   0.019219 0.158853 0.237148
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1114482  0.0070542 0.0970793 0.1258172
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.159440   0.010423 0.138209 0.180671
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1372654  0.0070847 0.1228343 0.1516965
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50  0.427766   0.230327 -0.041394  0.896925
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0991738  0.0040323 0.0909603 0.1073874
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.106855   0.022010 0.062022 0.151687
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.156127   0.021551 0.112229 0.200025
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.308127   0.019233 0.268951 0.347304
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.117014   0.012255 0.092052 0.141977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.177036   0.011915 0.152767 0.201305
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.234268   0.017095 0.199447 0.269088
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0172659  0.0012838 0.0146508 0.0198809
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.201737   0.012113 0.176998 0.226476
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.306968   0.078617 0.146831 0.467105
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.289597   0.081347 0.123464 0.455730
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.213191   0.024013 0.164278 0.262104
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.42728    0.28839 -0.16016  1.01472
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0900834  0.0021351 0.0857344 0.0944324
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1573077  0.0065037 0.1440602 0.1705553
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16319    0.01761 0.12732 0.19906
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20914    0.01403 0.18056 0.23772
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.17905    0.00849 0.16171 0.19639
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1587569  0.0098007 0.1387411 0.1787727
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1352667  0.0074545 0.1200824 0.1504511
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.247784   0.036714 0.173000 0.322567
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.235268   0.026532 0.181223 0.289313
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.066926   0.010213 0.046123 0.087728
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.174492   0.010501 0.153102 0.195882
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.181951   0.028336 0.124233 0.239669
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.195576   0.013476 0.168125 0.223027
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.168410   0.010795 0.146421 0.190399
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1546980  0.0093702 0.1354373 0.1739588
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.162666   0.011066 0.140126 0.185206
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.147113   0.008233 0.130343 0.163883
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1376907  0.0077899 0.1218232 0.1535582
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118886   0.004502 0.109716 0.128057
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.206342   0.016866 0.171988 0.240696
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.175509   0.013954 0.147086 0.203932
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.65376    0.63282 -0.63525  1.94277
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.211026   0.012571 0.185419 0.236633

# The loop and saving each iterations output

Awesome! we have all of our EC50 values very quickly.

But this is still not super helpful because the output is not saved and
we cant use it in a plot or something. So, we now need to save the
output of a loop into a dataframe.

The way we do this is to create the dataframe row by row in the loop.

To do this we first need to creat a null dataframe and add to it each
iteration.

``` r
EC50.ll4 <- NULL # create a null object 
for (i in seq_along(nm)) {
  isolate1 <- drm(100 * EC50.data$relgrowth[EC50.data$is == nm[[i]]] ~ 
        EC50.data$conc[EC50.data$is == nm[[i]]], 
                       fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                  names = c("Slope", "Lower", "Upper", "EC50")), 
                       na.action = na.omit)
    # outputs the summary of the paramters including the estimate, standard
    # error, t-value, and p-value outputs it into a data frame called
    # summary.mef.fit for 'summary of fit'
    summary.fit <- data.frame(summary(isolate1)[[3]])
    # outputs the summary of just the EC50 data including the estimate, standard
    # error, upper and lower bounds of the 95% confidence intervals around the
    # EC50
    EC50 <- ED(isolate1, respLev = c(50), type = "relative", 
        interval = "delta")[[1]]
    EC50
    isolate.ec_i <- data.frame(nm[[i]], EC50) # create a one row dataframe containing just the isolate name and the EC50
    colnames(isolate.ec_i) <- c("Isolate", "EC50") # change the column names
    
    # Then we need to append our one row dataframe to our null dataframe we created before
    # and save it as EC50.ll4. 
    EC50.ll4 <- rbind.data.frame(EC50.ll4, isolate.ec_i)
}
```

    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1070318  0.0055365 0.0957543 0.1183094
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.248655   0.028485 0.190633 0.306678
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.167592   0.010197 0.146821 0.188362
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1082677  0.0051459 0.0977858 0.1187495
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.184271   0.036047 0.110846 0.257695
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227432   0.040614 0.144704 0.310160
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.101863   0.003487 0.094760 0.108965
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1102721  0.0033354 0.1034780 0.1170661
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123288   0.014018 0.094735 0.151841
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0998727  0.0044787 0.0907498 0.1089956
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.69465    0.39164 -0.10310  1.49240
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.113975   0.012773 0.087958 0.139993
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.217436   0.027934 0.160536 0.274335
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1432333  0.0093132 0.1242629 0.1622036
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.18336    0.01293 0.15695 0.20977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.186929   0.034023 0.117626 0.256232
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0299288  0.0017812 0.0263007 0.0335569
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.200379   0.020104 0.159429 0.241329
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.30812    0.24033 -0.18142  0.79765
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227103   0.019697 0.186983 0.267224
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20009    0.01448 0.17059 0.22958
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.223966   0.058089 0.105642 0.342290
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.288001   0.074597 0.136052 0.439951
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.369422   0.077015 0.212549 0.526296
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118335   0.011733 0.094404 0.142265
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.189945   0.013146 0.163097 0.216793
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0483296  0.0022658 0.0437143 0.0529448
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.190146   0.027182 0.134779 0.245514
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16580    0.01082 0.14376 0.18784
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.183297   0.017237 0.148187 0.218407
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.130147   0.010705 0.108342 0.151951
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1915200  0.0077369 0.1757605 0.2072795
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123034   0.006696 0.109395 0.136673
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1935594  0.0094277 0.1743559 0.2127629
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.198000   0.019219 0.158853 0.237148
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1114482  0.0070542 0.0970793 0.1258172
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.159440   0.010423 0.138209 0.180671
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1372654  0.0070847 0.1228343 0.1516965
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50  0.427766   0.230327 -0.041394  0.896925
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0991738  0.0040323 0.0909603 0.1073874
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.106855   0.022010 0.062022 0.151687
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.156127   0.021551 0.112229 0.200025
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.308127   0.019233 0.268951 0.347304
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.117014   0.012255 0.092052 0.141977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.177036   0.011915 0.152767 0.201305
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.234268   0.017095 0.199447 0.269088
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0172659  0.0012838 0.0146508 0.0198809
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.201737   0.012113 0.176998 0.226476
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.306968   0.078617 0.146831 0.467105
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.289597   0.081347 0.123464 0.455730
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.213191   0.024013 0.164278 0.262104
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.42728    0.28839 -0.16016  1.01472
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0900834  0.0021351 0.0857344 0.0944324
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1573077  0.0065037 0.1440602 0.1705553
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16319    0.01761 0.12732 0.19906
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20914    0.01403 0.18056 0.23772
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.17905    0.00849 0.16171 0.19639
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1587569  0.0098007 0.1387411 0.1787727
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1352667  0.0074545 0.1200824 0.1504511
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.247784   0.036714 0.173000 0.322567
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.235268   0.026532 0.181223 0.289313
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.066926   0.010213 0.046123 0.087728
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.174492   0.010501 0.153102 0.195882
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.181951   0.028336 0.124233 0.239669
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.195576   0.013476 0.168125 0.223027
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.168410   0.010795 0.146421 0.190399
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1546980  0.0093702 0.1354373 0.1739588
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.162666   0.011066 0.140126 0.185206
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.147113   0.008233 0.130343 0.163883
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1376907  0.0077899 0.1218232 0.1535582
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118886   0.004502 0.109716 0.128057
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.206342   0.016866 0.171988 0.240696
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.175509   0.013954 0.147086 0.203932
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.65376    0.63282 -0.63525  1.94277
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.211026   0.012571 0.185419 0.236633

Now we have a final dataset with EC50 data contained within EC50.ll4
that was appended with each isolate upon each iteration. Something we
can use in a graph for example for publication. Like this.

``` r
ggplot(EC50.ll4, aes(x = EC50)) + geom_histogram() + theme_classic()
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](Loops_and_functions_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

# Using map with the tidy verse

However, just like everything else in R, there are multiple ways to do
the same thing.

Lets demonstrate this example with the function map() and nested
dataframes within the tidyr package.

The concept is that loops are combersome and hard to read. So, the map()
function was created and everything is contained within the same R
object.

Here is the example of how to do the same loop as above but with the
map() function:

``` r
EC50.data %>%
  group_by(is) %>%
  nest() %>%
  mutate(ll.4.mod = map(data, ~drm(.$relgrowth ~ .$conc, 
                              fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                         names = c("Slope", "Lower", "Upper", "EC50"))))) %>%
  mutate(ec50 = map(ll.4.mod, ~ED(., 
                              respLev = c(50), 
                              type = "relative",
                              interval = "delta")[[1]])) %>%
  unnest(ec50)
```

    ## Warning: There were 19 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `ll.4.mod = map(...)`.
    ## ℹ In group 4: `is = "C-MNSO2_2-10"`.
    ## Caused by warning in `log()`:
    ## ! NaNs produced
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 18 remaining warnings.

    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.106855   0.022010 0.062022 0.151687
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.177036   0.011915 0.152767 0.201305
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.234268   0.017095 0.199447 0.269088
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0172659  0.0012838 0.0146508 0.0198809
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.117014   0.012255 0.092052 0.141977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.147113   0.008233 0.130343 0.163883
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1376907  0.0077899 0.1218232 0.1535582
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118886   0.004502 0.109716 0.128057
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.206342   0.016866 0.171988 0.240696
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.175509   0.013954 0.147086 0.203932
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.65376    0.63282 -0.63525  1.94277
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118335   0.011733 0.094404 0.142265
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.189945   0.013146 0.163097 0.216793
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0483296  0.0022658 0.0437143 0.0529448
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.190146   0.027182 0.134779 0.245514
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16580    0.01082 0.14376 0.18784
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.183297   0.017237 0.148187 0.218407
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.130147   0.010705 0.108342 0.151951
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1915200  0.0077369 0.1757605 0.2072795
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123034   0.006696 0.109395 0.136673
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1935594  0.0094277 0.1743559 0.2127629
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.198000   0.019219 0.158853 0.237148
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1114482  0.0070542 0.0970793 0.1258172
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.159440   0.010423 0.138209 0.180671
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1372654  0.0070847 0.1228343 0.1516965
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1070318  0.0055365 0.0957543 0.1183094
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.248655   0.028485 0.190633 0.306678
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.167592   0.010197 0.146821 0.188362
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1082677  0.0051459 0.0977858 0.1187495
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.184271   0.036047 0.110846 0.257695
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123288   0.014018 0.094735 0.151841
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0998727  0.0044787 0.0907498 0.1089956
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227432   0.040614 0.144704 0.310160
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.101863   0.003487 0.094760 0.108965
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.69465    0.39164 -0.10310  1.49240
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.113975   0.012773 0.087958 0.139993
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.217436   0.027934 0.160536 0.274335
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1102721  0.0033354 0.1034780 0.1170661
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1432333  0.0093132 0.1242629 0.1622036
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.18336    0.01293 0.15695 0.20977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.186929   0.034023 0.117626 0.256232
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0299288  0.0017812 0.0263007 0.0335569
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.200379   0.020104 0.159429 0.241329
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.30812    0.24033 -0.18142  0.79765
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227103   0.019697 0.186983 0.267224
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20009    0.01448 0.17059 0.22958
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.223966   0.058089 0.105642 0.342290
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.288001   0.074597 0.136052 0.439951
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.369422   0.077015 0.212549 0.526296
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50  0.427766   0.230327 -0.041395  0.896927
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0991738  0.0040323 0.0909603 0.1073874
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.156127   0.021551 0.112229 0.200025
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.308127   0.019233 0.268951 0.347304
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.201737   0.012113 0.176998 0.226476
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.306968   0.078617 0.146831 0.467105
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.289597   0.081347 0.123464 0.455730
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.213191   0.024013 0.164278 0.262104
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1352667  0.0074545 0.1200824 0.1504511
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.247784   0.036714 0.173000 0.322567
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.235268   0.026532 0.181223 0.289313
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.066926   0.010213 0.046123 0.087728
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.174492   0.010501 0.153102 0.195882
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.181951   0.028336 0.124233 0.239669
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.195576   0.013476 0.168125 0.223027
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.168410   0.010795 0.146421 0.190399
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1546980  0.0093702 0.1354373 0.1739588
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.162666   0.011066 0.140126 0.185206
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.42728    0.28840 -0.16016  1.01472
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0900834  0.0021351 0.0857344 0.0944324
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1573077  0.0065037 0.1440602 0.1705553
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16319    0.01761 0.12732 0.19906
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20914    0.01403 0.18056 0.23772
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.17905    0.00849 0.16171 0.19639
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1587569  0.0098007 0.1387411 0.1787727
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.211026   0.012571 0.185419 0.236633

    ## # A tibble: 75 × 4
    ## # Groups:   is [75]
    ##    is         data               ll.4.mod   ec50
    ##    <chr>      <list>             <list>    <dbl>
    ##  1 ILSO_5-41c <tibble [36 × 11]> <drc>    0.107 
    ##  2 ILSO_5-42c <tibble [36 × 11]> <drc>    0.249 
    ##  3 ILSO_5-49b <tibble [36 × 11]> <drc>    0.168 
    ##  4 ILSO_6-1   <tibble [36 × 11]> <drc>    0.108 
    ##  5 ILSO_6-12B <tibble [36 × 11]> <drc>    0.184 
    ##  6 ILSO_6-2b  <tibble [36 × 11]> <drc>    0.227 
    ##  7 ILSO_6-33C <tibble [36 × 11]> <drc>    0.102 
    ##  8 ILSO_6-39C <tibble [36 × 11]> <drc>    0.110 
    ##  9 ILSO_6-15b <tibble [36 × 11]> <drc>    0.123 
    ## 10 ILSO_6-28C <tibble [36 × 11]> <drc>    0.0999
    ## # ℹ 65 more rows

Ok that was alot…

So lets break down what each step does after the pipe.

## Step 1.

All we are doing is setting a grouping variable. This is what we want to
loop over. The grouping variable can be multiple variables, but in this
case we just want isolate as the grouping variable. When running the
code below, it wont look like much has happened other than you will
notice a Groups header at the top of the table

``` r
EC50.data %>%
  group_by(is)
```

    ## # A tibble: 2,681 × 12
    ## # Groups:   is [75]
    ##    is         location trial   set batch   lat  conc   rep measure coldiam
    ##    <chr>      <chr>    <int> <int> <int> <dbl> <dbl> <int>   <int>   <dbl>
    ##  1 ILSO_5-41c Illinois     1     1     2  39.1  0        1       1    71.5
    ##  2 ILSO_5-41c Illinois     1     1     2  39.1  0        1       2    70.0
    ##  3 ILSO_5-41c Illinois     1     1     2  39.1  0        2       1    72.9
    ##  4 ILSO_5-41c Illinois     1     1     2  39.1  0        2       2    70.5
    ##  5 ILSO_5-41c Illinois     1     1     2  39.1  0        3       1    68.5
    ##  6 ILSO_5-41c Illinois     1     1     2  39.1  0        3       2    69.1
    ##  7 ILSO_5-41c Illinois     1     1     2  39.1  0.01     1       1    65.4
    ##  8 ILSO_5-41c Illinois     1     1     2  39.1  0.01     1       2    64.3
    ##  9 ILSO_5-41c Illinois     1     1     2  39.1  0.01     2       1    65.3
    ## 10 ILSO_5-41c Illinois     1     1     2  39.1  0.01     2       2    65.7
    ## # ℹ 2,671 more rows
    ## # ℹ 2 more variables: coldiamplug <dbl>, relgrowth <dbl>

## Step 2.

Next we are going to “nest’. This says that for all the data for each
isolate we want to collapse that into a column called data.

``` r
EC50.data %>%
  group_by(is) %>%
  nest()
```

    ## # A tibble: 75 × 2
    ## # Groups:   is [75]
    ##    is         data              
    ##    <chr>      <list>            
    ##  1 ILSO_5-41c <tibble [36 × 11]>
    ##  2 ILSO_5-42c <tibble [36 × 11]>
    ##  3 ILSO_5-49b <tibble [36 × 11]>
    ##  4 ILSO_6-1   <tibble [36 × 11]>
    ##  5 ILSO_6-12B <tibble [36 × 11]>
    ##  6 ILSO_6-2b  <tibble [36 × 11]>
    ##  7 ILSO_6-33C <tibble [36 × 11]>
    ##  8 ILSO_6-39C <tibble [36 × 11]>
    ##  9 ILSO_6-15b <tibble [36 × 11]>
    ## 10 ILSO_6-28C <tibble [36 × 11]>
    ## # ℹ 65 more rows

## Step 3.

Next we can enter our formula to iterate over for each set of data for
each isolate. Now this is alot. So lets break it down. The function
mutate tells R to create a new column. We are going to call it ll.4.mod.
This column is going to be equal to our output of our function from drm.
The map function tells R to iterate over the grouped variable, isolate.
We will use data within the data column for the mapping. The ‘.’ tells R
that we are considering all the data. Then a \$ followed by the column
headers in the data as the input into our drm function. The rest of the
options are consistent with the loop above. The output will contain a
new column with ll.4.mod column which contains a drm model for each
isolate.

``` r
EC50.data %>%
  group_by(is) %>%
  nest() %>%
  mutate(ll.4.mod = map(data, ~drm(.$relgrowth ~ .$conc, 
                              fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                         names = c("Slope", "Lower", "Upper", "EC50")))))
```

    ## Warning: There were 19 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `ll.4.mod = map(...)`.
    ## ℹ In group 4: `is = "C-MNSO2_2-10"`.
    ## Caused by warning in `log()`:
    ## ! NaNs produced
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 18 remaining warnings.

    ## # A tibble: 75 × 3
    ## # Groups:   is [75]
    ##    is         data               ll.4.mod
    ##    <chr>      <list>             <list>  
    ##  1 ILSO_5-41c <tibble [36 × 11]> <drc>   
    ##  2 ILSO_5-42c <tibble [36 × 11]> <drc>   
    ##  3 ILSO_5-49b <tibble [36 × 11]> <drc>   
    ##  4 ILSO_6-1   <tibble [36 × 11]> <drc>   
    ##  5 ILSO_6-12B <tibble [36 × 11]> <drc>   
    ##  6 ILSO_6-2b  <tibble [36 × 11]> <drc>   
    ##  7 ILSO_6-33C <tibble [36 × 11]> <drc>   
    ##  8 ILSO_6-39C <tibble [36 × 11]> <drc>   
    ##  9 ILSO_6-15b <tibble [36 × 11]> <drc>   
    ## 10 ILSO_6-28C <tibble [36 × 11]> <drc>   
    ## # ℹ 65 more rows

## Step 4.

Next we just want to estimate the EC50 for each fitted model contained
within the ll.4.mod column. So we apply another mutate to create another
column we iwll name ec50 and we want to map over the ll.4.mod column
because we want to apply the function ED to each model fit. The brackets
after the ED function \[\[1\]\] means we want the first element in that
output, which is the EC50. I know this because I’ve looked at the output
for one isolate and figured out how to get just the EC50 data output.

``` r
EC50.data %>%
  group_by(is) %>%
  nest() %>%
  mutate(ll.4.mod = map(data, ~drm(.$relgrowth ~ .$conc, 
                              fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                         names = c("Slope", "Lower", "Upper", "EC50"))))) %>%
  mutate(ec50 = map(ll.4.mod, ~ED(., 
                              respLev = c(50), 
                              type = "relative",
                              interval = "delta")[[1]]))
```

    ## Warning: There were 19 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `ll.4.mod = map(...)`.
    ## ℹ In group 4: `is = "C-MNSO2_2-10"`.
    ## Caused by warning in `log()`:
    ## ! NaNs produced
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 18 remaining warnings.

    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.106855   0.022010 0.062022 0.151687
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.177036   0.011915 0.152767 0.201305
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.234268   0.017095 0.199447 0.269088
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0172659  0.0012838 0.0146508 0.0198809
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.117014   0.012255 0.092052 0.141977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.147113   0.008233 0.130343 0.163883
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1376907  0.0077899 0.1218232 0.1535582
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118886   0.004502 0.109716 0.128057
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.206342   0.016866 0.171988 0.240696
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.175509   0.013954 0.147086 0.203932
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.65376    0.63282 -0.63525  1.94277
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118335   0.011733 0.094404 0.142265
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.189945   0.013146 0.163097 0.216793
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0483296  0.0022658 0.0437143 0.0529448
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.190146   0.027182 0.134779 0.245514
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16580    0.01082 0.14376 0.18784
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.183297   0.017237 0.148187 0.218407
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.130147   0.010705 0.108342 0.151951
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1915200  0.0077369 0.1757605 0.2072795
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123034   0.006696 0.109395 0.136673
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1935594  0.0094277 0.1743559 0.2127629
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.198000   0.019219 0.158853 0.237148
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1114482  0.0070542 0.0970793 0.1258172
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.159440   0.010423 0.138209 0.180671
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1372654  0.0070847 0.1228343 0.1516965
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1070318  0.0055365 0.0957543 0.1183094
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.248655   0.028485 0.190633 0.306678
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.167592   0.010197 0.146821 0.188362
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1082677  0.0051459 0.0977858 0.1187495
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.184271   0.036047 0.110846 0.257695
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123288   0.014018 0.094735 0.151841
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0998727  0.0044787 0.0907498 0.1089956
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227432   0.040614 0.144704 0.310160
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.101863   0.003487 0.094760 0.108965
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.69465    0.39164 -0.10310  1.49240
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.113975   0.012773 0.087958 0.139993
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.217436   0.027934 0.160536 0.274335
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1102721  0.0033354 0.1034780 0.1170661
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1432333  0.0093132 0.1242629 0.1622036
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.18336    0.01293 0.15695 0.20977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.186929   0.034023 0.117626 0.256232
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0299288  0.0017812 0.0263007 0.0335569
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.200379   0.020104 0.159429 0.241329
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.30812    0.24033 -0.18142  0.79765
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227103   0.019697 0.186983 0.267224
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20009    0.01448 0.17059 0.22958
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.223966   0.058089 0.105642 0.342290
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.288001   0.074597 0.136052 0.439951
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.369422   0.077015 0.212549 0.526296
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50  0.427766   0.230327 -0.041395  0.896927
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0991738  0.0040323 0.0909603 0.1073874
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.156127   0.021551 0.112229 0.200025
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.308127   0.019233 0.268951 0.347304
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.201737   0.012113 0.176998 0.226476
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.306968   0.078617 0.146831 0.467105
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.289597   0.081347 0.123464 0.455730
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.213191   0.024013 0.164278 0.262104
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1352667  0.0074545 0.1200824 0.1504511
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.247784   0.036714 0.173000 0.322567
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.235268   0.026532 0.181223 0.289313
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.066926   0.010213 0.046123 0.087728
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.174492   0.010501 0.153102 0.195882
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.181951   0.028336 0.124233 0.239669
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.195576   0.013476 0.168125 0.223027
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.168410   0.010795 0.146421 0.190399
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1546980  0.0093702 0.1354373 0.1739588
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.162666   0.011066 0.140126 0.185206
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.42728    0.28840 -0.16016  1.01472
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0900834  0.0021351 0.0857344 0.0944324
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1573077  0.0065037 0.1440602 0.1705553
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16319    0.01761 0.12732 0.19906
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20914    0.01403 0.18056 0.23772
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.17905    0.00849 0.16171 0.19639
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1587569  0.0098007 0.1387411 0.1787727
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.211026   0.012571 0.185419 0.236633

    ## # A tibble: 75 × 4
    ## # Groups:   is [75]
    ##    is         data               ll.4.mod ec50     
    ##    <chr>      <list>             <list>   <list>   
    ##  1 ILSO_5-41c <tibble [36 × 11]> <drc>    <dbl [1]>
    ##  2 ILSO_5-42c <tibble [36 × 11]> <drc>    <dbl [1]>
    ##  3 ILSO_5-49b <tibble [36 × 11]> <drc>    <dbl [1]>
    ##  4 ILSO_6-1   <tibble [36 × 11]> <drc>    <dbl [1]>
    ##  5 ILSO_6-12B <tibble [36 × 11]> <drc>    <dbl [1]>
    ##  6 ILSO_6-2b  <tibble [36 × 11]> <drc>    <dbl [1]>
    ##  7 ILSO_6-33C <tibble [36 × 11]> <drc>    <dbl [1]>
    ##  8 ILSO_6-39C <tibble [36 × 11]> <drc>    <dbl [1]>
    ##  9 ILSO_6-15b <tibble [36 × 11]> <drc>    <dbl [1]>
    ## 10 ILSO_6-28C <tibble [36 × 11]> <drc>    <dbl [1]>
    ## # ℹ 65 more rows

## Step 5.

Now last thing we do is to unnest the ec50 column. This should just be a
single number so it will esentailly ‘extract’ the value from the
nesting.

``` r
EC50.data %>%
  group_by(is) %>%
  nest() %>%
  mutate(ll.4.mod = map(data, ~drm(.$relgrowth ~ .$conc, 
                              fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                         names = c("Slope", "Lower", "Upper", "EC50"))))) %>%
  mutate(ec50 = map(ll.4.mod, ~ED(., 
                              respLev = c(50), 
                              type = "relative",
                              interval = "delta")[[1]])) %>%
  unnest(ec50)
```

    ## Warning: There were 19 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `ll.4.mod = map(...)`.
    ## ℹ In group 4: `is = "C-MNSO2_2-10"`.
    ## Caused by warning in `log()`:
    ## ! NaNs produced
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 18 remaining warnings.

    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.106855   0.022010 0.062022 0.151687
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.177036   0.011915 0.152767 0.201305
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.234268   0.017095 0.199447 0.269088
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0172659  0.0012838 0.0146508 0.0198809
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.117014   0.012255 0.092052 0.141977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.147113   0.008233 0.130343 0.163883
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1376907  0.0077899 0.1218232 0.1535582
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118886   0.004502 0.109716 0.128057
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.206342   0.016866 0.171988 0.240696
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.175509   0.013954 0.147086 0.203932
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.65376    0.63282 -0.63525  1.94277
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.118335   0.011733 0.094404 0.142265
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.189945   0.013146 0.163097 0.216793
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0483296  0.0022658 0.0437143 0.0529448
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.190146   0.027182 0.134779 0.245514
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16580    0.01082 0.14376 0.18784
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.183297   0.017237 0.148187 0.218407
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.130147   0.010705 0.108342 0.151951
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1915200  0.0077369 0.1757605 0.2072795
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123034   0.006696 0.109395 0.136673
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1935594  0.0094277 0.1743559 0.2127629
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.198000   0.019219 0.158853 0.237148
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1114482  0.0070542 0.0970793 0.1258172
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.159440   0.010423 0.138209 0.180671
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1372654  0.0070847 0.1228343 0.1516965
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1070318  0.0055365 0.0957543 0.1183094
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.248655   0.028485 0.190633 0.306678
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.167592   0.010197 0.146821 0.188362
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1082677  0.0051459 0.0977858 0.1187495
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.184271   0.036047 0.110846 0.257695
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.123288   0.014018 0.094735 0.151841
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0998727  0.0044787 0.0907498 0.1089956
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227432   0.040614 0.144704 0.310160
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.101863   0.003487 0.094760 0.108965
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.69465    0.39164 -0.10310  1.49240
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.113975   0.012773 0.087958 0.139993
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.217436   0.027934 0.160536 0.274335
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1102721  0.0033354 0.1034780 0.1170661
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1432333  0.0093132 0.1242629 0.1622036
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.18336    0.01293 0.15695 0.20977
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.186929   0.034023 0.117626 0.256232
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0299288  0.0017812 0.0263007 0.0335569
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.200379   0.020104 0.159429 0.241329
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.30812    0.24033 -0.18142  0.79765
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.227103   0.019697 0.186983 0.267224
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20009    0.01448 0.17059 0.22958
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.223966   0.058089 0.105642 0.342290
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.288001   0.074597 0.136052 0.439951
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.369422   0.077015 0.212549 0.526296
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50  0.427766   0.230327 -0.041395  0.896927
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0991738  0.0040323 0.0909603 0.1073874
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.156127   0.021551 0.112229 0.200025
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.308127   0.019233 0.268951 0.347304
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.201737   0.012113 0.176998 0.226476
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.306968   0.078617 0.146831 0.467105
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.289597   0.081347 0.123464 0.455730
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.213191   0.024013 0.164278 0.262104
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1352667  0.0074545 0.1200824 0.1504511
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.247784   0.036714 0.173000 0.322567
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.235268   0.026532 0.181223 0.289313
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.066926   0.010213 0.046123 0.087728
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.174492   0.010501 0.153102 0.195882
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.181951   0.028336 0.124233 0.239669
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.195576   0.013476 0.168125 0.223027
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.168410   0.010795 0.146421 0.190399
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1546980  0.0093702 0.1354373 0.1739588
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.162666   0.011066 0.140126 0.185206
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50  0.42728    0.28840 -0.16016  1.01472
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.0900834  0.0021351 0.0857344 0.0944324
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1573077  0.0065037 0.1440602 0.1705553
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.16319    0.01761 0.12732 0.19906
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.20914    0.01403 0.18056 0.23772
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error   Lower   Upper
    ## e:1:50  0.17905    0.00849 0.16171 0.19639
    ## 
    ## Estimated effective doses
    ## 
    ##         Estimate Std. Error     Lower     Upper
    ## e:1:50 0.1587569  0.0098007 0.1387411 0.1787727
    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:50 0.211026   0.012571 0.185419 0.236633

    ## # A tibble: 75 × 4
    ## # Groups:   is [75]
    ##    is         data               ll.4.mod   ec50
    ##    <chr>      <list>             <list>    <dbl>
    ##  1 ILSO_5-41c <tibble [36 × 11]> <drc>    0.107 
    ##  2 ILSO_5-42c <tibble [36 × 11]> <drc>    0.249 
    ##  3 ILSO_5-49b <tibble [36 × 11]> <drc>    0.168 
    ##  4 ILSO_6-1   <tibble [36 × 11]> <drc>    0.108 
    ##  5 ILSO_6-12B <tibble [36 × 11]> <drc>    0.184 
    ##  6 ILSO_6-2b  <tibble [36 × 11]> <drc>    0.227 
    ##  7 ILSO_6-33C <tibble [36 × 11]> <drc>    0.102 
    ##  8 ILSO_6-39C <tibble [36 × 11]> <drc>    0.110 
    ##  9 ILSO_6-15b <tibble [36 × 11]> <drc>    0.123 
    ## 10 ILSO_6-28C <tibble [36 × 11]> <drc>    0.0999
    ## # ℹ 65 more rows
