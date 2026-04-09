# Question 1 (Regarding Reproducibility)

The main point of writing your own functions and using iterations is to
make your analysis consistent, repeatable, and efficient. Functions
allow you to reuse the same code without rewriting it, which reduces
errors and ensures the same method is applied every time. Loops help
automate repetitive tasks, making it easier to reproduce results on new
or larger datasets.

# Question 2 (Conceptual)

1)  Function in R: A function in R is created using the function() and
    assigned a name. Inputs are dfined in parentheses, and the code is
    written inside {}. It returns a result using return(). Functions are
    written in scripts or R Markdown to resue code.

2)  For loop in R: A for loop repeats code multiple times using for (i
    in …), where i goes through a sequence. The code inside{} runs for
    each value of i. Results are usually stored in a variable that
    updates each iteration.

# Question 3 (Load Data)

``` r
Cities <-read.csv("Cities.csv")
```

# Question 4 (Function Writing)

``` r
 Haversine<- function(lat1, lon1, lat2, lon2) {rad.lat1 <- lat1 * pi/180
  rad.lon1 <- lon1 * pi/180
  rad.lat2 <- lat2 * pi/180
  rad.lon2 <- lon2 * pi/180
  
  # Haversine formula
  delta_lat <- rad.lat2 - rad.lat1
  delta_lon <- rad.lon2 - rad.lon1
  a <- sin(delta_lat / 2)^2 + cos(rad.lat1) * cos(rad.lat2) * sin(delta_lon / 2)^2
  c <- 2 * asin(sqrt(a))
  
  # Earth's radius in kilometers
  earth_radius <- 6378137
  
  # Calculate the distance
  distance_km <- (earth_radius * c) / 1000
  
  return(distance_km) 
}
colnames(Cities)
```

    ##  [1] "city"        "city_ascii"  "state_id"    "state_name"  "county_fips"
    ##  [6] "county_name" "lat"         "long"        "population"  "density"

# Question 5

``` r
auburn <- Cities$lat[Cities$city == "Auburn" ]
auburn2 <- Cities$long[Cities$city == "Auburn" ]

nyc <- Cities$lat[Cities$city == "New York"]
nyc2 <- Cities$long[Cities$city == "New York"]

# Calculate distance 
 auburn_nyc_dist <- Haversine(auburn, auburn2, nyc, nyc2)

auburn_nyc_dist
```

    ## [1] 1367.854

# Question 6 (Loop)

``` r
for (i in 1:nrow(Cities)) {
  
  dist <- Haversine(auburn, auburn2,
                    Cities$lat[i],
                    Cities$long[i])
  
  print(dist)
}
```

    ## [1] 1367.854
    ## [1] 3051.838
    ## [1] 1045.521
    ## [1] 916.4138
    ## [1] 993.0298
    ## [1] 1056.022
    ## [1] 1239.973
    ## [1] 162.5121
    ## [1] 1036.99
    ## [1] 1665.699
    ## [1] 2476.255
    ## [1] 1108.229
    ## [1] 3507.959
    ## [1] 3388.366
    ## [1] 2951.382
    ## [1] 1530.2
    ## [1] 591.1181
    ## [1] 1363.207
    ## [1] 1909.79
    ## [1] 1380.138
    ## [1] 2961.12
    ## [1] 2752.814
    ## [1] 1092.259
    ## [1] 796.7541
    ## [1] 3479.538
    ## [1] 1290.549
    ## [1] 3301.992
    ## [1] 1191.666
    ## [1] 608.2035
    ## [1] 2504.631
    ## [1] 3337.278
    ## [1] 800.1452
    ## [1] 1001.088
    ## [1] 732.5906
    ## [1] 1371.163
    ## [1] 1091.897
    ## [1] 1043.273
    ## [1] 851.3423
    ## [1] 1382.372
    ## [1] 0

# Qusetion 7 (dataframe)

``` r
distance_df <- NULL

for (i in 1:nrow(Cities)) {
    result_i <- data.frame(
      City1 = Cities$city[i],
      City2 = "Auburn",
      Distance_km = Haversine(auburn, auburn2,
                              Cities$lat[i],
                              Cities$long[i]))
    
    distance_df <- rbind.data.frame(distance_df, result_i)
}
print(distance_df)
```

    ##            City1  City2 Distance_km
    ## 1       New York Auburn   1367.8540
    ## 2    Los Angeles Auburn   3051.8382
    ## 3        Chicago Auburn   1045.5213
    ## 4          Miami Auburn    916.4138
    ## 5        Houston Auburn    993.0298
    ## 6         Dallas Auburn   1056.0217
    ## 7   Philadelphia Auburn   1239.9732
    ## 8        Atlanta Auburn    162.5121
    ## 9     Washington Auburn   1036.9900
    ## 10        Boston Auburn   1665.6985
    ## 11       Phoenix Auburn   2476.2552
    ## 12       Detroit Auburn   1108.2288
    ## 13       Seattle Auburn   3507.9589
    ## 14 San Francisco Auburn   3388.3656
    ## 15     San Diego Auburn   2951.3816
    ## 16   Minneapolis Auburn   1530.2000
    ## 17         Tampa Auburn    591.1181
    ## 18      Brooklyn Auburn   1363.2072
    ## 19        Denver Auburn   1909.7897
    ## 20        Queens Auburn   1380.1382
    ## 21     Riverside Auburn   2961.1199
    ## 22     Las Vegas Auburn   2752.8142
    ## 23     Baltimore Auburn   1092.2595
    ## 24     St. Louis Auburn    796.7541
    ## 25      Portland Auburn   3479.5376
    ## 26   San Antonio Auburn   1290.5492
    ## 27    Sacramento Auburn   3301.9923
    ## 28        Austin Auburn   1191.6657
    ## 29       Orlando Auburn    608.2035
    ## 30      San Juan Auburn   2504.6312
    ## 31      San Jose Auburn   3337.2781
    ## 32  Indianapolis Auburn    800.1452
    ## 33    Pittsburgh Auburn   1001.0879
    ## 34    Cincinnati Auburn    732.5906
    ## 35     Manhattan Auburn   1371.1633
    ## 36   Kansas City Auburn   1091.8970
    ## 37     Cleveland Auburn   1043.2727
    ## 38      Columbus Auburn    851.3423
    ## 39         Bronx Auburn   1382.3721
    ## 40        Auburn Auburn      0.0000
