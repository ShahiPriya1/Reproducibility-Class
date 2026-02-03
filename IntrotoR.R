Z = 1:200 # Create a vector named 'Z' with values from 1 to 200

mean(Z) #mean of Z
sd(Z)  # standard deviation of Z
Zlog = (Z > 30) # Create a logical vector 'Zlog'
Zdf= data.frame(Z,Zlog) # Make a dataframe with columns Z and Zlog
Zdf
colnames(Zdf) =c("Zvec", "Zlog") # Change column names to "Zvec" and "Zlogic"
Zdf
Zdf$Zsquared= (Zdf$Zvec)^2 # Create a new column 'Zsquared' which is Zvec squared
Zdf
Zdf$Zsquared[(Zdf$Zsquared>10) & (Zdf$Zsquared<100)] # Subset dataframe for Zsquared > 10 and < 100 (without subset())

subset(Zdf,Zsquared>10 & Zsquared<100) # Subset dataframe for Zsquared > 10 and < 100 (using subset() function)

Zdf[26,] # Subset dataframe to only include row 26

Zdf[180, "Zsquared"]# Subset to only include Zsquared value in the 180th row
