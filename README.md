# Reproducibility-Class
##Spring class
##first class
##Priya 
# use basic mathematical operators in R 
#know some data objects and how to index them 
#install packages 
#load your data into R from a .csv or .txt file

##mathematical operators
2+2
2-2
2*2
2/2
###Create an R object
x=5
y=6
x+y
x-y
x*y
x/y
name <- "Priya"
eight <- "8"
name + eight #Error


###Each function starts with a name and opens parentheses and
###you type what you want to do to the object in the parentheses.
class(name)
class(eight)

#concatenate function to create vector or a bunch of scalar elements
vec1 <- c(1,2,3,4) # numeric vector 
vec1 <- c(1:4)# same numeric vector as above, the ':' (colon) generates a sequence
vec1 <- 1:4 # also works without the concatenate function
vec2 <- c("Priya", "Priyanka", "Pramod") # character vector
vec3 <- c( "True", "False")  # logical vector

vec1 + 4

#Bulit-in Functions 

mean(vec1)# mean
sd(vec1) # standard deviation
sum(vec1) # sum
median(vec1) # median
max(vec1) # maximum
min(vec1)# minimum 
summary(vec1)#IQR - output depends on the data class
abs(vec1)# absolute value
sqrt(vec1)# square root (with subtraction)
log10(vec1)# log base 10
log(vec1) # natural log
exp(vec1) # power of e


#logical operators
10 > 5 # greater than
4 < 8 # less than
5 <= 3 # less than or equal to
3 == 3 # equal to
1 == 2 | 1 == 1 # | means 'OR'
1 == 2 & 1 == 1 # & means 'AND' 
2 == 2 & 2 == 2

t <- 1:10  # numeric vector
t[(t > 5) | (t < 2)]
t[(t > 6) & (t < 9)]
t[t!=5]
t[!t == 5]
6 %in% t
5 %in% t

##### Data Type ########
#matrices 
mat1 = matrix(data= c(1,6,10), nrow=3, ncol=3)
mat1
mat2 = matrix(data=c("Priya", "Priyanka", "Pramod"), nrow=3, ncol=3)
mat2

#dataframe
df<-data.frame(mat1[,1], mat2[,1])
df

colnames(df) <- c("value", "names")
df

#Indexing: Indexing is a fancy way to say subsetting

vec1[1]
vec2[2]

mat1[1]
mat2[2]
mat1[4]
mat1[2,3]
mat1[1,]
mat1[,3]
df[1,2]
df[3,"value"]
df[,"value"]
df[,"names"]
df$value
df$names
df$names[2]
df$names[2:3]

#Fancy subset examples
df$value[df$name=="Priya"]
df$value[df$name !="Priya"]
df$value[!df$name=="Priya"]
df$value[df$name %in% c("Priya", "Priyanka")]
df$value[df$name %in% c("Priya", "Priyanka", "Pramod")]
df$value[!df$name %in% c("Priya", "Priyanka", "Pramod")]
df[df$names=="Priya"]  #returns all rows with the value Priya in the name column.
df[df$names=="Priyanka"]
df[df$names=="Pramod"]
susbset(df, names == "Priya") #subset function

#making a new column in a dataframe
df$log_value <- log(df$value)
df$log_value
df

#Installing packages from CRAN repository
install.packages("ggplot2")  # use quotes around the package name
install.packages(tidyverse)
install.packages(lme4)

library(ggplot2)
library(tidyverse)
library(lme4)

##In an R project your working directory is the directory where your R project is saved!!
getwd()

#reading the data in CSV
datum1<-read.csv(file.choose(), na.strings="na")
