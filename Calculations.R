#Set working directory
setwd("~/Documents/projects/class/FIN7809_theoryII/HW1")

#Loading in data file
data.FF1 <- read.csv(file = "FF1.csv", head = TRUE, sep = ",")
data.FF2 <- read.csv(file = "FF2.csv", head = TRUE, sep = ",")

#Loading dplyr package to transform raw data
library("dplyr")
library("ggplot2")
library("GGally")
library("reshape2")

### DATA CLEANING ###
data.1 <- tbl_df(data.FF1) #Simplify data frame using tbl_df
data.2 <- tbl_df(data.FF2) #Simplify data frame using tbl_df

glimpse(data.1) #view data type for each variable
glimpse(data.2) #view data type for each variable

#Select relevant variables
data1 <- select(data.1, date,smlo_vwret,smme_vwret,smhi_vwret,bilo_vwret,bime_vwret,bihi_vwret)
data2 <- select(data.2, date = dateff, rf)

#Create final data set of interest
data <- inner_join(data1, data2, by="date")
data.portfolio <- select(data, -date, -rf)

write.csv(data, file="FinalData.csv")

### HW PROBLEMS ##

#Initializing vectors and values of interest
n <- ncol(data.portfolio) #number of assets
i <- 1 + numeric(n) #creating unity vector

#1a The 6x1 vector mu
mu <- sapply(data.portfolio, mean)

#1b The 6x6 covariance matrix
V <- cov(data.portfolio)


#1c Find intermediate A,B,C,D
A <- t(i) %*% solve(V) %*% mu
B <- t(mu) %*% solve(V) %*% mu
C <- t(i) %*% solve(V) %*% i
D <- B * C - A^2

#1d Find the 6x1 vector g
g <- 1/drop(D) * (drop(B) * (solve(V) %*% i) - drop(A) * (solve(V) %*% mu))

#1e Find the 6x1 vector h
h <- 1/drop(D) * (drop(C) * (solve(V) %*% mu) - drop(A) * (solve(V) %*% i))

#1f Find the globabl minimum variance portfolio wg
w.gl <- 1/drop(C) * solve(V) %*% i

#1g Find the globabl minimum variance portfolio mug
mu.gl <- drop(A) / drop(C)

#1h Find the globabl minimum variance portfolio sigmag
sigma.gl <- 1 / drop(C)
sigma.gl

#Find weight for an efficient portfolio with a mean equal to 3.5%, and call this portfolio p
mu.p <- .035
w.p <- g + h * mu.p
