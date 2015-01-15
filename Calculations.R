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
data <- merge(data1, data2)
data.portfolio <- select(data, -date, -rf)

write.csv(data, file="FinalData.csv")
?cov()
### HW PROBLEMS ##

#Initializing vectors and values of interest
n <- ncol(data.portfolio) #number of assets
i <- 1 + numeric(n) #creating unity vector

#1a The 6x1 vector mu
mu <- sapply(data.portfolio, mean)
mu
#1b The 6x6 covariance matrix
V <- cov(data.portfolio)
V

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

#1i Find weight for an efficient portfolio with a mean equal to 3.5%, and call this portfolio p
mu.p <- .035
w.p <- g + h * mu.p

#1j Find the wieghts, $w_{op}$ and the mean , $\mu_{op}$ for portfolio $p$'s zero beta portfolio
mu.0p <- (-D/C^2)/(mu.p - (A/C)) + A/C
mu.0p

#1k Find the regression beta of the first portfolio's return with respect to portfolio $p$.

port_vwret <- as.matrix(data.portfolio) %*% w.p
port_vwret <- data.frame(port_vwret)
data <- tbl_df(cbind(data, port_vwret))
w.p
#Creating separate regression data table in percent instead of decimals 
data.regression <- select(data, smlo_vwret, port_vwret) * 100
head(data.regression, n=4)

#Run regression
reg <- lm(smlo_vwret ~ port_vwret, data=data.regression)
results <- summary(reg)
beta <- results$coef[2,1]

mu.expression <- mu.0p + beta * (mu.p - mu.0p)
round(mu.expression, 5) == round(mu[1],5)

#Standard Deviation and Plot
v <- diag(V)
s <- sqrt(v)
retrisk <- mu/s
retrisk
max(retrisk)
qplot(s, mu) + theme_bw()

#Efficient Frontier

alpha <- seq(0,2,.01)
return.eff = NULL
std.eff = NULL

for (j in 1:length(alpha)) {
return.eff[j] = alpha[j] * mu.gl + (1 - alpha[j]) * mu.p
weight.eff = alpha[j] * (g + h*mu.gl) + (1 - alpha[j]) * (g + h*mu.p)
std.eff[j] = sqrt(diag(t(weight.eff) %*% V %*% weight.eff))
}

frontier.data <- data.frame(cbind(return.eff, std.eff))
ggplot() + 
  geom_path(data=frontier.data, aes(x=std.eff, y=return.eff)) + 
  geom_point(aes(x=s, y=mu)) + theme_bw()

#1o Tangent portfolio
rf <- mean(data$rf)
z <- solve(V) %*% (mu - rf)
z.sum <- t(i) %*% z
w_t <- 1/drop(z.sum) * z
w_t
data.portfolio

mu.t <- mean(as.matrix(data.portfolio) %*% w_t)
std.t <- sqrt(diag(t(w_t) %*% V %*% w_t))

ggplot() + 
  geom_path(data=frontier.data, aes(x=std.eff, y=return.eff)) + 
  geom_point(aes(x=s, y=mu)) + 
  geom_point(aes(x=std.t, y=mu.t)) +
  theme_bw()

#1p proportion in complete portfolio

A = 3
y <- (mu.t - rf)/(0.5 * A * std.t^2)
y

