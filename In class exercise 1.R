#Exercise 1
#Oracle Regression

library(tidyverse)
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

#1. Generate Data
genreg(1000)
dat<- genreg(1000)

#2 
dat <- mutate(dat, yhat = 0, yhat1 = 5-x1, yhat2 = 5+2*x2, yhat3=5-x1 + 2*x2)
dat

#3. Calculating MSE
mse <- mean((dat$yhat-dat$y)^2)
mse1 <- mean((dat$yhat1-dat$y)^2)
mse2 <- mean((dat$yhat2-dat$y)^2)
mse3 <- mean((dat$yhat3-dat$y)^2)

#4. Ordering best forecast
#Case4 x1 & x2
#Case3 x2
#Case2 x1
#Case1 null

#Oracle Classfication


#1. Calculating Probabilities
#x=1
x<- 1
pA <- 0.2
pB <- 0.8/(1+exp(-x))
y <- map_chr(pB, function(x) 
  sample(LETTERS[1:3], size=1, replace=TRUE,
         prob=c(0.2, x, 1-x)))
pC <- 1- pB - pA

#x=-2
x1<- -2
pA <- 0.2
pB <- 0.8/(1+exp(-x1))
y <- map_chr(pB, function(x1) 
  sample(LETTERS[1:3], size=1, replace=TRUE,
         prob=c(0.2, x1, 1-x1)))
pC <- 1- pB - pA

#2. Classification
# If x <0 then C
# If X> 0 then B

#3. Generate Data
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x)))
  tibble(x=x, y=y)
}

gencla(100)
data <- gencla(100)

#4. Predictions

data <- mutate(data, yhat = sapply(x, function(x_) 
  if(x_<0) "C" else "B"))
data

#Error Rate
mse4 <- 1- mean(data$yhat==data$y)
mse4
