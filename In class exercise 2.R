#Exerxise 1
library(tidyverse)
library(knitr)
set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))
plot(dat)


dat$d <- abs(dat$x)
kable(head(dat))

df <-arrange(dat,d)
df[1:6, ]

mean(df$y)


dat_loess <- filter(dat,d<0.454)
dat_loess
mean(dat_loess$y)

#Exercise 2
library(tidyverse)
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x){
  dat$d <- abs(dat$x-x)
  df <-arrange(dat,d)
  df[1:6, ]
  
  mean(df$y)
  ## YOUR CODE HERE FOR kNN
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for kNN from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
loess_estimates <- map_dbl(xgrid, function(x){
  dat_loess <- filter(dat,d<0.454)
  dat_loess
  mean(dat_loess$y)
  ## YOUR CODE HERE FOR LOESS
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for loess from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y)) +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()
