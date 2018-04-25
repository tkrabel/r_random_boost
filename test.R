##
##                        HOW TO USE RANDOM BOOST
##

source("predict_rboost.R")
source("rboost_train.R")
source("rboost.R")
source("tree.R")

df  <- mtcars
fit <- rboost(formula = mpg ~ cyl + disp + hp + wt + qsec, data = df,
              params = list(eta = 0.3, max_depth = 3, n_rounds = 50))
predict(fit)
