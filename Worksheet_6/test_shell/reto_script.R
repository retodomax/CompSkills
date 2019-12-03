library(tidyverse)
library(mgcv)
library(boot)
data <- read.table("../../frequency.dat")
names(data) <- c("year", "freq")

stat <- function(data, index){
  tryCatch({
    train <- data[index,]
    test <- data[-index,]
    if(!is_empty(test)){
      fit_gam <- gam(log(freq) ~ s(year), data = train)
      fit_ssfpl <- nls(freq ~ SSfpl(year, A, B, xmid, scal), data = train)
      gam_pred <- unname(exp(predict(fit_gam, newdata = test)))
      ssfpl_pred <- predict(fit_ssfpl, newdata = test)
      gam_rmse <- sqrt(mean((test$freq - gam_pred)^2))
      ssfpl_rmse <- sqrt(mean((test$freq - ssfpl_pred)^2))
      rmse <- c(gam_rmse, ssfpl_rmse)
      rmse
    } else {
      stop("no points in test set")
    }
  }, error = function(err) {return(c(NA, NA))})
}
myboot <- boot(data = data, statistic = stat, R = 2000, parallel = c("snow"))
(rmse_mean <- apply(myboot$t, 2, mean, na.rm = T))