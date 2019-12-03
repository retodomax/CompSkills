#' ---
#' project: CompSkills    #################################################
#' title:   Homework 5
#' author:  "[Reto Zihlmann](https://retodomax.github.io/)"
#' date:    2019-11-19 18:40:38
#' output:  github_document   #############################################
#' ---



# packages ----------------------------------------------------------------

library(mgcv)


# Single example ----------------------------------------------------------


data <- read.table("frequency.dat")
names(data) <- c("year", "freq")
plot(freq ~ year, data = data, ylab = "Freqency [MHz]",
     ylim = c(-1000, 6000))


# Assume a true function

fpl <- function(x, A = 0, B = 3000, xmid = 2003, scal = 3){
  A+(B-A)/(1+exp((xmid-x)/scal))
}
curve(fpl, from = 1970, to = 2020, add = T)


# sample new points
set.seed(1)
true_value <- fpl(data$year)
value <- abs(true_value + rnorm(length(true_value), mean = 0, sd = true_value/3))
points(data$year, value, col = 'green')
sim_data <- data.frame(year = data$year, freq = value)


# fit with GAM
fit <- gam(log(freq) ~ s(year), data = sim_data)


# fit with SSfpl()
fit_ssfpl <- nls(freq ~ SSfpl(year, A, B, xmid, scal), data = sim_data)


# plot all
plot(freq ~ year, data = sim_data, ylab = "Freqency [MHz]",
     ylim = c(-1000, 6000))
lines(sim_data$year,exp(predict(fit)), col = "red", lwd = 2)
lines(sim_data$year, predict(fit_ssfpl), col = 'green', lwd = 2)
curve(fpl, lwd = 2, add = T)
legend('topleft', legend = c('True model', 'GAM fit', 'FPL fit'),
       lwd = 2, col = c('black', 'red', 'green'))
abline(v = 2008, lty = 2)
abline(v = 2017, lty = 2)

# question: which model is closer to true function at year = 2008
# and year = 2017


# simulation --------------------------------------------------------------

sim_diff <- function() {
  true_value <- fpl(data$year)
  value <- abs(true_value + rnorm(length(true_value), mean = 0, sd = true_value/3))
  sim_data <- data.frame(year = data$year, freq = value)
  fit <- gam(log(freq) ~ s(year), data = sim_data)
  fit_ssfpl <- nls(freq ~ SSfpl(year, A, B, xmid, scal), data = sim_data)
  gam_pred <- unname(exp(predict(fit, newdata = list(year = c(2008, 2017)))))
  ssfpl_pred <- predict(fit_ssfpl, newdata = list(year = c(2008, 2017)))
  true_values <- fpl(c(2008, 2017))
  c(gam_pred-true_values, ssfpl_pred-true_values)
}

## maybe include:
# - variate the number of data points (n)
#    (at the moment n always the number of observations in real data)
# - variate the distribution of the errors
#    (symmetirc(normal)/nonsymetric(chisquare, lognormal), heavy tailed,
#     different support (only positive numbers),
#     discrete/count, ...)

sim_diff()


out <- replicate(1000, sim_diff())
out <- t(out)


# plot simulation results -------------------------------------------------

plot(density(out[,1]), main = 'Difference to true function at 2008',
     xlim = c(-1000, 1000), ylim = c(0, 0.005),
     col = 'red', lwd = 2)
lines(density(out[,3]),
      col = 'green', lwd = 2)
abline(v = 0, lty = 2)
legend('topleft', legend = c('True model', 'GAM fit', 'FPL fit'),
       lwd = 2, col = c('black', 'red', 'green'))



plot(density(out[,2]), main = 'Difference to true function at 2008',
     xlim = c(-1000, 1000), ylim = c(0, 0.005),
     col = 'red', lwd = 2)
lines(density(out[,4]),
      col = 'green', lwd = 2)
abline(v = 0, lty = 2)
legend('topleft', legend = c('True model', 'GAM fit', 'FPL fit'),
       lwd = 2, col = c('black', 'red', 'green'))


## finally evaluate

# for estimator
# -bias
# -MSE (variance)
# -distribution

# for CI
# -width (how wide are the CI)
# -coverage (do they include the true parameter)

# for curve
# -point-wise (only compare at specific locations)
# -integrate (AMSE: average MSE,
#             IMSE: integrated MSE)


