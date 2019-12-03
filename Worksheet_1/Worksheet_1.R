myfun <- function(x1, x2 = 4){
  10^6 * x2^-6 * exp(-1/(2*x2^2) * (120 - 6*x1 + x1^2))
}

ind_myfun <- function(x1, x2){
  as.numeric(myfun(x1, x2) >= 2)
}

dx <- 0.1
x1range <- seq(-10, 15, dx)
x2range <- seq(1, 20, dx)


## i)

val <- outer(x1range, x2range, FUN = ind_myfun)
image(x = x1range, y = x2range, z = val)

## calculate integral
sum(val) * dx * dx

## ii)

val <- outer(x1range, x2range, FUN = myfun)
val[val < 2] <- 0
image(x = x1range, y = x2range, z = val)
contour(x = x1range, y = x2range, z = val, levels = 2, add = T)

## calculate integral
sum(val) * dx * dx
