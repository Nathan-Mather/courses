# ==== graphs ====
#=================#

library(easimple)
library(ggplot2)
library(data.table)
ea_start()


# ginitialize blank graph
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))

fun_1 <- function(x) x*((10-x)/(10-5))
p <-  p + stat_function(fun = fun_1) + xlim(c(0,10))


print(p)
