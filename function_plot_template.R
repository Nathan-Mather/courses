# ==== graphs ====
#=================#

library(easimple)
library(ggplot2)
library(data.table)
ea_start()


# ginitialize blank graph
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p2 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))

fun_1 <- function(x) (x+(10/x)-20)/(1+(10/x)-20)
fun_2 <- function(x) (x/(1+(10/(1-x)) -20))
p <-  p + stat_function(fun = fun_1) +  xlim(c(0,.5))
p2 <-  p2 + stat_function(fun = fun_2) +  xlim(c(.5,1))


print(p)
print(p2)
