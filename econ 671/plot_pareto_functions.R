#=================#
# ==== graphs ====
#=================#

library(ggplot2)
ea_start()
# create theta vector 
theta_v <- seq(.1,2,.05)


# ginitialize blank graph
p <- ggplot(data = data.frame(x = rep.int(1, length(theta_v))), mapping = aes(x = x))

# Define function
fun_1 <- function(x, theta) theta*x^(1-theta)

# create a list of functions of x with theta defined 
fun_list <- lapply(theta_v, function(theta){
  
  fun <- function(x) theta*x^(1-theta)
  
  return(fun)
})

# loop over functions to create layered function plot 
for(i in 1:length(theta_v)){
  

 p <-  p + stat_function(fun = fun_list[[i]]) 
  
}


p <- p +  xlim(0,20)


print(p)
