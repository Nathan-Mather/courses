

n_iter <- 100
pi <- .75

# define functions 
fun_1 <- function(x) (x+(10/x)-20)/(1+(10/x)-20)
fun_2 <- function(x) (x/(1+(10/(1-x)) -20))

for( i in 1:n_iter){
  
  # if pi less than 1/2 update pi 
  if(pi < .5){
    
    pi <- fun_1(pi)
    
  }
  # if pi greater than 1/2 update pi
  if( pi >= .5 ){
    
    pi <- fun_2(pi)
    
  }
  
  if(pi <.00000000001){stop(print("pi is at 0"))}
}


print(pi)
