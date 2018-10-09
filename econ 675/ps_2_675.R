#===========================#
# ==== Metrics 675 ps 2 ====
#===========================#



#=======================================#
# ==== load packages and clear data ====
#=======================================#

library(data.table)

# clear data and consol 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

#================================================#
# ==== Question 1: Kernel Density Estimation ====
#================================================#

#=================#
# ==== Part a ====
#=================#


# now to find the theoretically optimal H I need to calculate integral of second derivative.
# second dericative of normal function is
phi_2 <- function(x, mean, v){ 
  
  dnorm(x=x,mean=mean,sd=sqrt(v))*(((x - mean)/v)^2-(1/v))
  
}

# now create the function to integrate 
f_int <- function(x){
  
  f_out <- (.5*phi_2(x=x, -1.5,1.5) + .5*phi_2(x=x, 1,1))^2
  return(f_out)
}

# and the integral is 
v2k <- integrate(f_int, lower = -Inf, upper = Inf)$val

# so optimal bandwith is 
h_opt <- (((3/5))/(v2k*(1/5)^2*1000))^(1/5)

#=================#
# ==== part b ====
#=================#

# set parms 
n     <- 1000

# start data.table for random data, take a random draw for weighted normals 
r_dt <- data.table( r1 = sample(1:2,prob=c(.5,.5),size=n,replace=T) )

# draw a random number from appropriate normal dist according to r1
r_dt[r1 == 1, rdraw := rnorm(.N,-1.5,1.5)]
r_dt[r1 == 2, rdraw := rnorm(.N,1,1)]
r_dt[, r1 := NULL]

# kernal function 
K0 <- function(u){
  
  out <- .75 * (1-u^2) * (abs(u) <= 1)
  return(out)
}


# for now just set a bandwidth 
h <- .5
r_dt[, rdraw]


# define variables for debug 
in_data <- r_dt
x_v <- "rdraw"
  imse_f <- function(in_data, x_v){
    
    # copy the data to aviod editing it in global enviorment 
    data <- copy(in_data)
    
    # add a constant for the merge 
    in_data[, const := 1]
    
    # cartesian merge to get all pairs 
    paired_dt <- merge(in_data, in_data, by = "const", allow.cartesian = TRUE)
    
    # get new variable names after the merge 
    x_vx <- paste0(x_v, ".x")
    x_vxi <- paste0(x_v, ".y")
    
    # get the kernal thing for each pair 
    paired_dt[, f_hatx := K0((get(x_vxi) - get(x_vx))/h)]
    
    # now mean the kernal by rdraw.x and devide by h 
    f_hats <- paired_dt[, list(f_hat_x = mean(f_hatx)/h), by = get(x_vx)]
    
    # now get the f_hats for the leave one out by deleating the observation where x= xi 
    paired_dt <- paired_dt[get(x_vxi) != get(x_vx), ]
    f_hats_lo <- paired_dt[, list(f_hat_x = mean(f_hatx)/h), by = get(x_vx)]
    
    
    
    
  }
  




