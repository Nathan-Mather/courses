#===========================#
# ==== Metrics 675 ps 2 ====
#===========================#



#=======================================#
# ==== load packages and clear data ====
#=======================================#

library(data.table)
library(doParallel)
library(foreach)

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
  M     <- 1000
  
  # generate data for debugging functions 
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
  
  # define the true f(x) function 
  f_x <- function(x){
    
    .5*dnorm(x, -1.5,1.5)+.5*dnorm(x,1,1)
  }
  

  #=============================#
  # ==== Make imse function ====
  #=============================#
  
    # define variables for debug 
    # in_data <- r_dt
    # x_v <- "rdraw"
    imse_f <- function(in_data, x_v, f_x = f_x){
      
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
      f_hats <- paired_dt[, list(f_hat_x = mean(f_hatx)/h), by = x_vx]
      
      # now get the f_hats for the leave one out by deleating the observation where x= xi 
      paired_dt <- paired_dt[get(x_vxi) != get(x_vx), ]
      f_hats_lo <- paired_dt[, list(f_hat_x = mean(f_hatx)/h), by = x_vx]
      
      # now add in f_x for each 
      f_hats[, f_x := f_x(get(x_vx))]
      f_hats_lo[, f_x := f_x(get(x_vx))]
      
      # now do squared error 
      f_hats[, sq_er := (f_hat_x - f_x)^2]
      f_hats_lo[, sq_er := (f_hat_x - f_x)^2]
      
      # now get imse 
      imse_li <- f_hats[, mean(sq_er)]
      imse_lo <- f_hats_lo[, mean(sq_er)]
      
      output <- data.table(imse_li = imse_li, imse_lo= imse_lo)
  
      return(output)
  
    }
    
  
  #==========================#
  # ==== run simulations ====
  #==========================#
  
  # note: pretty sure it would be faster yet to just include the simulations in the by group of the data table 
  # operations in the IMSE function. Probably marginally faster but kind of hard to wrap my head around. 
  
  # create vector of h's 
  h_v <- seq(.5, 1.5,.1)
  
  # create list for output for each h 
  h_list <- vector("list", length = length(h_v))
  
  # also lets time this
  start_t <- Sys.time()
  
  # for now for every bandwidth do the following
  for(h in h_v){
  
    # now to M simulations 
    sim_function <- function(i, n, f_x){
      
      # generate data 
      # start data.table for random data, take a random draw for weighted normals 
      r_dt <- data.table( r1 = sample(1:2,prob=c(.5,.5),size=n,replace=T) )
      
      # draw a random number from appropriate normal dist according to r1
      r_dt[r1 == 1, rdraw := rnorm(.N,-1.5,1.5)]
      r_dt[r1 == 2, rdraw := rnorm(.N,1,1)]
      r_dt[, r1 := NULL]
      
      
      # get IMSE 
      results_i <- imse_f(in_data = r_dt, x_v = "rdraw" ,f_x = f_x)
      results_i[, sim := i]
      return(results_i)
      
    }
    
    # parallel setup
    cl <- makeCluster(4, type = "PSOCK")
    registerDoParallel(cl)
    
    # run simulations in parallel
    output_list <- foreach(m = 1 : M,
                           .inorder = FALSE,
                           .packages = "data.table",
                           .options.multicore = list(preschedule = FALSE, cleanup = 9)) %dopar% sim_function(i = m, n = n, f_x = f_x)
    
    # stop clusters 
    stopCluster(cl)
    
    # bind list
    output_dt <- rbindlist(output_list)
    
    # now take the mean of imse 
    imse_h <- output_dt[, list(imse_li = mean(imse_li), imse_lo = mean(imse_lo)) ]
    
    # add in h 
    imse_h[, h := h]
    
    h_list[[(h*10-4)]] <- imse_h
    
  }
  
  # AND TIME 
  run_time <- Sys.time() - start_t
  
  # bind results 
  part_b_res <- rbindlist(h_list)
  
  # make them pretty 
  part_b_res_pretty <- signif(part_b_res, 3)
  
  
#====================#
# ==== save data ====
#====================#

  # save IMSE by h results 
  
  
  
  #===============================================================#
  # ==== dont need this anymore caues I can do it in paralell ====
  #===============================================================#
  
  #   # start a list for results 
  #   results_list <- vector("list", length = M)
  # # now to M simulations 
  #   for(i in 1:M){
  #     
  #     # generate data 
  #     # start data.table for random data, take a random draw for weighted normals 
  #     r_dt <- data.table( r1 = sample(1:2,prob=c(.5,.5),size=n,replace=T) )
  #     
  #     # draw a random number from appropriate normal dist according to r1
  #     r_dt[r1 == 1, rdraw := rnorm(.N,-1.5,1.5)]
  #     r_dt[r1 == 2, rdraw := rnorm(.N,1,1)]
  #     r_dt[, r1 := NULL]
  #     
  #     
  #     # get IMSE 
  #     results_i <- imse_f(in_data = r_dt, x_v = "rdraw")
  #     results_i[, sim := i]
  #     results_list[[i]] <- results_i
  #     
  #   }
  # 
  #   # bind results 
  #   results_dt <- rbindlist(results_list)
  