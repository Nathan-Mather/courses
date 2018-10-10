#===========================#
# ==== Metrics 675 ps 2 ====
#===========================#



#=======================================#
# ==== load packages and clear data ====
#=======================================#

library(data.table)
library(doParallel)
library(foreach)
library(ggplot2)

# clear data and consol 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# set options 
opt_test_run <- TRUE

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
h_opt <- (15/(v2k*1000))^(1/5)

#=================#
# ==== part b/d ====
#=================#
  
  # set parms 
  n     <- 1000
  M <- ifelse(opt_test_run, 10, 1000)
  
  
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

    # # generate data for debugging functions
    # # start data.table for random data, take a random draw for weighted normals
    # r_dt <- data.table( r1 = sample(1:2,prob=c(.5,.5),size=n,replace=T) )
    # 
    # # draw a random number from appropriate normal dist according to r1
    # r_dt[r1 == 1, rdraw := rnorm(.N,-1.5,1.5)]
    # r_dt[r1 == 2, rdraw := rnorm(.N,1,1)]
    # r_dt[, r1 := NULL]
    # in_data <- r_dt
    # h_v <- c(.5,.6)

    imse_f <- function(in_data, x_v, h_v = NULL, f_x = f_x){
    
      # copy the data to aviod editing it in global enviorment 
      data <- copy(in_data)
      
      # add a constant for the merge 
      in_data[, const := 1]
      
      # cartesian merge to get all pairs 
      paired_dt <- merge(in_data, in_data, by = "const", allow.cartesian = TRUE)
      
      # get new variable names after the merge. This kind of annoyingly general for a HW assingment. I regret nothing...
      x_vx <- paste0(x_v, ".x")
      x_vxi <- paste0(x_v, ".y")
      
      # initialize a list for output from each h 
      ouput_list <- vector("list", length= length(h_v))
      
      # now do the imse calculations for each h in h_v
      for(i in 1:length(h_v)){
        
        h <- h_v[[i]]
        
        # get the kernal thing for each pair 
        paired_dt[, k_x := K0((get(x_vxi) - get(x_vx))/h)]
        
        # now mean the kernal by rdraw.x and devide by h 
        f_hats <- paired_dt[, list(f_hat_x = mean(k_x)/h), by = x_vx]
        
        # now get the f_hats for the leave one out by deleating the observation where x= xi. This will be rows 
        # 1, M+2, 2M+3, 3M+4 ... so eq(1, M*M, M+1) should take care of those 
        paired_dt_lo <- paired_dt[-c(seq(1, M*M, M+1)), ]
        
        # now get the mean of the f_hats leacing out the x 
        f_hats_lo <- paired_dt_lo[, list(f_hat_x = mean(k_x)/h), by = x_vx]
        
        # now add in f_x for each 
        f_hats[, f_x := f_x(get(x_vx))]
        f_hats_lo[, f_x := f_x(get(x_vx))]
        
        # now do squared error 
        f_hats[, sq_er := (f_hat_x - f_x)^2]
        f_hats_lo[, sq_er := (f_hat_x - f_x)^2]
        
        # now get imse 
        imse_li <- f_hats[, mean(sq_er)]
        imse_lo <- f_hats_lo[, mean(sq_er)]
        
        # now put into a data.table and put in list 
        ouput_list[[i]] <- data.table(imse_li = imse_li, imse_lo= imse_lo, h = h)
      }
        
      output <- rbindlist(ouput_list)
  
      return(output)
  
    }
    
  
  #==========================#
  # ==== run simulations ====
  #==========================#
  
  # note: pretty sure it would be faster yet to just include the simulations in the by group of the data table 
  # operations in the IMSE function. Probably marginally faster but kind of hard to wrap my head around. 
  # update: I tried this an it exceeded R's vector length limit. Might be a workaround, unsure. 
  
  # define squared phi_2 function for part d
  phi_2_sq <- function(x , mean, v){
    
    phi_2(x =x , mean = mean, v = v)^2
  }
  
  
  # now set up function to run simulations, make sure to pass in user defined functons/vars or foreach can freak out
  sim_function <- function(i, n, f_x, phi_2, h_v){
    
    # generate data 
    # start data.table for random data, take a random draw for weighted normals 
    r_dt <- data.table( r1 = sample(1:2,prob=c(.5,.5),size=n,replace=T) )
    
    # draw a random number from appropriate normal dist according to r1
    r_dt[r1 == 1, rdraw := rnorm(.N,-1.5,1.5)]
    r_dt[r1 == 2, rdraw := rnorm(.N,1,1)]
    r_dt[, r1 := NULL]
    
    
    # get IMSE 
    results_i <- imse_f(in_data = r_dt, x_v = "rdraw" ,f_x = f_x ,h_v =h_v)
    results_i[, sim := i]
    
    # now get mean and SE or part d 
    mean_i <- r_dt[, mean(rdraw)]
    var_i <- r_dt[, var(rdraw)]
    
    # calculate "optimal bandwidth" under the procdure from part D 
    vok <- 3/5
    u2k2 <- (1/5)^2
    
    # and the integral is 
    v2phi <- integrate(phi_2_sq, mean = mean_i, v = var_i, lower = -Inf, upper = Inf)$val
  
    # now calculate h optimal
    h_opt <- (vok/ (u2k2 *v2phi*n))^(1/5)
    
    # put that bad boy in the table 
    results_i[, d_h_hat := h_opt]
    
    # return the rsults for all of q2
    return(results_i)
    
  }
  

  
  # lets time this sucker
  start_t <- Sys.time()
  

  # parallel setup
  cl <- makeCluster(4, type = "PSOCK")
  registerDoParallel(cl)
  
  # run simulations in parallel
  output_list <- foreach(m = 1 : M,
                         .inorder = FALSE,
                         .packages = "data.table",
                         .options.multicore = list(preschedule = FALSE, cleanup = 9)) %dopar% sim_function(i = m, n = n, h_v = h_v, f_x = f_x, phi_2 = phi_2)
  
  # stop clusters 
  stopCluster(cl)
  
  # bind list
  output_dt <- rbindlist(output_list)
  
  # now take the mean of imse 
  imse_h <- output_dt[, list(imse_li = mean(imse_li), imse_lo = mean(imse_lo), d_h_hat = mean(d_h_hat)) ]
  
  # add in h 
  imse_h[, h := h]
  
  h_list[[(h*10-4)]] <- imse_h
  
  # AND TIME 
  run_time <- Sys.time() - start_t
  
  # bind results 
  part_b_res <- rbindlist(h_list)
  
  # make them pretty 
  part_b_res_pretty <- signif(part_b_res, 3)
  part_b_res_pretty[, colnames(part_b_res_pretty)] <- lapply(part_b_res_pretty[,colnames(part_b_res_pretty), with = FALSE],  as.character)
  
  
#====================#
# ==== save data ====
#====================#

  # only save data if this isn't a test run 
  if(!opt_test_run){
    # save IMSE by h results 
    print(xtable(part_b_res_pretty, type = "latex", 
                 digits = 3), 
          file = "C:/Users/Nmath_000/Documents/Code/courses/econ 675/PS_2_tex/Q1_p3_b.tex",
          include.rownames = FALSE,
          floating = FALSE)
    
    
  }
  

  
  
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
  
  
  
  #=============================#
  # ==== Sanity check plots ====
  #=============================#

  
  # plot function
  plot <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
  plot <- plot + stat_function(fun = phi_2, 
                                           args = list(mean = mean_i, v = var_i), 
                                           color = "blue") 
  plot <- plot + xlim(-10,10)
  plot
  
  
  
  plot <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
  plot <- plot + stat_function(fun = f_int, 
                               color = "blue") 
  plot <- plot + xlim(-10,10)
  plot
  
  
  
  
  
  plot <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
  plot <- plot + stat_function(fun = phi_2, 
                               args = list(mean = -1.5, v = 1.5), 
                               color = "blue") 
  plot <- plot + xlim(-10,10)
  plot
  
  
  
  
  
  f_int
  power_plot <- power_plot + xlim(-5000,5000) + xlab("tau") + ylab("Power")  + plot_attributes
  power_plot
  