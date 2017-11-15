#========================#
# ==== load packages ====
#========================#

  library(data.table)

#==============================================#
# ==== check the unbiasedness of estimator ====
#==============================================#

  # set n obs and theta and n iterations 
  n <- 25
  theta <- .8
  n_iter <- 1000000
  
  # initialize list to store values 
  theta_hat_lst <- vector("list", length = n_iter)
  
  # start loop
  for(i in 1:n_iter){
  
    # create univorm distribution 
    ud <- runif(n, 0,1)
    
    # use inverse transform techniqe to generate a random distribution from 1(a)'s dist 
    sample <- ud^(1/theta)
    
    # take the mean
    smean <- mean(sample)
    
    # estimate theta 
    theta_hat <- smean/(1-smean)
    
    # save theta hat in list
    theta_hat_lst[[i]] <- theta_hat
  
  }
  
  # put into a data.table
  theta_hat_dt <- data.table( theta_hats = as.numeric(theta_hat_lst))
  
  # take mean 
  theta_hat_dt[, mean(theta_hats)]
