#========================#
# ==== load packages ====
#========================#

  library(data.table)

#==============================================#
# ==== check the unbiasedness of estimator ====
#==============================================#

  # set n obs and theta and n iterations 
  n <- 25
  theta <- .999999
  n_iter <- 100000
  
  # initialize list to store values 
  theta_hat_lst <- vector("list", length = n_iter)
  theta_hat_mle_lst <- vector("list", length = n_iter)
  
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
    
    #==================================#
    # ==== now do 4a mle estimator ====
    #==================================#

    log_sum <- sum(log(sample))
    theta_hat_mle <- -n/(log_sum)
    theta_hat_mle_lst[[i]] <- theta_hat_mle
  
  }
  
  # put into a data.table
  theta_hat_dt <- data.table( theta_hats = as.numeric(theta_hat_lst))
  theta_hat_mle_dt <- data.table( theta_hats_mle = as.numeric(theta_hat_mle_lst))
  
  # take mean for 1 a
  theta_hat_dt[, mean(theta_hats)]
  
  # take mean for 4a
  theta_hat_mle_dt[, mean(theta_hats_mle)]
  
  
  
