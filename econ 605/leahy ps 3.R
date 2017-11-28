
#========================#
# ==== load packages ====
#========================#

  # library packages 
  library(easimple)
  library(data.table)
  library(Matrix)
  library(DTMCPack)
  
#=================================#
# ==== macro ps 3 problem 1  ====
#=================================#

#====================================#
# ==== write function to do this ====
#====================================#

  # function to do this stuff 
  dope_fun <- function(gamma_v = NULL, B_v = NULL, P = NULL, lam_base = NULL, pi = NULL, opt_return_vectors = FALSE){
    
    # initalize list 
    output_list <-list()
    vector_list <- list()

    # loop over betas 
    for(B_i in B_v){
      # loop over gammas 
      for(gamma in gamma_v){
        
        # create a vector of lambda to the -gamma for rfr 
        lam_v <- matrix(lam_base^(-gamma))
      
      # find the conditional risk free rate 
      rfr_v <- (B_i * P%*%lam_v)^(-1)
      
      # find the unconditional or average risk free rate 
      rfr <- as.numeric(pi %*% rfr_v)
      
      # raise lamda values to the 1-gamma and put in matrix
      lam <- diag(lam_base^(1-gamma))
      
      # create iota vector of ones 
      iota <- matrix(rep(1, nrow(lam)), nrow(lam), 1)
      
      # calculate w 
      w <- B_i*solve((diag(nrow(lam)) - B_i*P%*%lam))%*%(P%*%lam%*%iota)
      
      # calculate ers 
      ers <- (P%*%(matrix(lam_base)*(w+1)))/w
      
      # calculate average ers 
      a_ers <- as.numeric(pi%*%ers)
      
      # creat data.table of values 
      values <- data.table( risk_free_rate = rfr, equity_return = a_ers, gamma = gamma, B =B_i)
      
      # store answers in list 
      output_list[[paste0(gamma, B_i)]] <- values
      
      # store ectors in list for part E
      vector_list[[paste0(gamma, "_", B_i)]] <- data.table(rfr = rfr_v, ers = ers, gamma = gamma, B =B_i, state = c(1:nrow(lam)))

    }
    }
    
    # stack gammas 
    output_dt <- rbindlist(output_list)
    
    # do equity premium
    output_dt[, equity_premium := equity_return - risk_free_rate]
    
    # stack vectors 
    vectors_dt <- rbindlist(vector_list)
    
    # return stuff for part E
    if(opt_return_vectors == TRUE){
      
      outputlist <- list(dt = output_dt, vectors = vectors_dt)
      return(outputlist)
    }
    
    # otherwise return the basic shiz 
    return(output_dt[])
  }


  #======================================#
  # ==== define variables for part B ====
  #======================================#
  
    B <- .99
    u <- .018
    d <- .036
    phi <- .43
    gamma_v <- c(2,4,6,10)
    
    # create individual lambdas 
    lam_1 <- 1 + u + d
    lam_2 <- 1 + u - d
    
    # create lambda vector 
    lam_base <- c(lam_1,lam_2)
 
    # create P matrix 
    P <- matrix(c(phi, 1-phi,1-phi,phi), 2,2)

    # find stationary dist 
    pi <- statdistr(P)
     

    # run function 
    output_dt_B <- dope_fun(gamma_v, B, P, lam_base, pi)
     
  #======================================#
  # ==== define variables for part C ====
  #======================================#
     
     B <- c(.99,.97)
     p <- .987
     q <- .516
     P <- matrix(c(p, 1-q,1-p,q), 2,2)

     gamma_v <- c(2,4,6,10)
     
     # create individual lambdas 
     lam_1 <- 1 + .02252
     lam_2 <- 1 - .06785
     
     # create lambda vector 
     lam_base <- c(lam_1,lam_2)
     
     # find stationary dist 
     pi <- statdistr(P)

     # run through function 
     output_dt_C <- dope_fun(gamma_v, B, P, lam_base, pi)
     
     
  #======================================#
  # ==== define variables for part D ====
  #======================================#
     
     B <- c(.99,.97)
     p <- .6
     q <- .9
     P <- matrix(c(p, 1-q,1-p,q), 2,2)
 
     
     # note tha twe keep the same TRUE pi from above, and lamda is the same 

     # run function 
     output_dt_D <- dope_fun(gamma_v, B, P, lam_base, pi)
     
    
  #======================================#
  # ==== define variables for part E ====
  #======================================#
     B <- c(.99)
     p <- .43

     # create transition matrix 
     r <- .017
     P <- matrix(c(p-(r/2), 1-p-(r/2), .5, 1-p-(r/2), p-(r/2), .5, r, r, 0), 3, 3)
     
     # find stationary dist 
     pi <- statdistr(P)
     
     gamma_v <- c(2,4,6,10)
     
     # create individual lambdas 
     u <- .018
     d <- .036
     lam_1 <- 1 + u + d
     lam_2 <- 1 + u - d
     lam_3 <- 1 -.3
     
     # create lambda vector 
     lam_base <- c(lam_1,lam_2, lam_3)
     
     output_dt_E <- dope_fun(gamma_v, B, P, lam_base, pi, opt_return_vectors = TRUE)
     
     E_dt <- output_dt_E$dt
     
     e_rfr <- output_dt_E$vectors
  
  
  #=================#
  # ==== part F ====
  #=================#
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     

    

     
     
     
     