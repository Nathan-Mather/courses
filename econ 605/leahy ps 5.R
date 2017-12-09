#===========================#
# ==== Problem 1 pset 5 ====
#===========================#

library(easimple)
library(data.table)
library(Matrix)
library(DTMCPack)
library(ggplot2)

# ea_start()
#=============================#
# ==== set function parms ====
#=============================#


# define them o work with for now
gam <- 1.5
B     <- .98
min_a = -2
max_a = 8
n     = 50
min_i = .05
max_i = 1
m     = 2
np_min = -1
np_max = 1
# guess an interest rate, start with range of 0 and something huge, 10000
# but guess something reasonably large so that we hit the upper bound
rl <- -.5
rh <- .5
rg <- .021
P_mat      =    matrix(c(.8,.5,.2,.5), 2,2)





hugget_model <- function(gam  = NULL,
                         B      = NULL,
                         min_a  = NULL,
                         max_a  = NULL,
                         n      = NULL,
                         min_i  = NULL,
                         max_i  = NULL,
                         m      = NULL,
                         np_min = -1,
                         np_max = 1,
                         rl = -.5,
                         rh = .5,
                         rg  = .021,
                         P_mat = NULL){
  
  start_time <- Sys.time()
  #=========================#
  # ==== create objects ====
  #=========================#

    # create asset vecor
    A <- data.table(a = seq(min_a, max_a, length.out = n))
  
    # create potential income vector (states)
    S <- data.table( s = seq(min_i,max_i,length.out = m))
  
    if(!is.null(P_mat)){
      
      P <- P_mat
    }else{
    # inialize a P data.table
    P <- data.table(id_col = seq.int(1,nrow(S)))

    # create a transition matrix, get a normal distribution centered around i for each entry
    for( i in 1:nrow(S)){

      # createa vectory of values for normal(0,1) distribtion
      val <- data.table( v= seq(np_min, np_max, length.out = nrow(S)))

      # find the difference of the ith entry from 0
      normalize_val <-  as.numeric(val[i,1,with = FALSE])

      # normalize I to be the 0
      val[, v := v - normalize_val]

      # now fill in the probabilities of these values from a normal distribution
      val[, prob := dnorm(v)]

      # normalize that to 1
      val[, prob := prob/(sum(prob))]

      # add it to the full data.table
      P[, paste0("v_", i) := val[, prob]]
    }

    # now get rid of the Id col and make P a matrix
    P[, id_col := NULL]
    P <- as.matrix(P)

    # transpose it because thats how transition matrices work dumn ass #late_fix
    P <- t(P)
  }
  #==============================================#
  # ==== start iterating over interest rates ====
  #==============================================#

    # create ulility function, add 10000 to CRRA utility to ensure that we dont try to square root a negative  
    U <- function(x) ((x)^(1-gam)-1)/(1-gam)
    

    # now itereate and continually update our guesses for R 
    run_flag_1 <- TRUE
    
    # create data.table for G  and V function 
    A[, id := 1]
    S[, id := 1]
    V <- merge(A, S, by = "id", allow.cartesian = TRUE)
    V[, id := NULL]
    
    # convert A back into a numeric vector 
    A <- as.numeric(A[, a])
    S <- as.numeric(S[, s])
    
    # create identical table for Vlaue function V and put in best guess 
    V[, b_guess := 0]
    V[, G := 0]
    
    # this is a loop over our interest rate guesses 
    n_iter_1 <- 0
    while(run_flag_1){
      n_iter_1 <- n_iter_1 +1
      
      #===============================#
      # ==== do bellman iteration ====
      #===============================#

      # now iterate over our bellman to solve for the policy functon G given our interest rate guess 
      run_flag_2 <- TRUE
      n_iter_2 <- 0
      while(run_flag_2){
        
        n_iter_2 <- n_iter_2 +1
        
        # now with our current best guess for V and r we need to calculate the values of a' for a given a,s combo
        # so we loop over the choices of tommorows a, A' and calculate the value function under the given choice
        for(i in 1:length(A)){
          
          # grab relavent best guess for continuation values 
          V_i <- as.matrix(V[a == A[i], b_guess ])
          
          # compute expected continuation values for each s given choice A'
          e_cont_value <- data.table( P %*% V_i)
          setnames(e_cont_value, "V1", "cont_value_i")
          e_cont_value[, s := S]
          
          # merge on the continuation value to V
          V <- merge(V, e_cont_value, "s")
          
          V[, paste0("a_", i) := U((1+rg)*a+s- A[i]) + B* cont_value_i]
          
          # set negative consumtion values to negative utility 
          V[((1+rg)*a+s- A[i]) <=0, paste0("a_", i) := -10000]

          # remove the continuation value 
          V[, cont_value_i := NULL]
          
        }
        
      
        # #  need to actually get what the argmax values are because we need a policy function 
        # V[, G := max.col(V[, -c("a", "s", "b_guess", "G")])]
        # 
        # # fill it in with actual choice 
        # V[, G := A[G]]
         
        # now we select the max to get update to our best gess of the value function 
        V$b_guess_new <- apply(V[, -c("a", "s", "b_guess", "G")], 1, max)
        
        V[, G := as.character(G)]
        
        # need to actually get what the argmax values are because we need a policy function, dont use the busted ass
        # max.col function because for whatever reason max.col 
        for(col in colnames(V[, -c("a", "s", "b_guess", "G", "b_guess_new")])){
          
          V[get(col) == b_guess_new, G := paste(col)]
          
        }
        
        V[, G := ea_scan(G, 2, "_")]

        # check if the best guess has converged, check that the distance between guess and true value is small
        error <- V[, max(abs((b_guess_new - b_guess)/b_guess)) ]
        
        # strip V back down to its needed compnents 
        V[, b_guess := b_guess_new]
        V <- V[, c("s", "a", "b_guess", "G")]
        
        # if error is small, stop, but also make sure it runs at least 10 becuase less is sketch 
        if(abs(error) < .0001 & n_iter_2 >= 10 ){run_flag_2 <- FALSE}
      }
      print(paste0(n_iter_2, " iterations to converge"))
      
      # convert G to numeric 
      V[, G := as.numeric(G)]
      V[, G := A[G]]
      
      #==============================#
      # ==== find ergotic lambda ====
      #==============================#

        # create lambda matrix 
      lam_1 <- V[, c("s", "a", "G")]
      lam_1[, id := 1]
      lam_2 <- lam_1[, c("s", "a", "id")]
      setnames(lam_2, c("s", "a"), c("s_prime", "a_prime"))
      
  
      lam <- merge(lam_1, lam_2, "id", allow.cartesian = TRUE)
      
      # melt p to get long a to a transition prob 
      P_l <- data.table(P)
      setnames(P_l, colnames(P_l), as.character(S))
      P_l[, s := S]
      P_l <- melt.data.table(P_l, id.vars = "s", variable.name = "s_prime", value.name = "trans_prob", variable.factor = FALSE)
      P_l[, s_prime := as.numeric(s_prime)]
      
      
      # convert to chaaracter so the moerge works properly 
      lam[, colnames(lam)] <- lapply(lam[,colnames(lam), with = FALSE],  as.character)
      P_l[, colnames(P_l)] <- lapply(P_l[,colnames(P_l), with = FALSE],  as.character)
      
      
      # merge on trans probs 
      lam<- merge(lam, P_l, c("s", "s_prime"))
    
      
      # convert bakc to numeric 
      lam[, colnames(lam)] <- lapply(lam[,colnames(lam), with = FALSE],  as.numeric)
      
      
      # fill in s to s trasition probabiliies 
      lam[G!=a_prime, trans_prob := 0]
      
      # cast s prim wide 
      lam_m <- dcast.data.table(lam, s + a  ~paste0("s'_", s_prime, "_a'_", a_prime), value.var = "trans_prob")
      
      # create actual matrix 
      lam_m <- as.matrix(lam_m[, -c("s", "a")])
      
      lambda <- copy(lam_m)
      for(i in 1:100){
        
        lambda <- lambda%*%lam_m
        
      }
      
      lambda_v <- matrix(lambda[1,])
      
      
      # find if there is excess demand or supply in the loan market 
      excess_demand <- t(V[, G]) %*% lambda_v
      
      print(paste0(excess_demand, " excess demand"))
      
      if(excess_demand <0){
        
        rl <- rg
      }
      
      if(excess_demand > 0 ){
        
        rh <- rg
      }
      
      if(abs(excess_demand) < .1){
        
        run_flag_1 <- FALSE
      }else{
        
        # update our guess 
        rg <- (rh+rl)/2
      }
      
      # print r guess for sanity 
      print(paste0(rg, " interest rate"))
      
    }
      
# return interest rate 
    output_list <- list()
    output_list[["interest_rate"]] <- rg
    
    V[, wealth := (1+rg)*a+s- G]
    V[, income_dist := lambda_v]
    
    
    output_list[["wealth_dist"]] <-  V
    
    
    time_to_run <- as.numeric(Sys.time() - start_time, units = "mins")
    
    output_list[["run_time"]] <- time_to_run
    output_list[["total_iterations"]] <- n_iter_1
    return(output_list)
}


#example from aarons post 
results <- hugget_model(gam         =1.5,
                         B          = .98,
                         min_a      = -2,
                         max_a      = 8,
                         n          = 50,
                         min_i      = .05,
                         max_i      = 1,
                         m          = 2,
                         np_min     = -1,
                         np_max     = 1,
                         rl         = -.5,
                         rh         = .5,
                         rg         =.021,
                         P_mat      =    matrix(c(.8,.5,.2,.5), 2,2))


# example using probability generator so I can check a mean preserving spread 
results_2 <- hugget_model(gam         =1.5,
                        B          = .98,
                        min_a      = -2,
                        max_a      = 8,
                        n          = 50,
                        min_i      = .05,
                        max_i      = 1,
                        m          = 5,
                        np_min     = -1,
                        np_max     = 1,
                        rl         = -.5,
                        rh         = .5,
                        rg         =.021,
                        P_mat      =   NULL)

# increase wealth mobility. (the surfs no longer stay in their feifs)
results_3 <- hugget_model(gam         =1.5,
                          B          = .98,
                          min_a      = -2,
                          max_a      = 8,
                          n          = 50,
                          min_i      = .05,
                          max_i      = 1,
                          m          = 5,
                          np_min     = -.5,
                          np_max     = .5,
                          rl         = -.5,
                          rh         = .5,
                          rg         =.021,
                          P_mat      =   NULL)

# increase wealth mobility more. (random communism)
results_4 <- hugget_model(gam         =1.5,
                          B          = .98,
                          min_a      = -2,
                          max_a      = 8,
                          n          = 50,
                          min_i      = .05,
                          max_i      = 1,
                          m          = 5,
                          np_min     = -.25,
                          np_max     = .25,
                          rl         = -.5,
                          rh         = .5,
                          rg         =.021,
                          P_mat      =   NULL)


# increase gamma with #2 parms 
results_5 <- hugget_model(gam         =6,
                          B          = .98,
                          min_a      = -2,
                          max_a      = 8,
                          n          = 50,
                          min_i      = .05,
                          max_i      = 1,
                          m          = 5,
                          np_min     = -1,
                          np_max     = 1,
                          rl         = -.5,
                          rh         = .5,
                          rg         =.021,
                          P_mat      =   NULL)


# decrease beta with #2 parms 
results_5 <- hugget_model(gam         =1.5,
                          B          = .7,
                          min_a      = -2,
                          max_a      = 8,
                          n          = 50,
                          min_i      = .05,
                          max_i      = 1,
                          m          = 5,
                          np_min     = -1,
                          np_max     = 1,
                          rl         = -.5,
                          rh         = .5,
                          rg         =.021,
                          P_mat      =   NULL)






#=======================#
# ==== plot results ====
#=======================#
# # set plot attributes
# 
# plot_1 <- ggplot(data = results$wealth_dist, aes(x = wealth, y = income_dist)) + geom_point(size = 1) 
# plot_2 <- ggplot(data = results_2$wealth_dist, aes(x = wealth, y = income_dist)) + geom_point(size = 1) 
# plot_3 <- ggplot(data = results_3$wealth_dist, aes(x = wealth, y = income_dist)) + geom_point(size = 1) 
# plot_4 <- ggplot(data = results_4$wealth_dist, aes(x = wealth, y = income_dist)) + geom_point(size = 1) 
# 

bar_1 <- ggplot(data = results$wealth_dist, aes(x = wealth, y = income_dist)) + geom_col(width = .001, alpha = .5) + ylim(c(0,.05))
bar_2 <- ggplot(data = results_2$wealth_dist, aes(x = wealth, y = income_dist)) + geom_col(width = .005, alpha = .5) +  ylim(c(0,.05))
bar_3 <- ggplot(data = results_3$wealth_dist, aes(x = wealth, y = income_dist)) + geom_col(width = .005, alpha = .5) +   ylim(c(0,.05))
bar_4 <- ggplot(data = results_4$wealth_dist, aes(x = wealth, y = income_dist)) + geom_col(width = .005,alpha = .5) + ylim(c(0,.05))

bar_1
bar_2
bar_3
bar_4


# plot_1
# plot_2
# plot_3 
# plot_4
#=======================#
# ==== save results ====
#=======================#


save(results,  file = "C:/Users/Nmath_000/Documents/MI_school/macro 605/John_Leahy_stuff/B. Problem sets/pset_5_data/results_1.rdata")
save(results_2,  file = "C:/Users/Nmath_000/Documents/MI_school/macro 605/John_Leahy_stuff/B. Problem sets/pset_5_data/results_2.rdata")
save(results_3,  file = "C:/Users/Nmath_000/Documents/MI_school/macro 605/John_Leahy_stuff/B. Problem sets/pset_5_data/results_3.rdata")
save(results_4,  file = "C:/Users/Nmath_000/Documents/MI_school/macro 605/John_Leahy_stuff/B. Problem sets/pset_5_data/results_4.rdata")


for(n in 1:4){
ggsave(paste0("C:/Users/Nmath_000/Documents/MI_school/macro 605/John_Leahy_stuff/B. Problem sets/graphs/pset_5_plot_", n, ".png"), plot = get(paste0("bar_", n)), device = "png", height = 20, width = 20)
}

