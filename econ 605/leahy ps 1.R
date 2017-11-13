library(data.table)
library(Matrix)
library(MASS)
library(easimple)
library(ggplot2)
ea_start()
#==================================#
# ==== check work on problem 1 ====
#==================================#

# A 
p <- matrix(c(.9,.25,.1,.75), 2 , 2)
p

stat <- p
for( n in 1:10000){
  
  stat <- p%*%stat
}
stat

pi <- c(5/7, 2/7)

# B 
fractions((pi %*% c(1.2, -0.4)))


# find the eight step transition matrix
mc_8 <- p

for(i in 1:7){
  mc_8 <- mc_8 %*% p
  
  
}
round(mc_8,2)

#================================#
# ==== problem 3 programming ====
#================================#
  
  # initialize number of observations 
  n <- 1000
  
  # initialize other variables 
k <- 5
  max_w <- 200
  min_w <- 10
  B <- .8
  c <- 8
  df <- 5
  
  # create vector of wages within a working data.table  
  dt <- data.table(wages = seq(from = min_w, to = max_w, by = (max_w - min_w)/n))
  
  # create probability distribution over the wages using chi squared bec
  # start by creating a spred of even values between 0 and something with prob close to 1
  x <- qchisq(.99, df = df)
  dt[, cut_p := seq(0,x,x/n)]

  # create cumulative distribution table of the above even cuts using chi squared 
  dt[, cum_p := pchisq(cut_p, df = df)]
  
  # change highest wage to probability 1 (since wages cant be infinite)
  dt[cum_p == max(cum_p), cum_p := 1]
  
  # break it into a discreate pdf by taking difference 
  dt[, pdf := cum_p - shift(cum_p, 1, type = "lag")]
  
  # remove  0, it has prob 0 and gives us 1 too many observations
  dt <- dt[cum_p != 0,]
  dt[, cut_p := NULL]
  
  # check that probability does sum to 1
  if (dt[, sum(pdf)] != 1){ warning("your probabilities are fucked")}
  
  #=========================#
  # ==== create bellman ====
  #=========================#

    # create a best guess at value 
    dt[, og_guess := wages/(1-B)]
  
    # psych, do a terrible guess to see if it still works 
    # dt[, og_guess := 1000]

    # find the expected value of our best guess at the value function 
    exp_bg <- dt[, sum(og_guess*pdf)]
    
    # create value function per wage matrix to start 
    value_matrix <- dt[, c("wages", "og_guess")]
    setnames(value_matrix, "og_guess", "value_0")
    
    # create list for cut wages and the expected value of V
    cut_w_lst <- list()
    exp_v_lst <- list()
    
    # set convergence flag 
    f_convrged_abs <- 0 
    f_convrged_perc <- 0 
    
    n_iter <- 100
    for(i in 1:n_iter){
      
      # find the cutoff wage. where w/1-B = c + B(E(best_guess)) or where
      cut_w <- (1-B)*(c + B*exp_bg)
      
      # now we find the expecation of the value function. i.e. the expectation of the max of taking 
      # the offered wage or continueing on. Using out best gess of the vlaue of continueing .
      
      # first, for all wages below the cutoff, we recieve the value of continued search 
      dt[wages < cut_w, new_g_val := (c + B*exp_bg)]
      
      # if you are above the cutoff, you take the wage and recieve w/1-B
      dt[wages >= cut_w, new_g_val := wages/(1-B)]
      
      # fill in large value estimate matrix 
      value_matrix[, paste0("value_", i) := dt$new_g_val]
      
  
      # now find the expected payoff of my new guess and replace my old guess 
      exp_bg <- dt[, sum(new_g_val*pdf)]
      
      # add cut wage to the list and expected value to list 
      cut_w_lst[[i]] <- cut_w
      exp_v_lst[[i]] <- exp_bg
      
      # flag the number of iterations until the expected value changes by less than .1
      if(i != 1){
        if( abs(cut_w_lst[[i]] -  cut_w_lst[[i-1]]) < .1 & f_convrged_abs == 0){
      
          f_convrged_abs <- 1
          iterations_needed_abs <- i
        }
        
        if( abs((cut_w_lst[[i]] -  cut_w_lst[[i-1]])/ cut_w_lst[[i]] ) < .001 & f_convrged_perc == 0){
          
          f_convrged_perc <- 1
          iterations_needed_perc <- i
        }
        
      }
    }


# bind cut wage list
cut_w_dt <- data.table(cut_w_lst)
exp_v_dt <- data.table(exp_v_lst)

# cut_w_dt
# exp_v_dt


    
    
    
#=================#
# ==== graphs ====
#=================#

# call plot attributes
plot_attributes <- theme(plot.background    = element_rect(fill  = "white"),
                         panel.grid.major.x = element_line(color = "gray90"),
                         panel.grid.minor   = element_blank(),
                         panel.background   = element_rect(fill  = "white", colour = "black") ,
                         panel.grid.major.y = element_line(color = "gray90"),
                         text               = element_text(size  = 14),
                         plot.title         = element_text(vjust = 0, colour = "#0B6357",
                                                           face = "bold", size = 12, hjust = 0.5), 
                         legend.position = "none")

    
#========================#
# ==== correct graph ====
#========================#
    value_matrix_long <- melt.data.table(value_matrix, id.vars = "wages")
    
    #  loop over wages and plot the values over time
      plot_1_aes <- ggplot(data = value_matrix_long, aes(x = wages, y = value, siz = 10, color = variable)) +
        geom_line() + plot_attributes + labs(x = "Wages",
                                            y = "Value",
                                            title = paste( " B = ", B, ",  c = ", c, ",  deg of freedom = ", df,
                                                            ",  iterations to convgerge = ", iterations_needed_perc))
                                              
                                        
      
  
    
    
    print(plot_1_aes)
    ggsave(paste0("C:/Users/Nmath_000/Documents/MI_school/macro 605/John_Leahy_stuff/B. Problem sets/graphs/graph_", k), plot = plot_1_aes, device = "png", height = 6, width = 9)

  
# #====================#
# # ==== my graphs ====
# #====================#
# 
#     # fix the matrix becasuse lahey's matrix is no good for graphing
#     value_matrix2 <- melt.data.table(value_matrix, id.vars = "wages", variable.name = "iteration", variable.factor = FALSE)
#     value_matrix2 <- dcast.data.table(value_matrix2, iteration ~ wages, value.var = "value")
# 
#     # fix colnames
#     wage_vars <- grep("[[:digit:]]", colnames(value_matrix2), value = TRUE)
#     setnames(value_matrix2, wage_vars, paste0("wage_", wage_vars))
# 
#     # fix iteration variable
#     value_matrix2[, iteration := as.numeric(ea_scan(iteration, 2, "_"))]
# 
#     #  loop over wages and plot the values over time
#     for(wage_col in colnames(value_matrix2)[-1]){
#       plot_1_aes <- ggplot(data = value_matrix2, aes(x = iteration, y = get(wage_col), siz = 10)) +
#         geom_point() + plot_attributes + labs(x = "Iteration",
#                                               y = paste0(gsub("_"," ", wage_col), " value"),
#                                               title = "Value Estimate Per Iteration")
#         print(plot_1_aes)
# 
#   }
# 









    print(paste0(iterations_needed_abs, " iterations needed to converge abs"))
    print(paste0(iterations_needed_perc, " iterations needed to converge in percentage "))
    
















