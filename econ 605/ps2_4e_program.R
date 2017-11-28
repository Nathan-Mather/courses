library(data.table)
library(Matrix)
library(MASS)
library(easimple)
library(ggplot2)
ea_start()


#====================================#
# ==== attempt at 4e ps2 program ====
#====================================#

# initialize number of observations 
n <- 10

# initialize other variables 
max_p <- 10
min_p <- 0
true_p <- 8
B <- .9


# # create value function per wage matrix to start 
# value_matrix <- dt[, c("wages", "og_guess")]
# setnames(value_matrix, "og_guess", "value_0")

# create list for cut wages and the expected value of V
cut_w_lst <- list()
exp_v_lst <- list()

# set convergence flag 
f_convrged_abs <- 0 
f_convrged_perc <- 0 


#====================================================#
# ==== create matrix of value function estimates ====
#====================================================#

pl_v <- data.table(pl = seq(from = min_p, to = max_p, by = (max_p - min_p)/n), id = 1)
pu_v <- data.table(pu = seq(from = min_p, to = max_p, by = (max_p - min_p)/n), id = 1)

vf_est_m <- merge(pl_v, pu_v, by = "id", allow.cartesian=TRUE)
vf_est_m <- vf_est_m[pu > pl & pl != 0, -"id", with = FALSE]

vf_est_m[, ev := 0]

# will iteratively update this to get better estimates 


#=============================#
# ==== create dist tables ====
#=============================#


n_iter <- 100
for(i in 1:n_iter){

  # create vector of prices within a working data.table  
  dt <- data.table(price = seq(from = min_p, to = max_p, by = (max_p - min_p)/n))
  
  # create probability distribution over the price using uniform dist
  
  # create cumulative distribution table of the above even cuts using chi squared 
  dt[, cum_p := (price - min_p)/(max_p - min_p)]
  
  # break it into a discreate pdf by taking difference 
  dt[, pdf := cum_p - shift(cum_p, 1, type = "lag")]
  
  # remove  0, it has prob 0 and gives us 1 too many observations
  dt <- dt[cum_p != 0,]
  
  # check that probability does sum to 1
  if (dt[, sum(pdf)] != 1){ warning("your probabilities are fucked")}
  
  
#=========================#
# ==== create bellman ====
#=========================#

# find the expected value of our best guess at the value function 
# first find the expected profit tomorrow "next day profit"
dt[, nd_prof := price*((max_p-price)/(max_p - min_p))]

# next find continuation value 
  # get v_est_1 
  cv_est_1_est <- vf_est_m[ pu == max_p ,]
  
  dt[, cv_est_1 :=  ]
  
dt[, cont_v := (vf_est_m[pl == price & pu == max_p , ev]*((max_p-price)/(max_p - min_p)) + 
                  vf_est_m[pl == min_p & pu == price , ev]*((price-min_p)/(max_p - min_p)))]

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





print(paste0(iterations_needed_abs, " iterations needed to converge abs"))
print(paste0(iterations_needed_perc, " iterations needed to converge in percentage "))

















