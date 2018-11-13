#====================#
# ==== ps_5_675R ====
#====================#


#=========================================#
# ==== Load packages, clear workspace ====
#=========================================#


library(MASS)            
library(data.table)       
library(broom)
library(AER)
library(xtable)

rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

#========================#
# ==== Q2 simulation ====
#========================#

# set final run parm for n simulations and if it should save 
final_run <- TRUE

  
  #=============================#
  # ==== Write sim function ====
  #=============================#
    
    # parms for funciton 
    f_stat = 0
    n = 200
    sim = 1 # a tag for the simulation number 
    
    # sim function 
    sim_fun2 <- function(sim, f_stat, n = 200){
      
      # make gamma
      gamma <- sqrt(f_stat/n)
      
     # make mu vector 
      mu = c(0,0,0)
      
      # make sigma matrix 
      sigma <- matrix(c(1,0,0,0,1,.99,0,1,.99), 3,3)
      
      # make data 
      rdt <- mvrnorm(n, mu, sigma)
      
      # make it a data.table 
      rdt <- data.table(rdt)
      setnames(rdt, colnames(rdt), c("z","u","v"))
      
      # back out x
      rdt[, x := gamma*z + v]
      
      # back out y given b=0
      rdt[, y := u]
      
      # run ols
      ols_res <- data.table(tidy(lm(y~x, data = rdt)))
      
      # make column of rejecting the null 
      ols_res[, rej := as.numeric(abs(statistic) > 1.96)]
      
      # take what we need 
      ols_res <- ols_res[term == "x", c("estimate", "std.error", "rej")]
      
      # add on ols suffix
      ols_res[, reg_type := "ols"]
      
      # melt data for matias table 
      ols_res <- melt.data.table(id.vars = "reg_type", data = ols_res)
      
      # run first stage of 2sls to get f test 
      fst_stg <- lm(x~z, data = rdt)
      f_stat <- summary(fst_stg)$fstatistic[1]
    
      # now run 2sls 
      iv_reg <- data.table(tidy(ivreg(y ~ x | z , data = rdt)))
      
      # compute rej 
      iv_reg[, rej := as.numeric(abs(statistic) > 1.96)]
      
      # take what we need 
      iv_reg <- iv_reg[term == "x", c("estimate", "std.error", "rej")]
      
      # throw in the f stat 
      iv_reg[, f_stat := f_stat]
      
      # add 2sls indicator 
      iv_reg[, reg_type := "2sls"]
      
      # melt data for matias table 
      iv_reg <- melt.data.table(id.vars = "reg_type", data = iv_reg)
      
      
      # stack  these tables 
      out_dt <- rbind(ols_res, iv_reg)
      
      # add sim number 
      out_dt[, sim := sim]
      
      # ruturn that shiz 
      return(out_dt[])
      
      }# end sim funciton 
    
    


  #===========================#
  # ==== run sim funciton ====
  #===========================#

    # time this sucker 
    start_time <- Sys.time()
    
    # initialize list to store output 
    sim_list <- list()
    for(f_stat_i in c(0,.25,9,99)){
      
      # get number of sims 
      n_sims <- ifelse(final_run, 5000, 50)
      
      # apply the function 5000 times 
      sim_out <- lapply(c(1:n_sims), sim_fun2, f_stat = f_stat_i, n = 200)
      
      # bind the results 
      sim_out <- rbindlist(sim_out)
      
      # take mean, std, quantiles by group 
      results <- sim_out[, list("mean" = mean(value),
                                "st.dev" = sd(value),
                                "quant .1" = quantile(value, .1),
                                "quant .5" = quantile(value, .5),
                                "quant .9" = quantile(value, .9)), by = c("reg_type", "variable")]
      
      # store results in a list 
      sim_list[[paste0(f_stat_i)]] <- results
      
    }# end loop over gamams 
    

    # check time 
    end_time <- Sys.time()
    
    # print time 
    print(paste0(round(as.numeric(end_time - start_time, units = "mins"), 3), " minutes to run"))
  
  #================================================#
  # ==== save out these tables into a tex file ====
  #================================================#
  
    # check if this is a final run 
    if(final_run){
    
      # for each item in the list 
      for(tab_i in ls(sim_list)){
        
        print(xtable(sim_list[[tab_i]], type = "latex"), 
              file = paste0("C://Users/Nmath_000/Documents/Code/courses/econ 675/PS_5_tex/q2tab_fstat_",tab_i, ".tex"),
              include.rownames = FALSE,
              floating = FALSE)
        }#end loop

    } # end if statement 
    













