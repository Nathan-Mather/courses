#===============================================#
# ==== Inustrial ORganization PRoblem Set 1 ====
#===============================================#


# clear objects 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")


# Load packages 
library(data.table)
library(stats4)
library(broom)
library(AER)

# set save option 

opt_save <- TRUE 

#=====================#
# ==== Question 1 ====
#=====================#

# lad data 
q1dt <- fread("file:///C:/Users/Nmath_000/Documents/MI_school/Third Year/Econ 631/ps1/ps1.dat")


# create variable names 
setnames(q1dt, colnames(q1dt), c("y", "x1", "x2", "z"))


  #=================#
  # ==== part 1 ====
  #=================#
    
    # write log likelihood function 
    Probit_llf <- function(th0, th1, th2){
      
      
      mu <- q1dt[,  pnorm(th0 + th1*x1 + th2*x2)]
      
      -sum(q1dt[, y*log(mu) + (1-y)*log(1-mu)])
    }
  
    # create startign values 
    prob_start <- list(th0 = 0, 
                       th1 = .01,
                       th2 = .01)
    
    # run the mle function 
    probit_res <- mle(Probit_llf, start = prob_start)
    
    # get the results I need 
    probit_res <- data.table(variable = rownames(summary(probit_res)@coef),
                             summary(probit_res)@coef)
      
    # check my results 
    check <- glm( y ~ x1+ x2, 
                      family = binomial(link = "probit"), 
                      data = q1dt)
    
    # looks good 
    summary(check)
    probit_res
    
  #================#
  # ==== Part3 ====
  #================#
    
    # define logit log likelihood
    logit_llf <- function(th0, th1, th2){
      
      
      mu <- q1dt[,  plogis(th0 + th1*x1 + th2*x2)]
      
      -sum(q1dt[, y*log(mu) + (1-y)*log(1-mu)])
    }
    
    # create startign values 
    logit_start <- list(th0 = 0, 
                       th1 = .01,
                       th2 = .01)
    
    # run the mle function 
    logit_res <- mle(logit_llf, start = logit_start)
    
    # get the results I need 
    logit_res <- data.table(variable = rownames(summary(logit_res)@coef),
                            summary(logit_res)@coef)
    
    # check my results 
    check <- glm( y ~ x1+ x2, 
                       family = binomial(link = "logit"), 
                       data = q1dt)
    # looks good 
    summary(check)
    logit_res
    
  #=================#
  # ==== Part 6 ====
  #=================#
    
    
    ## test values 
    # th0 = 0
    # th1 = 0.01
    # th2 = 0.01
    # th3 = 0.01
    # th4 = 0.01
    # th5 = 0.01
    # p = .01
    # sig = 2
    
    # define a log likliehood function 
    # not craxy about altering the data.tble within the function but it's fine 
    # next time maybe create objects rather than columns. Althoguh that may be slower? unsure...
    p6_llf <- function(th0, th1, th2, th3, th4, th5, p, sig){
      
      # define some intermediate terms 
      
      # conditional mean of epsilon error  
      q1dt[, m_eps := -(1/sig^2)*p*(x2-(th3+th4*x1+th5*z)) ]
      
      # conditional variance of epsilon error 
       v_eps <- 1-(p^2/sig^2)
      
      # get prob y equals one conditional on x1, x2, z, theta 
       q1dt[, int := th0 + th1*x1 + th2*x2]
      q1dt[, p_y_1 := pnorm( th0 + th1*x1 + th2*x2 , m_eps, sqrt(v_eps))]

      # get pro x2 = x2i given x1 z theta
      q1dt[, p_x := dnorm( x2 - th3 - th4*x1 - th5*z, 0, sig)]
      
      # get log likelihoods 
      q1dt[, llf := y*log(p_y_1) + (1-y)*log(1-p_y_1) + log(p_x)]
      
      # sum them 
      sum_llf <- q1dt[, sum(llf)]
      
      # remove extra vars 
      q1dt[ ,`:=` (m_eps = NULL, p_y_1 = NULL, p_x = NULL, llf = NULL)]
      
      #  return negative sum 
      return(-sum_llf)
      
    }
    
    
    
    # create startign values 
    p6_start <- list(th0 = 0,
                      th1 = 0.0,
                      th2 = 0.0,
                      th3 = 0.0,
                      th4 = 0.0,
                      th5 = 0.0,
                      p = .01,
                      sig = 2)
  
    # run the mle function 
    p6_res <- mle(p6_llf, start = p6_start)
    
    # get the results I need 
    p6_res_tab <- data.table(variable = rownames(summary(p6_res)@coef),
                             summary(p6_res)@coef)
    
    p6_res_tab
    
    
    
    
#=====================#
# ==== Question 2 ====
#=====================#
    
  #=================#
  # ==== part 3 ====
  #=================#

  
    # load cereal data 
    cereal <- data.table(readxl::read_excel("C:/Users/Nmath_000/Documents/MI_school/Third Year/Econ 631/ps1/cereal_data.xlsx"))
      
      
    # get total market share by city year 
    cereal[, s0 := 1-sum(share), c("city", "year", "quarter" )]
    
    
    # create column for mean utility 
    cereal[, m_u := log(share) - log(s0)]
    

    # run ols, tidy it up, make it a data.table 
    q2_p3_ols <- data.table(tidy(lm(m_u ~ mushy + sugar + price , data = cereal)))
      
    # round p value 
    q2_p3_ols[, p.value := round(p.value, 6)]
    
    # create  sugar instroment 
    cereal[, i1_sugar := (sum(sugar) - sugar)/ (.N-1), c('firm_id', "city", "quarter")]

    # create mush intrument
    cereal[, i1_mushy := (sum(mushy) - mushy)/(.N-1), c('firm_id', "city", "quarter")]
      
    # create price instrument 
    cereal[, i1_price := (sum(price) - price)/ (.N-1), c('firm_id', "city", "quarter")]
    
    # now do 2sls, tidy it up, make it a data.table 
    q2_p3_iv1 <- data.table(tidy(ivreg(m_u ~ mushy + sugar + price + firm_id - 1 | i1_sugar + i1_mushy + i1_price + firm_id, data = cereal)))
    
    q2_p3_iv1[, p.value := round(p.value,6)]
    
    # now create second set of instroments 
    cereal[,  , ]
    
    
#===============================#
# ==== save output to latex ====
#===============================#
  
  if(opt_save){
    
    
    print(xtable(probit_res, type = "latex"), 
          file = paste0(f_out, ""),
          include.rownames = FALSE,
          floating = FALSE)
    
    
    
    
  }
    
    
    
    
    
    
    
    
    

   