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
    
    
   