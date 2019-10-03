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

  #=================#
  # ==== part 1 ====
  #=================#
  
    
    # lad data 
    q1dt <- fread("file:///C:/Users/Nmath_000/Documents/MI_school/Third Year/Econ 631/ps1/ps1.dat")
    
    
    # create variable names 
    setnames(q1dt, colnames(q1dt), c("y", "x1", "x2", "z"))
    
    
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
    probit_res <- data.table(summary(probit_res)@coef)
    
    # check my results 
    denyprobit <- glm( y ~ x1+ x2, 
                      family = binomial(link = "probit"), 
                      data = q1dt)
    
    # looks good 
    summary(denyprobit)
    probit_res
  #================#
  # ==== Part3 ====
  #================#
    
    

    Probit_llf <- function(in_dt ){ 
      function(th0, th1, th2){
        
        
        mu <- in_dt[,  pnorm(th0 + th1*x1 + th2*x2)]
        
        -sum(in_dt[, y*log(mu) + (1-y)*log(1-mu)])
      }
    }
    
  
