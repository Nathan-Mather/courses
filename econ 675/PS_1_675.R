#=======================================#
# ==== Load packages and clear data ====
#=======================================#

library(data.table)
library(Matrix)
library(lmtest)
library(sandwich)
library(broom)

# clear objects and script 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

#=============================#
# ==== Question 2  part 4 ====
#=============================#

  #=========================#
  # ==== geneerate data ====
  #=========================#

    # set n_col and n_row 
    n_col <- 10
    n_row <- 100
    n_cell <- n_col*n_row 
    
    # create random matrices 
    y_data <- matrix(runif(n_row, 0, 100), nrow = n_row, ncol = 1)
    x_data <- matrix(runif(n_cell, 0, 1), nrow = n_row, ncol = n_col)
  
  #================================#
  # ==== write function for q2 ====
  #================================#
  
    x = x_data
    y = y_data
    
    mat_reg <- function(x = NULL, y = NULL, opt_chol = FALSE, CI_level = .95){
      
      # get matrix size parameters 
      n_col <- ncol(x)
      n_row <- nrow(x)

    #========================#
    # ==== estimate beta ====
    #========================#
    
      # check which inverse function to use 
      if(!opt_chol){
        
        # use standard inverse 
        B <- Matrix::solve(Matrix::crossprod(x, x))%*%(Matrix::crossprod(x, y))
      }else{
        
        # use cholesy incerse 
        chol_m <- chol(Matrix::crossprod(x, x))
        B<- chol2inv(chol_m)%*%(Matrix::crossprod(x, y))
        
      }
      
    #=====================#
    # ==== estimate V ====
    #=====================#
      
      # calculate residuals 
      my_resid <- y - x%*%B
      
      # see if I need to use cholesky 
      if(!opt_chol){
     
        # calculate asymptotic variance 
        V <- solve(crossprod(x, x)) %*% (t(x) %*% diag(as.numeric(my_resid^2), nrow = n_row, ncol = n_row) %*% x) %*% solve(crossprod(x, x)) 
      
      }else{
        
        A_inv <-   chol2inv(chol_m) %*% (t(x) %*% diag(as.numeric(my_resid^2), nrow = n_row, ncol = n_row) %*% x) %*% chol2inv(chol_m)
        V <- A_inv 
        
        }
      sqrt(diag(V))

    #======================#
    # ==== other stats ====
    #======================#
      
      # start by putting beta and  diagonal of variance in a data.table 
      out_dt <- data.table(beta = as.numeric(B), V_hat = diag(V) )
      
      # calculate standard errors 
      out_dt[, se := sqrt(V_hat)]
      
      # calculate t test 
      out_dt[, t_test := beta/(se)]
      
      # calculate p values 
      out_dt[, p_value := 2*(1- pt((abs(t_test)), n_row))]

      # calculate confidence interval 
      out_dt[, CI_L := beta - (se) * qt(1-((1-CI_level)/2), n_row )]
      out_dt[, CI_U := beta + (se) * qt(1-((1-CI_level)/2), n_row )]
      
      # drop v_hat cause I dont need it 
      out_dt[, V_hat := NULL]
      
      # create list to return 
      out_list <- list() 
      
      out_list[["results"]] <- out_dt
      out_list[["varcov"]] <- V
      
      return(out_list)
      
}
      
#======================================#
# ==== run function on random data ====
#======================================#

  # run on random data with and without cholesky 
  reg_1 <- mat_reg(x = x_data, y = y_data, opt_chol = FALSE)
  reg_2 <- mat_reg(x = x_data, y = y_data, opt_chol = TRUE)
    
  # compare coefficients, differences are just floating point errors 
  coeff_diff <- reg_1[["results"]][, beta] - reg_2[["results"]][, beta]
  
  # compare varcov NOTE: differences are just floating point errors 
  all.equal(reg_1$varcov, reg_2$varcov)
  reg_1$varcov -  reg_2$varcov
#============================#
# ==== Question 2 part 5 ====
#============================#
  
  #==========================#
  # ==== matrix function ====
  #==========================#


    # load daata 
    lalonde_dt <- fread("C:/Users/Nmath_000/Documents/MI_school/Second Year/675 Applied Econometrics/hw/hw1/LaLonde_1986.csv")
    
    # grab y matrix 
    y_la <- as.matrix(lalonde_dt[, earn78])
    
    # create other vars for regression 
    lalonde_dt[, educ_sq := educ^2]
    lalonde_dt[, black_earn74 := black*earn74]
    lalonde_dt[, const := 1]
   
     # grab x vars 
    x_vars <- c("treat", "black", "age", "educ", 
                "educ_sq", "earn74","black_earn74", 
                "u74","u75")
    x_la <- as.matrix(lalonde_dt[, c("const", x_vars), with = FALSE])
    
    # run function on this data 
    lalonde_reg <- mat_reg(x = x_la, y = y_la)
    lalonde_reg_dt <- lalonde_reg[["results"]]
  #===================#
  # ==== using lm ====
  #===================#
    
    # get regression formula 
    reg_form <- as.formula(paste("earn78~", paste(x_vars, collapse="+")))
    
    # run regression 
    lalonde_lm <- lm(reg_form, lalonde_dt)
    
    # get summary, NOTE: these are NOT robust standard errors 
    lalong_lm_dt <- summary(lalonde_lm)$coefficients
      
    # get robust standard errors. I use HCO to match my math above 
    # any differnces are floating point errors 
    lm_robust <- data.table(tidy(coeftest(lalonde_lm, vcov = vcovHC(lalonde_lm, type="HC0"))))
    lm_robust
    
    