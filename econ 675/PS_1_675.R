#=======================================#
# ==== Load packages and clear data ====
#=======================================#

library(data.table)
library(Matrix)

# clear objects and script 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

#=====================#
# ==== Question 2 ====
#=====================#

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
    
    # define vars for line by line 
    x <- x_data
    y <- y_data
  
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
      resid <- y - x%*%B
     
      # calculate asymptotic variance 
      V <- solve(crossprod(x, x)) %*% (t(x) %*% diag(as.numeric(resid^2), nrow = n_row, ncol = n_row) %*% x)
      
    #======================#
    # ==== other stats ====
    #======================#
      
      # start by putting beta and  diagonal of variance in a data.table 
      out_dt <- data.table(beta = as.numeric(B), V_hat = diag(V) )
      
      # calculate standard errors 
      out_dt[, se := sqrt(V_hat)/sqrt(n_row)]
      
      # calculate t test 
      out_dt[, t_test := beta/(se)]
      
      # calculate p values 
      out_dt[, p_value := 2*(1- pt(t_test, n_row))]

      # calculate confidence interval 
      out_dt[, CI_L := beta - (se) * qt(1-((1-CI_level)/2), n_row )]
      out_dt[, CI_U := beta + (se) * qt(1-((1-CI_level)/2), n_row )]
      
      
      return(out_dt[])
      
}
      
#======================================#
# ==== run function on random data ====
#======================================#

  # run on random data with and without cholesky 
  reg_1 <- mat_reg(x = x_data, y = y_data, opt_chol = FALSE)
  reg_2 <- mat_reg(x = x_data, y = y_data, opt_chol = TRUE)
    
  # compare coefficients 
  coeff_diff <- reg_1[, beta] - reg_2[, beta]
    
# run on 
    
    
    
    
    
    