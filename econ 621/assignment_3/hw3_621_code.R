#==============================#
# ==== Labor Problem set 3 ====
#==============================#


#=======================================#
# ==== load packages and clear data ====
#=======================================#

# clear data and consol 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# load packages 
library(data.table)
library(lmtest)
library(sandwich)
library(broom)
library(foreign)
library(AER)
library(ivpack)
library(Matrix)
library(knitr)
library(kableExtra)
library(xtable)
library(stats)
#======================#
# ==== data set up ====
#======================#

# laod data 
M_dt <- data.table(read.dta("c://users/Nmath_000/Documents/MI_school/Second Year/621 Labor/Assignments/Assignment_3/MROZ.DTA"))

# create neede vars 
M_dt[, age_sq := age^2]
M_dt[,age_cu := age^3]
M_dt[,educ_sq := educ^2]
M_dt[,educ_cu := educ^3]
M_dt[,age_educ := age*educ]
M_dt[,age_sq_educ := age_sq*educ]
M_dt[,age_educ_sq := age*educ_sq]
M_dt[, nonlab_i := (faminc - wage*hours - huswage*hushrs)/1000]
M_dt[, const := 1]


# create a list to store main results 
r_l <- list()


# create variable for working 
M_dt[hours > 0, working := 1]
M_dt[hours <= 0, working := 0]


#================#
# ==== notes ====
#================#

# euqation 1 
# hours ~ lwage + nwifeinc + kidslt6 + kidsge6  + age + educ

# EQ 4 
# lwag~ age + educ + age_sq + age_cu + educ_sq + educ_cu + 
#   age_educ + age_sq_educ + age_educ_sq + 
#   unem + city + motheduc + fatheduc

# probit EQ
# working ~nwifeinc + kidslt6 + kidsge6 + age + educ +
# age_sq + age_cu + educ_sq + educ_cu + 
#   age_educ + age_sq_educ + age_educ_sq + 
#   unem + city + motheduc + fatheduc

#===============================#
# ==== testing out function ====
#===============================#

# grab y vector 
y <- as.matrix(M_dt[, working])

# grab y matrix 
x_vars <- c("const", "nwifeinc", "kidslt6", "kidsge6" , "age", "educ",
  "age_sq", "age_cu", "educ_sq", "educ_cu", 
     "age_educ", "age_sq_educ", "age_educ_sq", 
     "unem", "city", "motheduc", "fatheduc")
  
x <- as.matrix(M_dt[, x_vars, with = FALSE])

# for line by line checks 
beta <- rep(0, length(x_vars))
mu <- .5
sigma <- 1

LL <- function(beta0,
               beta1,
               beta2, 
               beta3,
               beta4,
               beta5,
               beta6,
               beta7,
               beta8,
               beta9,
               beta10,
               beta11,
               beta12,
               beta13,
               beta14,
               beta15,
               beta16,
               mu, 
               sigma) {
  
  # create beta vector 
  beta = c(beta0, beta1, beta2, beta3,beta4,beta5,beta6,beta7,beta8,beta9,beta10, beta11, beta12,beta13,beta14,beta15,beta16)
  # Find residuals
  R = y - x %*% beta 
  #
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R = dnorm(R, mu, sigma)
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(log(R))
}

fit <- stats4::mle(minuslogl = LL, start = list(beta0 = 1,
                                                beta1 = 1,
                                                beta2 = 1, 
                                                beta3 = 1,
                                                beta4 = 1,
                                                beta5 = 1,
                                                beta6 = 1,
                                                beta7 = 1,
                                                beta8 = 1,
                                                beta9 = 1,
                                                beta10 = 1,
                                                beta11 = 1,
                                                beta12 = 1,
                                                beta13 = 1,
                                                beta14 = 1,
                                                beta15 = 1,
                                                beta16 = 1,
                                                mu = .2, 
                                                sigma=1))



#===============#
# ==== Q1 A ====
#===============#

# set up the funciton for MLE 
probit_ll <- function( theta1 ){
  
  lnf <- log(dnorm(theta1))
  
}

# fun the funciton 
?mle







