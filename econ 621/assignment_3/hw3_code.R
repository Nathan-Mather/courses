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


#===============#
# ==== Q1 A ====
#===============#

# set up the funciton for MLE 
probit_ll <- function(lnf, theta1 ){
  
  fi( ) lnf_new <- 
  
}

# fun the funciton 







