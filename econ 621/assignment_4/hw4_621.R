#=======================================#
# ==== Load packages and clear data ====
#=======================================#

library(data.table)
library(Matrix)
library(lmtest)
library(sandwich)
library(broom)
library(ggplot2)
library(stats)
library(xtable)
library(haven)

# clear objects and script 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

#=====================#
# ==== Question 2 ====
#=====================#

cfl_dt <- data.table(read_dta("C:/Users/Nmath_000/Documents/MI_school/Second Year/621 Labor/Assignments/Assignment_4/cfl_jpe_data.dta"))

#==============#
# ==== 2 a ====
#==============#

# check out the variables 
all_c <- colnames(cfl_dt)

# grab a list of vars of interest
t_v <- c(paste0(c("h", "lnw", "age", "edu", "white", "black"), "f"), 
paste0(c("h", "lnw", "age", "edu", "white", "black"), "h"),
"child6", "child17", "mu3", "sexrat", "div1", "div2", "div3", "div4", "divindex")


# get means and format it like paper
tab_1 <- melt.data.table(round(cfl_dt[,lapply(.SD,mean), .SDcols =t_v],2), value.name = "Full Sample Mean")


#=============#
# ==== 2b ====
#=============#


# collect y_vars 
y_vf  <- "hf"
y_vh <- "hh"
x_vf <- c("lnwf", "lnwh", "lnwfwh", "mu3", "sexrat", "divindex","child6", "child17", "eduf", "agef",  "whitef")
x_vh <- c("lnwf", "lnwh", "lnwfwh", "mu3", "sexrat", "divindex","child6", "child17", "eduh", "ageh",  "whiteh")

z_vf <- c("agef2", "eduf2", "edp_f", "eduagef", "whitef" ,"spanf", "metro", "cite", "ville",
          "northe", "northc", "west" ,"prof", "juiff", "cathf", "sexrat", "divindex")
z_vh <- c("ageh2", "eduh2", "edp_h", "eduagef",  "whiteh" ,"spanh", "metro", "cite", "ville",
          "northe", "northc", "west" ,"proh", "juifh", "cathh", "sexrat", "divindex")

# now actually grab the vectors for female 
yf <- as.matrix(cfl_dt[, y_vf, with=FALSE])
xf <- as.matrix(cfl_dt[, x_vf, with=FALSE])
zf <- as.matrix(cfl_dt[, z_vf, with=FALSE])

res <- gmm(yf ~ xf, zf)
summary(res)

res$


  
  
  
  
  
  
  
  
  
x_vf <- c(lnwf lnwh lnwfwh mu3 sexrat divindex child6 child17 eduf agef  whitef)
x_vh <- c(lnwf lnwh lnwfwh mu3 sexrat divindex child6 child17 eduh ageh  whiteh)

z_vf <- c(agef2 eduf2 edp_f eduagef whitef spanf metro cite ville
          northe northc west prof juiff cathf sexrat divindex)
z_vh <- c(ageh2 eduh2 edp_h eduagef  whiteh spanh metro cite ville
          northe northc west proh juifh cathh sexrat divindex)

