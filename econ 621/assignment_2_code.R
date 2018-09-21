#==============================#
# ==== Labor Problem set 2 ====
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

#======================#
# ==== data set up ====
#======================#

# laod data 
dt <- data.table(read.dta("c://users/Nmath_000/Documents/MI_school/Second Year/621 Labor/Assignments/Assignment_2/MROZ.DTA"))

# create neede vars 
dt[, age_sq := age^2]
dt[,age_cu := age^3]
dt[,educ_sq := educ^2]
dt[,educ_cu := educ^3]
dt[,age_educ := age*educ]
dt[,age_sq_educ := age_sq*educ]
dt[,age_educ_sq := age*educ_sq]
dt[, nonlab_i := (faminc - wage*hours - huswage*hushrs)/1000]



#===============================#
# ==== a  replicate table 3 ====
#===============================#


# get vars to mean ans sd 
var_l <- c("age", "educ", "kidslt6", "kidsge6", "husage", "huseduc", "wage", "huswage", "nonlab_i", "hours", "hushrs"  )

tb1 <- melt.data.table(round(dt[,lapply(.SD,mean), .SDcols =var_l],2), value.name = "Full Sample Mean")
tb2 <- melt.data.table(round(dt[,lapply(.SD,sd), .SDcols =var_l],2), value.name = "Full Sample Standard Deviation")
tb3 <- melt.data.table(round(dt[inlf == 1,lapply(.SD,mean), .SDcols =var_l],2), value.name = "Working Women Mean")
tb4 <- melt.data.table(round(dt[inlf == 1,lapply(.SD,sd), .SDcols =var_l],2), value.name = "Working Women Standard Deviation")

# merge them all 
tb_f <- Reduce(function(x, y) merge(x, y, by = "variable", all = T), list(tb1, tb2, tb3, tb4))

#=========================#
# ==== b baseline ols ====
#=========================#

# run regression 
base_lm <- lm(hours ~ lwage + nwifeinc + kidslt6 + kidsge6  + age + educ , dt)

# get robust standard errors. 
lm_robust <- coeftest(base_lm, vcov = vcovHC(base_lm, type="HC0"))

lm_robust <- data.table(tidy(lm_robust))

lm_robust

#========================#
# ==== C IV estimate ====
#========================#


