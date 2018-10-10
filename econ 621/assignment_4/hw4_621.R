#=======================================#
# ==== Load packages and clear data ====
#=======================================#

library(data.table)
library(haven)
library(xtable)

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
tab_1 <- melt.data.table(round(
  cfl_dt[,lapply(.SD,mean), .SDcols =t_v],2), value.name = "Full Sample Mean")


#=====================#
# ==== save table ====
#=====================#
print(xtable(tab_1, type = "latex", 
             digits = 3), 
      file = "C:/Users/Nmath_000/Documents/Code/courses/econ 621/assignment_4/means_tab_2a.tex",
      include.rownames = FALSE,
      floating = FALSE)
