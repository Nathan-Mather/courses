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

# use choleskey invers chol2inv
?chol2inv

# use symmetric inverse 
chol2inv

?solve
