#=======================================#
# ==== annoying game theory problem ====
#=======================================#

#MWG 8.E.1
# clear objects 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# load data.table (always)
library(data.table)

# function to rank untility values 
utility_ranker <- function(m= NULL, s = NULL, w = NULL){
  
  # create a list of utility value equations 
  u_values <- c("m",
                "m/2",
                "-w/2",
                "0",
                "(3*m)/4-(s+w)/4",
                "(m/2) - (s/4)",
                "(m-w)/4",
                "(m/2)-(s+w)/4",
                "(m-s)/4",
                "(m/4)-(s+w)/2",
                "(m/4)-(s/2)")
  
  # create data.table of values 
  dt <- data.table(equation = u_values)
  
  # fill in values with equatons 
  dt[, value := eval(parse( text = equation)), by = 1:nrow(dt)]
  
  # sort by value 
  dt <- dt[order(-value)]  
  
  # assign rank
  dt[, rank := .I]
  
  return(dt[])
}

# run the function 
case_1 <- utility_ranker(m = 25, s = 10 , w =15 )
case_1

case_2 <-utility_ranker(m = 1.75, s = 1 , w =1.5 )
case_2

case_3 <- utility_ranker(m = 1.5, s = 1, w = 1.75)
case_3

