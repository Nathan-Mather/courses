#===================================#
# ==== Convert pdf file to csvs ====
#===================================#

# clear data and consol
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# load packages 
library(pdftools)
library(stringr)
library(xlsx)
library(tabulizer)
library(data.table)

# have you done the shiny app?
shiny_app_done <- FALSE

# get path base 
path_pdf <- "C:/Users/Nmath_000/Documents/MI_school/Second Year/PP 713/research_project/digest_of_ed_2016.pdf"

#=======================#
# ==== convert file ====
#=======================#


if(!shiny_app_done){
  # find areas with the table on it ()
  area <- locate_areas(path_pdf, pages = 420, resolution = 70L)
  
  # save this so I dont have to do the shiny app again 
  save(area, file = "C:/Users/Nmath_000/Documents/MI_school/Second Year/PP 713/research_project/pdf_area.R")

  
  }else{
  
  # load this in if you have already done the shiny app thing once 
  load("C:/Users/Nmath_000/Documents/MI_school/Second Year/PP 713/research_project/pdf_area.R")
}


  # convert page we need 
  out1 <- extract_tables(path_pdf, method = "stream", pages = 420, guess = FALSE, area = area )
  out1_dt <- data.table(out1[[1]][-c(1,2),])
  setnames(out1_dt, colnames(out1_dt), make.names(out1[[1]][1,]))

  #=======================#
  # ==== format stuff ====
  #=======================#

  # remove dots 
  out1_dt[, State := gsub("\\.", "", State)]
  
  # trim leaading and ending spaces 
  out1_dt[, State :=  gsub("^\\s+|\\s+$", "", State)]
  
  # extract the higher age
  # this is first remove the number before "to" 
  variable_splitter <- function (in_var_split = NULL, in_val_position = NULL, in_val_delim = NULL){
    out_var <- sapply(strsplit(in_var_split, in_val_delim), function(x) x[in_val_position])
    return(out_var)
  }
  
  # get columns that need fixing 
  cols_to_fix <- grep("X", colnames(out1_dt), value = TRUE)
  
  # strip out the "x to " portion of the string 
  out1_dt[ , (cols_to_fix) := lapply(.SD, variable_splitter, in_val_position= 2, in_val_delim=" to "), .SDcols = cols_to_fix]
  
  # now strip out the footnores that made it in 
  out1_dt[ , (cols_to_fix) := lapply(.SD, variable_splitter, in_val_position= 1, in_val_delim=" "), .SDcols = cols_to_fix]
  

  #=========================#
  # ==== write out file ====
  #=========================#

  # write this to a csv   
  write.csv(out1_dt, "C:/Users/Nmath_000/Documents/MI_school/Second Year/PP 713/research_project/digest_of_ed_csv.csv" )



