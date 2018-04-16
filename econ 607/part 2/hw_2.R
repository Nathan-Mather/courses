#===============================#
# ==== econ 607 part 2 HW 2 ====
#===============================#
  # load packages 
  library(data.table)
  library(ggplot2)
  library(Matrix)
  library(quantmod)
  
  # clear objects 
  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  options(scipen = 999)
  cat("\f")

#====================================#
# ==== create HP filter function ====
#====================================#
  hp_filter <- function(y = NULL, u = NULL){
    
    # figure out the length of our unput vector 
    v_T <- length(y)
    
    # create blank matrix 
    A <- matrix(0, v_T, v_T)
    
    # fill in the special rows 
    A[1,] <- c(1+u, -2*u, u, rep(0,v_T-3))
    A[2,] <- c(-2*u, 1+5*u, -4*u, u, rep(0,v_T-4))
    A[v_T-1, ] <- c(rep(0,v_T-4), u, -4*u, 1+5*u, -2*u)
    A[v_T,] <- c(rep(0,v_T-3), u, -2*u, 1+u)
    
    # for rest of rows fill in 
    for(t in 3:(v_T-2)){
      
      A[t,] <- c(rep(0,t-3), u, -4*u, 1+6*u, -4*u, u, rep(0,v_T-t-2))
      
    }
    
    # now colculate smoothed dat 
    smooth_data <- solve(A)%*%y
    
    # now put it in deviations from the trend 
    smooth_data = smooth_data - y;
    
  }

#====================#
# ==== load data ====
#====================#
  
  # initialize list for data 
  data_list <- list()
  
  # getSymbols("GDP", src = "FRED", from = as.Date("1960-01-04"), to = as.Date("2009-01-01"), auto.assign=F)
  # grab GDP
  data_list[[1]] <- getSymbols("GDP", src = "FRED", auto.assign = FALSE)
  
  # consumption
  data_list[[2]] <- getSymbols("PCEC", src = "FRED", auto.assign = FALSE)
  
  # investment
  data_list[[3]] <- getSymbols("PNFI", src = "FRED", auto.assign = FALSE)
  
  # hours worked 
  data_list[[4]] <- getSymbols("HOANBS", src = "FRED", auto.assign = FALSE)
  
  # wages 
  data_list[[5]] <- getSymbols("COMPRNFB", src = "FRED", auto.assign = FALSE)
  
  # Implicit GDP Deflator 
  data_list[[6]] <- getSymbols("GDPDEF", src = "FRED", auto.assign = FALSE)
  
  # civilian non_inst pop 16+
  data_list[[7]] <- getSymbols("CNP16OV", src = "FRED", auto.assign = FALSE)
  
  # TFP solow resid  
  tfp_dt <- data.table(readxl::read_excel("C:/Users/Nmath_000/Documents/MI_school/macro 607/part 2/data/quarterly_tfp.xlsx", sheet = "quarterly", skip =1 ))
  
  
#==================#
# ==== Fix TFP ====
#==================#

  # edit tfp data. Keep only date and dtfp. Remove anything in the date column that doesn't start with a number 
  tfp_dt<- tfp_dt[!is.na(dtfp) & grepl("^[[:digit:]]", date), c( "date", "dtfp")]
  
  # do edit with initial value that pablo wanted 
  # set initial value 
  tfp_dt[, tfp := 100]
  
  # quick loop to fill in values 
  for(r_t in 2:nrow(tfp_dt)){
    
    tfp_dt[r_t, tfp := exp(log(tfp_dt[r_t-1, tfp]) + (tfp_dt[r_t, dtfp]/400))]
    
  }
  
#============================#
# ==== set up other data ====
#============================#

  # convert all to data.table 
  data_list <- lapply(data_list, as.data.table)
  
  # merge data #note this will eliminate extra months for population 
  merger <- function(x,y){merge(x,y, by = "index")}
  dt <- Reduce(merger, data_list)

  # deflate variables and per capita them and make the in $ not billions of $
  to_deflate <- c("GDP", "PCEC", "PNFI" )
  
  # fix deflator 
  dt[, GDPDEF_dec := GDPDEF/100]
  
  # fix population 
  dt[, CNP16OV_real := CNP16OV*1000]
  
  # deflate and per cap data 
  dt[, (paste0("real_", to_deflate, "_percap" )) := lapply(.SD, function(x){x/((GDPDEF_dec)*CNP16OV_real)*10^9}), .SDcols = to_deflate]

  # Grab all variables to hp filter 
  all_vars <- c(paste0("real_", to_deflate, "_percap" ),  "HOANBS", "COMPRNFB", "tfp")
  
  # fix date on tfp and merge it onto FRED data 
  tfp_dt[, index := date ]
  tfp_dt[, index := gsub(":Q1", "-01-01",index) ]
  tfp_dt[, index := gsub(":Q2", "-04-01",index) ]
  tfp_dt[, index := gsub(":Q3", "-07-01",index) ]
  tfp_dt[, index := gsub(":Q4", "-10-01",index) ]
  tfp_dt[, index := as.Date(index)]
  
  # merge on TFP
  dt <- merge(dt, tfp_dt, by = "index")

  
  # log data first 
  dt[, (all_vars) := lapply(.SD, log), .SDcols = all_vars]
  
  # hp filter it 
  dt[, (paste0("HP_Filtered_", all_vars)) := lapply(.SD, hp_filter, 1600), .SDcols = all_vars]
  
  
#====================#
# ==== plot data ====
#====================#

  # create xwalks of plot info for each variable
  to_plot <- paste0("HP_Filtered_", all_vars)
  titles <- c("Real GDP Per Capita", "Real Consumption Per Capita", "Real Total Private fixed Investment Per Capita",
             "Hours Worked Index", "Real Compensation Per Hour Index", "Total Factor Productivity (Solow Residual)")
  
  titles <- paste0("HP Filtered ", titles)
  
  plot_attributes <- theme(plot.background    = element_rect(fill  = "white"),
                           panel.grid.major.x = element_line(color = "gray90"), 
                           panel.grid.minor   = element_blank(),
                           panel.background   = element_rect(fill  = "white", colour = "black") , 
                           panel.grid.major.y = element_line(color = "gray90"),
                           text               = element_text(size  = 14),
                           plot.title         = element_text( colour = "#0B6357",face = "bold", size = 18),
                           legend.position = "none")
  
  # loop over vars
  for( i in 1:length(to_plot)){
    
    # grab variable and plot 
    var_i <- to_plot[[i]]
    title_i <- titles[[i]]
    
    # create plots.
    plot_i <-  ggplot(data = dt, aes_string(x = "index", y = var_i)) + geom_line()
    
    # add titles 
    plot_i <- plot_i + labs(x = "Time by Quarter", y = title_i, title = paste0(title_i, " Over Time"))
    print(plot_i)
    
    # save plot somewhere 
    ggsave(paste0("C:/Users/Nmath_000/Documents/MI_school/macro 607/part 2/plots/", title_i, ".png"),  plot_i, height = 6, width = 9)
    
    }

  
#========================#
# ==== create tables ====
#========================#
  
  # subset data to make it easier to work with 
  dt_s <- dt[, c( "index", to_plot), with = FALSE]
  
  # get standard dev for each of these 
  std <- dt_s[,  lapply(.SD, sd), .SDcols = to_plot]
  corr <- dt_s[,  lapply(.SD, function(x){cor(x, HP_Filtered_real_GDP_percap)}), .SDcols = to_plot]

  # create lagged data 
  dt_s[, (paste0("lagged_", to_plot)) := lapply(.SD, shift), .SDcols = to_plot]
  
  # calculate auto correlations 
  auto_cor <- dt_s[,  lapply(.SD, function(x){cor(x, get(paste0("lagged_", to_plot)), use = "complete.obs")}), .SDcols = to_plot]
  
  # create data table of variables and variable titles to fill in data for 
  stat_tab <- data.table(Measure = titles, variable = to_plot )

  # melt all stats
  std_l <- melt.data.table(std, measure.vars = to_plot,variable.name = "variable", value.name = "std")
  corr_l <- melt.data.table(corr, measure.vars = to_plot, variable.name = "variable", value.name = "corr")
  auto_cor_l <- melt.data.table(auto_cor, measure.vars = to_plot, variable.name = "variable", value.name = "auto_cor")
  
  # merge them all together 
  stat_tab <- merge(stat_tab, std_l, "variable")
  stat_tab <- merge(stat_tab, corr_l, "variable")
  stat_tab <- merge(stat_tab, auto_cor_l, "variable")

  # create GDP variance ration 
  gdp_std <- stat_tab[variable == "HP_Filtered_real_GDP_percap", std]
  stat_tab[, gdp_var_ratio := std/gdp_std]
  
  
  
  #=========================#
  # ==== run regressions ====
  #=========================#

  # I logged tfp above, so thats fine. Create a time index starting at one 
  dt[, time_t := 1:nrow(dt)]
  
  # regress log SR on time 
  reg1 <- lm(dt$tfp~dt$time_t)
  
  # grab residuals 
  resids_1 <- data.table(resids = resid(reg1))
  resids_1[, lag_resid := shift(resids)]
  resids_1 <- resids_1[!is.na(lag_resid)]
  
  # regress residuals on lagged residuals 
  reg2 <- lm(resids_1$resids~resids_1$lag_resid)
  
  rho <- coef(reg2)[[2]]
  
  sigma <- sd(resid(reg2))
  
  # put values in a data.table 
  parms <- data.table(rho_a = rho, sigma_a = sigma)
  
  #====================#
  # ==== save data ====
  #====================#

  write.csv(stat_tab, "C:/Users/Nmath_000/Documents/MI_school/macro 607/part 2/hw2_table.csv", row.names = FALSE)
  write.csv(parms, "C:/Users/Nmath_000/Documents/MI_school/macro 607/part 2/hw2_parms.csv", row.names = FALSE)

  
  