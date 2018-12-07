

#=======================#
# ==== PS 6 Metrics ====
#=======================#

#========================================#
# ==== Load packages clear workspace ====
#========================================#

# clear workspace 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# laod packages 
library(data.table)         # helps do everything faster and better
library(ggplot2)            # for pretty plots
library(xtable)             # for latex tables
library(rdrobust)           # for RD plots and other stuff
library(rddensity)          # for RD density continuity tests
library(rdlocrand)          # for RD randomization inference
library(grid)
library(gridGraphics)
library(ggplotify)          # use this to fix the wierd graphs in rdrobust 
library(broom)
library(sandwich)
# output folder 
f_out <- "c:/Users/Nmath_000/Documents/Code/courses/econ 675/ps_6_tex/"

# plot attributes 
plot_attributes <- theme( plot.background = element_rect(fill = "lightgrey"),
                          panel.grid.major.x = element_line(color = "gray90"), 
                          panel.grid.minor  = element_blank(),
                          panel.background = element_rect(fill = "white", colour = "black") , 
                          panel.grid.major.y = element_line(color = "gray90"),
                          text = element_text(size= 30),
                          plot.title = element_text(vjust=0, hjust = 0.5, colour = "#0B6357",face = "bold", size = 40))


#=====================#
# ==== Question 2 ====
#=====================#

# load data 
hs <- fread("c:/Users/Nmath_000/Documents/MI_school/Second Year/675 Applied Econometrics/hw/hw6/HeadStart.csv")
hs

  #=================#
  # ==== Q2.1.1 ====
  #=================#

    # Evenly-spaced bins, IMSE optimal
    rdplot(hs[,mort_related_pre], hs[,povrate60], c =0, p=1, binselect = "es", x.label="povrate60", y.label="mort_related_pre", title="Evenly-spaced bins, IMSE optimal")
    # I guess do this because this since I can't make ggplots with this function 
    dev.copy(pdf, paste0(f_out, 'plot_2.1.1.i.a.pdf'))
    dev.off()
    
    # Evenly-spaced bins, mimicking variance 
    rdplot(hs[,mort_related_pre],hs[,povrate60],p=1, binselect = "esmv",x.label="povrate60",y.label="mort_related_pre",title="Evenly-spaced bins, mimicking variance ")
    dev.copy(pdf, paste0(f_out, 'plot_2.1.1.i.b.pdf'))
    dev.off()
    
    # Quantile-spaced bins, IMSE optimal
    rdplot(hs[,mort_related_pre],hs[,povrate60],p=1, binselect = "qs",x.label="povrate60",y.label="mort_related_pre",title="Quantile-spaced bins, IMSE optimal")
    dev.copy(pdf, paste0(f_out, 'plot_2.1.1.ii.a.pdf'))
    dev.off()
    
    # Quantile-spaced bins, mimicking variance
    rdplot(hs[,mort_related_pre],hs[,povrate60],p=1, binselect = "qsmv", x.label="povrate60", y.label="mort_related_pre", title="Quantile-spaced bins, mimicking variance")
    dev.copy(pdf, paste0(f_out, 'plot_2.1.1.ii.b.pdf'))
    dev.off()
    
  #==================#
  # ==== Q 2.1.2 ====
  #==================#
    
  ##### i Histogram ###
     # add above below zero flag 
    hs[povrate60 >= 0, f_cut := "Above Cutoff"]
    hs[povrate60 < 0, f_cut := "Below Cutoff"]
    
    # make a histogram before and after cutoff 
    plot_2.1.2.i <- ggplot(hs, aes(povrate60)) + 
                    geom_histogram(aes(fill = f_cut), breaks = seq(-50,25,2)) + 
                    xlab("60 - Poverty rate") + ylab("Count") + ggtitle("Histogram of Running Variable") + 
                    scale_fill_discrete(name = "RD Group")
    # check it out 
    plot_2.1.2.i
    
    # then add atrribute that make it look good once save 
    plot_2.1.2.i <- plot_2.1.2.i + plot_attributes
    
  ## ii local binomial test ###
  # make a grid of bandwidiths to test 
  bi_test <- data.table(bandwidth = seq(.4,4,.2))
  
  # get number above and below cutoff for each bandwidth 
  bi_test[, below_c := nrow(hs[abs(povrate60) <= bandwidth/2 & f_cut == "Below Cutoff"]), bandwidth] 
  bi_test[, above_c := nrow(hs[abs(povrate60) <= bandwidth/2 & f_cut == "Above Cutoff"]), bandwidth] 
  bi_test[, total := below_c + above_c]
  
  # do binomial tests 
  bi_test[, bin_test := binom.test(below_c, total, .5)$p.value, bandwidth]  
    
  # make this table for hw 
  table_2.1.2.ii <- bi_test[, -c("total")]
  colnames(table_2.1.2.ii) <- c("Bandwidth", "Number Below Cutoff", "Number Above Cutoff", "binomial P Valu")

  ## iii continuity in design tests 
  ## Continuity in density tests (defaults are triangular kernel, jackknife SEs)
  cdt <- rddensity(hs$povrate60)

  #==================#
  # ==== Q 2.2.1 ====
  #==================#

  # create polynomials 
  p_dt <- as.data.table(poly(hs$povrate60, 6))
  colnames(p_dt) <- paste0("poly_pov60_", colnames(p_dt))
  p_dt[,poly_pov60_1 := NULL ]
  hs <- cbind(hs, p_dt)
  hs[, treatment := as.numeric(f_cut == "Above Cutoff")]
  
  # initialize data
  table_2.2.1 <- data.table(value = c("Estimate", "Standard Error"))

  # write loop  to make everything we need for polynomial of order N 
  poly_n <- 3
  for(poly_n in 3:6){
    
    # get x variables we need, there is a better way to do this out this works fine 
    x_vars <- c("povrate60", "treatment", grep(paste(as.character(c(1:poly_n)), collapse = "|"), colnames(hs), value = TRUE))
    
    # make formula 
    reg_form <- as.formula(paste0("mort_related_post ~", paste(x_vars, collapse = " + ")))
    
    # run regressin 
    reg_o1 <- lm(reg_form, data = hs)
    reg_o <- data.table(tidy(reg_o1))
    tab_col <- reg_o[term == "treatment", c(estimate, std.error)]
   
    # put stuff in table 
    table_2.2.1[, temp := tab_col]
    setnames(table_2.2.1, "temp", paste0("Polynomial ", poly_n))
    
    # get fitted values and data
    temp.rd = rdplot(hs[,mort_related_post], hs[,povrate60] ,hide=TRUE)
    temp.rd_dt <- data.table(rdplot_mean_x = temp.rd$vars_bins$rdplot_mean_x, rdplot_mean_y =  temp.rd$vars_bins$rdplot_mean_y)
    fitted_dt <- data.table(x = hs$povrate60, y = reg_o1$fitted.values)
    
    # make plot 
    t_plot <- ggplot() + geom_point(data = temp.rd_dt, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = "Binned Values"))
    t_plot <- t_plot + geom_point(data = fitted_dt, aes(x = x, y = y, color = "Fitted Values"))
    t_plot <- t_plot + xlab("60 - Poverty rate") + ylab("HS Related Mortality") + scale_color_manual(values = c("black", "blue"), name = "")
    t_plot <- t_plot + ggtitle(paste0("Fitted Values for Polynomial of degree ", poly_n)) + plot_attributes
    t_plot
    
    # save plot 
    png(paste0(f_out, "plot_2.2.1_poly_", poly_n, ".png"), height = 800, width = 800, type = "cairo")
    print(t_plot)
    dev.off()
    
  }
  
  x <- hs[,povrate60]
  Y <- hs[,mort_related_post]
  treat <- hs[,treatment ]
  X.pol = cbind(x,x^2,x^3,x^4,x^5,x^6)
  
  # Run polynomial regressions
  global.regs = lapply(0:3,function(i) lm(Y ~ treat + X.pol[,c(1:(3+i))]))
  
  # Get point estimates
  global.betas = sapply(1:4,function(i) global.regs[[i]]$coefficients[2])
  
  # Get robust SEs
  global.SEs   = sapply(1:4,function(i) sqrt(diag(vcovHC(global.regs[[i]],"HC2")))[2])
  
  # Put results together
  global.results = rbind(global.betas, global.SEs)
  colnames(global.results) = c(3,4,5,6)
  xtable(global.results,digits=c(0,2,2,2,2))
  

  plot(temp.rd$vars_bins$rdplot_mean_x,temp.rd$vars_bins$rdplot_mean_y,pch=20,xlab="povrate60",ylab="mort_related_post")
  points(x,global.regs[[2]]$fitted.values,pch=6,col="blue")
  abline(v=0)
  dev.copy(pdf,'q2-2-const.pdf')
  dev.off()
  


  
  
  
  
  
  