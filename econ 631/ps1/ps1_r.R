#===============================================#
# ==== Inustrial ORganization PRoblem Set 1 ====
#===============================================#


# clear objects 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")


# Load packages 
library(data.table)
library(stats4)
library(broom)
library(AER)
library(xtable)
library(Matrix)
library(BLPestimatoR)
library(SQUAREM)
library(BB)

# set save option 

opt_save <- TRUE 

# set path for output 
f_out <- "C:/Users/Nmath_000/Documents/Code/courses/econ 631/ps1/tex/"

#=====================#
# ==== Question 1 ====
#=====================#

# laod data 
q1dt <- fread("file:///C:/Users/Nmath_000/Documents/MI_school/Third Year/Econ 631/ps1/ps1.dat")



# create variable names 
setnames(q1dt, colnames(q1dt), c("y", "x1", "x2", "z"))


  #=================#
  # ==== part 1 ====
  #=================#
    
    # write log likelihood function 
    Probit_llf <- function(th0, th1, th2){
      
      
      mu <- q1dt[,  pnorm(th0 + th1*x1 + th2*x2)]
      
      -sum(q1dt[, y*log(mu) + (1-y)*log(1-mu)])
    }
  
    # create startign values 
    prob_start <- list(th0 = 0, 
                       th1 = .01,
                       th2 = .01)
    
    # run the mle function 
    probit_res <- mle(Probit_llf, start = prob_start)
    
    # get the results I need 
    probit_res <- data.table(variable = rownames(summary(probit_res)@coef),
                             summary(probit_res)@coef)
      
    # check my results 
    check <- glm( y ~ x1+ x2, 
                      family = binomial(link = "probit"), 
                      data = q1dt)
    
    # looks good 
    summary(check)
    probit_res
    
  #================#
  # ==== Part3 ====
  #================#
    
    # define logit log likelihood
    logit_llf <- function(th0, th1, th2){
      
      
      mu <- q1dt[,  plogis(th0 + th1*x1 + th2*x2)]
      
      -sum(q1dt[, y*log(mu) + (1-y)*log(1-mu)])
    }
    
    # create startign values 
    logit_start <- list(th0 = 0, 
                       th1 = .01,
                       th2 = .01)
    
    # run the mle function 
    logit_res <- mle(logit_llf, start = logit_start)
    
    # get the results I need 
    logit_res <- data.table(variable = rownames(summary(logit_res)@coef),
                            summary(logit_res)@coef)
    
    # check my results 
    check <- glm( y ~ x1+ x2, 
                       family = binomial(link = "logit"), 
                       data = q1dt)
    # looks good 
    summary(check)
    logit_res
    
  #=================#
  # ==== Part 6 ====
  #=================#
    
    
    ## test values 
    # th0 = 0
    # th1 = 0.01
    # th2 = 0.01
    # th3 = 0.01
    # th4 = 0.01
    # th5 = 0.01
    # p = .01
    # sig = 2
    
    # define a log likliehood function 
    # not craxy about altering the data.tble within the function but it's fine 
    # next time maybe create objects rather than columns. Althoguh that may be slower? unsure...
    p6_llf <- function(th0, th1, th2, th3, th4, th5, p, sig){
      
      # define some intermediate terms 
      
      # conditional mean of epsilon error  
      q1dt[, m_eps := -(1/sig^2)*p*(x2-(th3+th4*x1+th5*z)) ]
      
      # conditional variance of epsilon error 
       v_eps <- 1-(p^2/sig^2)
      
      # get prob y equals one conditional on x1, x2, z, theta 
       q1dt[, int := th0 + th1*x1 + th2*x2]
      q1dt[, p_y_1 := pnorm( th0 + th1*x1 + th2*x2 , m_eps, sqrt(v_eps))]

      # get pro x2 = x2i given x1 z theta
      q1dt[, p_x := dnorm( x2 - th3 - th4*x1 - th5*z, 0, sig)]
      
      # get log likelihoods 
      q1dt[, llf := y*log(p_y_1) + (1-y)*log(1-p_y_1) + log(p_x)]
      
      # sum them 
      sum_llf <- q1dt[, sum(llf)]
      
      # remove extra vars 
      q1dt[ ,`:=` (m_eps = NULL, p_y_1 = NULL, p_x = NULL, llf = NULL)]
      
      #  return negative sum 
      return(-sum_llf)
      
    }
    
    # create startign values 
    p6_start <- list(th0 = 0,
                      th1 = 0.0,
                      th2 = 0.0,
                      th3 = 0.0,
                      th4 = 0.0,
                      th5 = 0.0,
                      p = .01,
                      sig = 2)
  
    # run the mle function 
    p6_res <- mle(p6_llf, start = p6_start)
    logLik(p6_res)
    # get the results I need 
    p6_res_tab <- data.table(variable = rownames(summary(p6_res)@coef),
                             summary(p6_res)@coef)
    
    p6_res_tab
    
    
    
    
#=====================#
# ==== Question 2 ====
#=====================#
    
  #=================#
  # ==== part 3 ====
  #=================#

  
    # load cereal data 
    cereal <- data.table(readxl::read_excel("C:/Users/Nmath_000/Documents/MI_school/Third Year/Econ 631/ps1/cereal_data.xlsx"))
      
      
    # get total market share by city year 
    cereal[, s0 := 1-sum(share), c("city", "year", "quarter" )]
    
    
    # create column for mean utility 
    cereal[, m_u := log(share) - log(s0)]
    
    #============#
    # ==== a ====
    #============#


    # run ols, tidy it up, make it a data.table 
    q2_p3_ols <- data.table(tidy(lm(m_u ~ mushy + sugar + price , data = cereal)))
      
    # round p value 
    q2_p3_ols[, p.value := round(p.value, 6)]
    
    #============#
    # ==== b ====
    #============#

    
    # create  sugar instroment 
    cereal[, (.N-1), c('firm_id', "city", "quarter")]
    cereal[, i1_sugar := (sum(sugar) - sugar)/ (.N-1), c('firm_id', "city", "quarter")]

    # create mush intrument
    cereal[, i1_mushy := (sum(mushy) - mushy)/(.N-1), c('firm_id', "city", "quarter")]
      
    # create price instrument 
    cereal[, i1_price := (sum(price) - price)/ (.N-1), c('firm_id', "city", "quarter")]
    
    # now do 2sls, tidy it up, make it a data.table 
    q2_p3_iv1 <- data.table(tidy(ivreg(m_u ~ mushy + sugar + price | i1_sugar + i1_mushy +  mushy + sugar , data = cereal)))
    
    q2_p3_iv1[, p.value := round(p.value,6)]
    
    
    #============#
    # ==== c ====
    #============#

    # now create second set of instroments 
    # would be ideal to write functino for this, but who has the time
    # create  sugar instroment
    # start by getting sum of sugar for firm 
    cereal[, f_sugar_sum := sum(sugar), c( "city", "quarter", "firm_id")]
    
    # get number of products by firm 
    cereal[, f_nprod := .N, c( "city", "quarter", "firm_id")]
    
    # get sum of sugar in market, subtract off sum of sugar for the firm 
    # divide by number of products minus the products for this firm that we subtracted off 
    cereal[, i2_sugar := (sum(sugar) - f_sugar_sum)/ (.N-f_nprod), c("city", "quarter")]
    
    # now do the same for the other
    cereal[, f_mushy_sum := sum(mushy), c( "city", "quarter", "firm_id")]
    cereal[, i2_mushy := (sum(mushy) - f_mushy_sum)/ (.N-f_nprod), c("city", "quarter")]
    
    # #note dont actually need this 
    # cereal[, f_price_sum := sum(price), c( "city", "quarter", "firm_id")]
    # cereal[, i2_price := (sum(price) - f_price_sum)/ (.N-f_nprod), c("city", "quarter")]
    
    # now do 2sls 
    ivreg_out <- ivreg(m_u ~ mushy + sugar + price | i2_sugar + i2_mushy + mushy + sugar , data = cereal)
    q2_p3_iv2 <- data.table(tidy(ivreg(m_u ~ mushy + sugar + price | i2_sugar + i2_mushy + mushy + sugar , data = cereal)))
    
    q2_p3_iv2[, p.value := round(p.value,6)]
    
  
    
    
#=====================#
# ==== Question 3 ====
#=====================#
    
    # make guess for this to work on code 
    sd_mush <- .1 
    sd_sug <- .2 

    # clear variables we don't need 
    cereal <- cereal[, c(1:10, 12,13), with = FALSE]
    
    #===============================#
    # ====  ====
    #===============================#
    # create a market variale 
    cereal[, mkt := paste0(city, "_", quarter)]
    
    # redo s0 
    cereal[, s0 := 1-sum(share), c("mkt")]
    
    # defint some variables 
    # this just gives the code some generalizability I guess 
    share.fld =     "share"
    prod.id.fld =   "product_id"
    #note they don't have quarter but I think we should 
    mkt.id.fld =    "mkt"
    prc.fld =       "price"
    x.var.flds =    c("sugar",
                      "mushy")
    
    # order data and get nrows 
    cereal <- setorder(cereal, "city", "product_id")
    JM <- nrow(cereal)
    
    # make some matrix 
    X <- as.matrix(cereal[, c(x.var.flds), with = FALSE])
    K <- ncol(X)

    #market object
    mkt.id <- cereal[, get(mkt.id.fld)]
    
    #shares object
    s.jm <- as.vector(cereal[, get(share.fld)]);
    
    # get s0 object 
    s.j0 <- cereal[, s0 ]

    ## Matrix of individuals' characteristics ##
    #number of simulated consumers
    n.sim = 100
    
    #Standard normal distribution draws, one for each characteristic in X
    #columns are simulated consumers, rows are variables in X (including constant and price)
    v = matrix(rnorm(K * n.sim), nrow = K, ncol = n.sim)

    
    temp <-   cereal[, list( sd_mush*mushy, sd_sug*sugar) ]
    temp <- as.matrix(temp)
    
    mu.in <- temp%*%v
    dim(mu.in)
    
    # get delta guess 
    #note doesn't need to come from cereal data.table, this is just an initial guess 
    delta.in <- cereal[, delta_r]
    
    ind_sh <- function(delta.in, mu.in){
      # This function computes the "individual" probabilities of choosing each brand
      # Requires global variables: mkt.id, X, v
      numer <- exp(mu.in) * matrix(rep(exp(delta.in), n.sim), ncol = n.sim)
      
      denom <- as.matrix(do.call("rbind", lapply(mkt.id, function(tt){
        1 + colSums(numer[mkt.id %in% tt, ])
        
      })))
      return(numer / denom);  
    }
    
    
    blp_inner <- function(delta.in, mu.in) {
      # Computes a single update of the BLP (1995) contraction mapping.
      # of market level predicted shares.
      # This single-update function is required by SQUAREM, see Varadhan and
      # Roland (SJS, 2008), and Roland and Varadhan (ANM, 2005)
      # INPUT
      #   delta.in : current value of delta vector
      #   mu.in: current mu matrix
      # Requires global variables: s.jm
      # OUTPUT
      #   delta.out : delta vector that equates observed with predicted market shares
      pred.s <- rowMeans(ind_sh(delta.in, mu.in));
      delta.out <- delta.in + log(s.jm) - log(pred.s)
      return(delta.out)
    }
    

    
    

#===============================#
# ==== save output to latex ====
#===============================#
  
  if(opt_save){
    
    
    print(xtable(probit_res, type = "latex"), 
          file = paste0(f_out, "q1_p1.tex"),
          include.rownames = FALSE,
          floating = FALSE)
    
    
    print(xtable(logit_res, type = "latex"), 
          file = paste0(f_out, "q1_p3.tex"),
          include.rownames = FALSE,
          floating = FALSE)
    
    
    print(xtable(p6_res_tab, type = "latex"), 
          file = paste0(f_out, "q1_p6.tex"),
          include.rownames = FALSE,
          floating = FALSE)
    
    print(xtable(q2_p3_ols, type = "latex"), 
          file = paste0(f_out, "q2_p3a.tex"),
          include.rownames = FALSE,
          floating = FALSE)
    
    print(xtable(q2_p3_iv1, type = "latex"), 
          file = paste0(f_out, "q2_p3b.tex"),
          include.rownames = FALSE,
          floating = FALSE)
    
    print(xtable(q2_p3_iv2, type = "latex"), 
          file = paste0(f_out, "q2_p3c.tex"),
          include.rownames = FALSE,
          floating = FALSE)
    
    
  
  }
    
    
    
    
    
    
    
    
    

   