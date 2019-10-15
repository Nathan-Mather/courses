#===============================================#
# ==== Inustrial Organization Problem Set 1 ====
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

# create starting values 
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
# ==== Part2 ====
#================#

probit_res$Estimate[3]*dnorm(probit_res$Estimate[1] + probit_res$Estimate[2]*mean(q1dt$x1) + probit_res$Estimate[3]*mean(q1dt$x2))

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


p6_llf <- function(th0, th1, th2, th3, th4, th5, p, sig){
  
  # define some intermediate terms 
  q1dt[, psi := (p/sig^2)*(x2 - (th3 + th4*x1 + th5*z))]
  q1dt[, tau := 1-(p^2/sig^2)]
  q1dt[, m:= (-th0 - th1*x1 - th2*x2 - psi)/(tau^.5)]  
  
  # get prob y equals one conditional on x1, x2, z, theta 
  q1dt[, p_y_1 := 1- pnorm(m)]
  
  # get pro x2 = x2i given x1 z theta
  q1dt[, p_x := dnorm((x2 - th3 - th4*x1 - th5*z)/sig)]
  
  # get log likelihoods 
  q1dt[, llf := y*log(p_y_1) + (1-y)*log(1-p_y_1) + log(p_x) - log(sig)]
  
  # sum them 
  sum_llf <- q1dt[, sum(llf)]
  
  # remove extra vars 
  q1dt[ ,`:=` (psi = NULL, tau = NULL, m = NULL, p_y_1 = NULL, p_x = NULL, llf = NULL)]
  
  #  return negative sum 
  return(-sum_llf)
}  

second_stage <- glm(y ~ x1+ x2, 
                    family = binomial(link = "probit"), 
                    data = q1dt)

first_stage <- lm(x2 ~ x1 + z, data = q1dt)

p6_start <- as.list(c(second_stage$coefficients, first_stage$coefficients, .1, 1))

names(p6_start) <- c( paste0("th", 0:5), "p", "sig")

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
# load cereal data 
    cereal <- data.table(readxl::read_excel("C:/Users/Nmath_000/Documents/MI_school/Third Year/Econ 631/ps1/cereal_data.xlsx"))
      
  #====================================#
  # ==== set up data for functions ====
  #====================================#

    #note these are data manipulations that can happen outside the outer loop. We 
    # just need to convert some things to matrices since we actually need to 
    # use matrix algebra in this question 
    
    # drop obsercations with missing instruments. Missing becasue firm only has one product 
    cereal <- cereal[!is.na(i1_sugar)]
    
    # create a single market variale. Don't have to referece two variables then 
    cereal[, mkt := paste0(city, "_", quarter)]
    
    # defint some variables 
    # I'm not crazy about doing this. I think this either needs to be written as a function 
    # or else we should just hard code the column names. This is like a weird middle ground 
    # of generalizability that isn't that helpful 
    share.fld =     "share"
    prod.id.fld =   "product_id"
    mkt.id.fld =    "mkt"
    prc.fld =       "price"
    x.var.flds =    c("sugar",
                      "mushy")
    
    # order data and get nrows 
    cereal <- setorder(cereal, "city", "product_id")
    JM <- nrow(cereal)
    
    # make matrix of controls X 
    X <- as.matrix(cereal[, c(x.var.flds), with = FALSE])
    K <- ncol(X)
    
    # put market into a vector 
    mkt.id <- cereal[, get(mkt.id.fld)]
    
    # put shares into a vector 
    s.jm <- as.vector(cereal[, get(share.fld)]);
    
    # define market share of the outside option
    #note: should this happen before or after we drop the obs with missing instruments?
    cereal[, s0 := 1-sum(share), mkt.id]

    # put shates into a vector  
    s.j0 <- cereal[, s0 ]
    
    # set number of simulated consumers
    n.sim = 100
     
    # Standard normal distribution draws, one for each characteristic in X
    # columns are simulated consumers, rows are variables in X (including constant and price)
    # as Ying pointed out we want to do this once outside the outer loop to get faster convergence 
    v = matrix(rnorm(K * n.sim), nrow = K, ncol = n.sim)
    
    # Z matrix needed for gmm function 
    Z <- as.matrix(cereal[!is.na(i1_sugar),c("sugar", "mushy", "i1_sugar", "i1_mushy"), with = FALSE])
    
    # PZ matrix needed for GMM function 
    PZ <- Z %*% solve(t(Z) %*% Z) %*% t(Z)
    
    # make weighting matrix. Also used in GMM function 
    W.inv <- diag(1, 4, 4 )
    
  #===============================#
  # ==== functions for search ====
  #===============================#

    # these are all the functions we will need for a given sd_mush sd_sug guess 
    
    # hard code estimates for variance paremter to test funcitons
    # this is what the outside loop will be optimizing over
    sd_mush_test <- .1
    sd_sug_test <- .2
    
    # get inital delta guess
    #note doesn't need to come from cereal data.table, this is just an initial guess
    delta.initial <- cereal[, m_u]
    
    # this function takes the data.table, V sims,  and the sd parameter estimates and returns 
    # the mu (individual utility) estimates
    #note I am hard coding this for mushy and sugar as the relavent variables. 
    # could write this in a more general way if we wanted and have it take a list of theta parms 
    # and a list of corresponding variabe names 
    find_mu <- function(in_data, sd_mush.in, sd_sug.in, v.in){
      
      temp <-   as.matrix(in_data[, list( sd_mush.in*mushy, sd_sug.in*sugar) ])
      
      mu.out <- temp%*%v.in
      
      return(mu.out)
    }
    
    # test it out
    mu_mat <- find_mu(cereal,sd_mush_test, sd_sug_test, v )
    
    # This computes a matrix of share estiamtes. rows are estimates for every product 
    # columns are estimates for every simulated V 
    ind_sh <- function(delta.in, mu.in, mkt.id.in){
      # This function computes the "individual" probabilities of choosing each brand
      numer <- exp(mu.in) * matrix(rep(exp(delta.in), n.sim), ncol = n.sim)
      
      denom <- as.matrix(do.call("rbind", lapply(mkt.id.in, function(tt){
        1 + colSums(numer[mkt.id.in %in% tt, ])
        
      })))
      return(numer / denom);  
    }
    
    # test it out
    sj_mat <- ind_sh(delta.initial, mu_mat, mkt.id)
    
    
    blp_inner <- function(delta.in, mu.in, s.jm.in, mkt.id.in) {
      # Computes a single update of the BLP (1995) contraction mapping.
      # of market level predicted shares.
      # This single-update function is required by SQUAREM, see Varadhan and
      # Roland (SJS, 2008), and Roland and Varadhan (ANM, 2005)
      # INPUT
      #   delta.in : current value of delta vector
      #   mu.in: current mu matrix
      # OUTPUT
      #   delta.out : delta vector that equates observed with predicted market shares
      pred.s <- rowMeans(ind_sh(delta.in, mu.in, mkt.id.in));
      delta.out <- delta.in + log(s.jm.in) - log(pred.s)
      return(delta.out)
    }
    
    # test it out
    delta_new <- blp_inner(delta.initial, mu_mat, s.jm, mkt.id)

    
    # The function to iterate the contraction mapping is genereic. WE don't need to write it
    # here is an example #note it is dependent on output of above examples 
    #note I have to pass additonal variables so that we are not pulling objects from global enviorment
      squarem.output <- squarem(par       = delta.initial, 
                                fixptfn   = blp_inner, 
                                mu.in     = mu_mat, 
                                s.jm.in   = s.jm,
                                mkt.id.in = mkt.id,
                                control   = list(trace = TRUE))
      delta <- squarem.output$par
      summary(delta.initial - delta)
    
      
    gmm_obj_f <- function(delta.in, X.in, Z.in, PZ.in, W.inv.in){
      
      # first step 
      PX.inv <- solve(t(X.in) %*% PZ.in %*% X.in)
      
      # finsih getting theta 
      theta1 <- PX.inv %*% t(X.in) %*% PZ.in %*% delta.in
      
      # get xi hat 
      xi.hat <- delta - X.in %*% theta1
      
      # get function value 
      return(t(xi.hat) %*% Z.in %*% W.inv.in %*% t(Z.in) %*% xi.hat)
      
    }
    
    # test it out 
    f <- gmm_obj_f(delta.initial, X, Z, PZ, W.inv)

    
    
    
  # #OLD CODE 
  # #===============================#
  # # ==== fgirue out functions ====
  # #===============================#
  #   #Note WILL DELETE THIS SECITON LATER 
  #   
  #   # make guess for this to work on code 
  #   sd_mush <- .1 
  #   sd_sug <- .2 
  #   
  #   # defint some variables 
  #   # this just gives the code some generalizability I guess 
  #   share.fld =     "share"
  #   prod.id.fld =   "product_id"
  #   #note they don't have quarter but I think we should 
  #   mkt.id.fld =    "mkt"
  #   prc.fld =       "price"
  #   x.var.flds =    c("sugar",
  #                     "mushy")
  #   
  #   # order data and get nrows 
  #   cereal <- setorder(cereal, "city", "product_id")
  #   JM <- nrow(cereal)
  #   
  #   # make some matrix 
  #   X <- as.matrix(cereal[, c(x.var.flds), with = FALSE])
  #   K <- ncol(X)
  # 
  #   #market object
  #   mkt.id <- cereal[, get(mkt.id.fld)]
  #   
  #   #shares object
  #   s.jm <- as.vector(cereal[, get(share.fld)]);
  #   
  #   # get s0 object 
  #   s.j0 <- cereal[, s0 ]
  # 
  #   ## Matrix of individuals' characteristics ##
  #   #number of simulated consumers
  #   n.sim = 100
  #   
  #   #Standard normal distribution draws, one for each characteristic in X
  #   #columns are simulated consumers, rows are variables in X (including constant and price)
  #   v = matrix(rnorm(K * n.sim), nrow = K, ncol = n.sim)
  # 
  #   
  #   temp <-   cereal[, list( sd_mush*mushy, sd_sug*sugar) ]
  #   temp <- as.matrix(temp)
  #   
  #   mu.in <- temp%*%v
  #   dim(mu.in)
  #   
  #   # get delta guess 
  #   #note doesn't need to come from cereal data.table, this is just an initial guess 
  #   delta.in <- cereal[, m_u]
  #   
  #   ind_sh <- function(delta.in, mu.in){
  #     # This function computes the "individual" probabilities of choosing each brand
  #     # Requires global variables: mkt.id, X, v
  #     numer <- exp(mu.in) * matrix(rep(exp(delta.in), n.sim), ncol = n.sim)
  #     
  #     denom <- as.matrix(do.call("rbind", lapply(mkt.id, function(tt){
  #       1 + colSums(numer[mkt.id %in% tt, ])
  #       
  #     })))
  #     return(numer / denom);  
  #   }
  #   
  #   
  #   blp_inner <- function(delta.in, mu.in) {
  #     # Computes a single update of the BLP (1995) contraction mapping.
  #     # of market level predicted shares.
  #     # This single-update function is required by SQUAREM, see Varadhan and
  #     # Roland (SJS, 2008), and Roland and Varadhan (ANM, 2005)
  #     # INPUT
  #     #   delta.in : current value of delta vector
  #     #   mu.in: current mu matrix
  #     # Requires global variables: s.jm
  #     # OUTPUT
  #     #   delta.out : delta vector that equates observed with predicted market shares
  #     pred.s <- rowMeans(ind_sh(delta.in, mu.in));
  #     delta.out <- delta.in + log(s.jm) - log(pred.s)
  #     return(delta.out)
  #   }
  #   
  #   
  #   # for now use matrices we made above 
  #   mu  <- mu.in
  #   delta <- delta.in
  # 
  #   print("Running SQUAREM contraction mapping")
  #   print(system.time(
  #     squarem.output <- squarem(par = delta, fixptfn = blp_inner, mu.in = mu, control = list(trace = TRUE))
  #   ));
  #   delta <- squarem.output$par
  #   
  #   print(summary(cereal[, m_u] - delta));
  #   cereal[, delta_r := delta]
  #   
  #   # 
  #   prc.iv.flds = c("i1_sugar",
  #                   "i1_mushy")
  #   
  #   str.ivreg.y <- "delta_r ~ "
  #   str.ivreg.x <- paste(x.var.flds, collapse = " + ")
  #   str.ivreg.prc <- paste(prc.fld, collapse = " + ")
  #   str.ivreg.iv <- paste(prc.iv.flds, collapse = " + ")
  #   print(fm.ivreg <- paste0(str.ivreg.y, str.ivreg.x, " + ", str.ivreg.prc, " | ", str.ivreg.x, " + ", str.ivreg.iv))
  #   
  #   # define Z matrix 
  #   Z <- as.matrix(cereal[!is.na(i1_sugar),c("sugar", "mushy", "i1_sugar", "i1_mushy"), with = FALSE])
  #   PZ <- Z %*% solve(t(Z) %*% Z) %*% t(Z)
  #   
  #   # B <- solve(crossprod(z, x))%*%(crossprod(z, y))
  #   
  #   
  # 
  #     # make weighting matrix
  #     W.inv <- diag(1, 4, 4 )
  #   
  #  
  #     # first step 
  #     PX.inv <- solve(t(X) %*% PZ %*% X)
  #     # finsih getting theta 
  #     theta1 <- PX.inv %*% t(X) %*% PZ %*% delta
  #     
  #     # get xi hat 
  #     xi.hat <- delta - X %*% theta1
  #     
  # 
  #     # come ack to this when we have these defined better 
  #     # print(beta.est <<- data.frame(beta.est = theta1, beta.se = tsls.se, sigma.est = theta2))
  # 
  #   
  #     f <- t(xi.hat) %*% Z %*% W.inv %*% t(Z) %*% xi.hat;
  #     
   
    
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
    
    
    
    
    
    
    
    
    

   
