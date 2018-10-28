## ECON675: ASSIGNMENT 3
## Q1: NLS
## Anirudh Yadav 
## 10/24/2018

######################################################################
# Load packages, clear workspace
######################################################################
rm(list = ls())             #clear workspace
library(foreach)            #for looping
library(data.table)         #for data manipulation
library(Matrix)             #fast matrix calcs
library(ggplot2)            #for pretty plots
library(sandwich)           #for variance-covariance estimation 
library(xtable)             #for latex tables
library(boot)               #for bootstrapping
options(scipen = 999)       #forces R to use normal numbers instead of scientific notation


######################################################################
# Input data, create additional covariates
######################################################################

# Get Piso Firme data
data <- as.data.table(read.csv('PhD_Coursework/ECON675/HW3/pisofirme.csv'))

# Create dependent variable for logistic regression
data[,s:= 1-dmissing]

# Create income regressor
data[,log_inc:= log(S_incomepc+1)]

######################################################################
# Q9(a): Estimate logistic regression
######################################################################

# Estimate model
mylogit <- glm(s ~ S_age + S_HHpeople + log_inc, data = data, family = "binomial")

b.hat   <- as.data.table(mylogit["coefficients"])

# Get robust standard errors
V.hat       <- vcovHC(mylogit, type = "HC1")
se.hat      <- as.data.table(sqrt(diag(V.hat)))

# Compute t-stats
t.stats       <- b.hat/se.hat

# Compute p-vals
n = nrow(data)
d = 4
p = round(2*pt(abs(t.stats[[1]]),df=n-d,lower.tail=FALSE),3)

# Compute CIs
CI.lower = b.hat - qnorm(0.975)*se.hat
CI.upper = b.hat + qnorm(0.975)*se.hat

# Mash results together
results  = as.data.frame(cbind(b.hat,se.hat,t.stats,p,CI.lower,CI.upper))
colnames(results) = c("Coef.","Std. Err.","t-stat","p-val","CI.lower","CI.upper")
rownames(results) = c("Const.", "S_age","S_HHpeople","log_inc")

# Get latex table output
xtable(results,digits=3)


######################################################################
# Q9(b): Bootstrap statistics
######################################################################

# Define function for bootstrap statistic
boot.logit <- function(data, i){
  logit  <- glm(s ~ S_age + S_HHpeople + log_inc,
                data = data[i, ], family = "binomial")
  V      <- vcovHC(logit, type = "HC1")
  se     <- sqrt(diag(V.hat))
  t.boot <- (coef(logit)-coef(mylogit))/se
  
  return(t.boot)
}

# Run bootstrap replications
set.seed(123)
boot.results <- boot(data = data, R = 999, statistic = boot.logit)

# Get 0.025/0.975 quantiles from the boot t-distribution
boot.q      <- sapply(1:4, function (i) quantile(boot.results$t[,i], c(0.025, 0.975)))

# Construct 95% CIs using bootstrapped quantiles
boot.ci.lower = b.hat + t(boot.q)[,1]*se.hat
boot.ci.upper = b.hat + t(boot.q)[,2]*se.hat

# Get p-val -- I'm not sure if this is right!!!
boot.p = sapply(1:4,function(i) 1/999*sum(boot.results$t[,i]>=t.stats[i])) 

# Tabulate bootstrap results
results.b  = as.data.frame(cbind(b.hat,boot.ci.lower,boot.ci.upper,boot.p))
colnames(results.b) = c("Coef.","CI.lower","CI.upper","p-val")
rownames(results.b) = c("Const.", "S_age","S_HHpeople","log_inc")

# Get latex table output
xtable(results.b,digits=3)


######################################################################
# Q9(c): Predicted probabilities
######################################################################

b.hat = coef(mylogit)

# Subset data
X     = data[,.(S_age,S_HHpeople,log_inc)]
X[,const:= 1]
setcolorder(X,c("const","S_age","S_HHpeople","log_inc"))

# Define logistic cdf (i.e. mu function)
mu = function(u){(1+exp(-u))^(-1)}

# Construct vector of x_i'*beta.hats
XB = as.matrix(X)%*%b.hat

# Compute predicted probabilities
mu.hat = mu(XB)

X[,mu.hat:=mu.hat]

#Make plot
plot(density(mu.hat,kernel="e",bw="ucv",na.rm=TRUE),main="Kernel Density Plot of Predicted Probabilities")


