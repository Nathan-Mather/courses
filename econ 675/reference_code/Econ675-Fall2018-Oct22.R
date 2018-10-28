###################################################################
# ECON 675, Assignment 3
# Fall 2018
# University of Michigan
# Latest update: Oct 22, 2018
###################################################################

rm(list=ls(all=TRUE))
library(foreign); library(MASS); library(Hmisc)
library(boot)
library(gmm)

# load the data
pisofirme <- data.frame(p_dt) 
complete  <- complete.cases(pisofirme[, 5:27])
pisofirme <- pisofirme[complete, ] 
# s_i: non-missing indicator
pisofirme$nmissing <- 1 - pisofirme$dmissing

###################################################################
# Q 2.2 MCAR
###################################################################
# GMM moment condition: logistic
g_logistic <- function(theta, data) {
  a <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$dpisofirme
  b <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$S_age
  c <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$S_HHpeople
  d <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    log(1+data$S_incomepc)
  cbind(a, b, c, d)
}

# logistic bootstrap
boot.T_logistic <- function(boot.data, ind) {
  gmm(g_logistic, boot.data[ind, ], t0=c(0,0,0,0), wmatrix="ident", vcov="iid")$coef
}
ptm <- proc.time()
set.seed(123)
temp <- boot(data=pisofirme[pisofirme$nmissing==1, ], R=499, statistic = boot.T_logistic, stype = "i")
proc.time() - ptm
table3 <- matrix(NA, ncol=6, nrow=4)
for (i in 1:4) {
  table3[i, 1] <- temp$t0[i]
  table3[i, 2] <- sd(temp$t[, i])
  table3[i, 3] <- table3[i, 1] / table3[i, 2]
  table3[i, 4] <- 2 * max( mean(temp$t[, i]-temp$t0[i]>=abs(temp$t0[i])), mean(temp$t[, i]-temp$t0[i]<=-1*abs(temp$t0[i])) )
  table3[i, 5] <- 2 * temp$t0[i] - quantile(temp$t[, i], 0.975)
  table3[i, 6] <- 2 * temp$t0[i] - quantile(temp$t[, i], 0.025)
}
filename <- paste("logistic_boot_MCAR.txt")
latex(round(table3, 4),
      file=paste0(filename),append=FALSE,table.env=FALSE,center="none",title="",
      n.cgroup=c(1, 1, 1, 1, 2),
      cgroup=c("Estimate", "Std.Error", "t", "p-value", "CI"),
      colheads=rep("", 6),
      n.rgroup=c(4), 
      rgroup=c("covariates"),
      rowname=c("dpisofirme", "S\\_age", "S\\_HHpeople", "log(S\\_incomepc+1)") 
)

# GMM moment condition: probit
g_probit <- function(theta, data) {
  a <- (data$danemia - pnorm(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$dpisofirme * data$weights
  b <- (data$danemia - pnorm(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$S_age * data$weights
  c <- (data$danemia - pnorm(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$S_HHpeople * data$weights
  d <- (data$danemia - pnorm(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    log(1+data$S_incomepc) * data$weights
  cbind(a, b, c, d)
}

# probit bootstrap
boot.T_probit <- function(boot.data, ind) {
  data_temp <- boot.data[ind, ]
  fitted <- (glm(danemia ~ dpisofirme + S_age + S_HHpeople +I(log(S_incomepc+1)) - 1, 
                 data = data_temp, family = binomial(link = "probit")))$fitted
  dens <- dnorm(qnorm(fitted))
  weights <- dens / (fitted * (1-fitted))
  data_temp$weights <- weights
  gmm(g_probit, data_temp, t0=c(0,0,0,0), wmatrix="ident", vcov="iid")$coef
}
ptm <- proc.time()
set.seed(123)
temp <- boot(data=pisofirme[pisofirme$nmissing==1, ], R=499, statistic = boot.T_probit, stype = "i")
proc.time() - ptm
table4 <- matrix(NA, ncol=6, nrow=4)
for (i in 1:4) {
  table4[i, 1] <- temp$t0[i]
  table4[i, 2] <- sd(temp$t[, i])
  table4[i, 3] <- table4[i, 1] / table4[i, 2]
  table4[i, 4] <- 2 * max( mean(temp$t[, i]-temp$t0[i]>=abs(temp$t0[i])), mean(temp$t[, i]-temp$t0[i]<=-1*abs(temp$t0[i])) )
  table4[i, 5] <- 2 * temp$t0[i] - quantile(temp$t[, i], 0.975)
  table4[i, 6] <- 2 * temp$t0[i] - quantile(temp$t[, i], 0.025)
}
filename <- paste("probit_boot_MCAR.txt")
latex(round(table4, 4),
      file=paste0(filename),append=FALSE,table.env=FALSE,center="none",title="",
      n.cgroup=c(1, 1, 1, 1, 2),
      cgroup=c("Estimate", "Std.Error", "t", "p-value", "CI"),
      colheads=rep("", 6),
      n.rgroup=c(4), 
      rgroup=c("covariates"),
      rowname=c("dpisofirme", "S\\_age", "S\\_HHpeople", "log(S\\_incomepc+1)") 
)

###################################################################
# Q 2.3 MAR
###################################################################
# GMM moment condition
g_MAR <- function(theta, data) {
  data <- data[data$nmissing==1, ]
  a <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$dpisofirme * data$weights
  b <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$S_age * data$weights
  c <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$S_HHpeople * data$weights
  d <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    log(1+data$S_incomepc) * data$weights
  cbind(a, b, c, d)
}

# logistic bootstrap
boot.T_MAR <- function(boot.data, ind) {
  data.temp <- boot.data[ind, ]
  fitted <- glm(nmissing ~ dpisofirme + S_age + S_HHpeople +I(log(S_incomepc+1)) - 1, 
                data = data.temp, 
                family = binomial(link = "logit"))$fitted
  data.temp$weights <- 1 / fitted
  gmm(g_MAR, data.temp, t0=c(0,0,0,0), wmatrix="ident", vcov="iid")$coef
}

ptm <- proc.time()
set.seed(123)
temp <- boot(data=pisofirme, R=499, statistic = boot.T_MAR, stype = "i")
proc.time() - ptm
table5 <- matrix(NA, ncol=6, nrow=4)
for (i in 1:4) {
  table5[i, 1] <- temp$t0[i]
  table5[i, 2] <- sd(temp$t[, i])
  table5[i, 3] <- table5[i, 1] / table5[i, 2]
  table5[i, 4] <- 2 * max( mean(temp$t[, i]-temp$t0[i]>=abs(temp$t0[i])), mean(temp$t[, i]-temp$t0[i]<=-1*abs(temp$t0[i])) )
  table5[i, 5] <- 2 * temp$t0[i] - quantile(temp$t[, i], 0.975)
  table5[i, 6] <- 2 * temp$t0[i] - quantile(temp$t[, i], 0.025)
}
filename <- paste("logistic_boot_MAR.txt")
latex(round(table5, 4),
      file=paste0(filename),append=FALSE,table.env=FALSE,center="none",title="",
      n.cgroup=c(1, 1, 1, 1, 2),
      cgroup=c("Estimate", "Std.Error", "t", "p-value", "CI"),
      colheads=rep("", 6),
      n.rgroup=c(4), 
      rgroup=c("covariates"),
      rowname=c("dpisofirme", "S\\_age", "S\\_HHpeople", "log(S\\_incomepc+1)") 
)


# GMM moment condition with trimming
g_MAR2 <- function(theta, data) {
  data <- data[data$nmissing==1 & data$weights<=1/0.1, ]
  a <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$dpisofirme * data$weights
  b <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$S_age * data$weights
  c <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    data$S_HHpeople * data$weights
  d <- (data$danemia - plogis(theta[1]*data$dpisofirme + theta[2]*data$S_age + theta[3]*data$S_HHpeople + theta[4]*log(1+data$S_incomepc))) * 
    log(1+data$S_incomepc) * data$weights
  cbind(a, b, c, d)
}

# logistic bootstrap
boot.T_MAR2 <- function(boot.data, ind) {
  data.temp <- boot.data[ind, ]
  fitted <- glm(nmissing ~ dpisofirme + S_age + S_HHpeople +I(log(S_incomepc+1)) - 1, 
                data = data.temp, 
                family = binomial(link = "logit"))$fitted
  data.temp$weights <- 1 / fitted
  gmm(g_MAR2, data.temp, t0=c(0,0,0,0), wmatrix="ident", vcov="iid")$coef
}

ptm <- proc.time()
set.seed(123)
temp <- boot(data=pisofirme, R=499, statistic = boot.T_MAR2, stype = "i")
proc.time() - ptm
table6 <- matrix(NA, ncol=6, nrow=4)
for (i in 1:4) {
  table6[i, 1] <- temp$t0[i]
  table6[i, 2] <- sd(temp$t[, i])
  table6[i, 3] <- table6[i, 1] / table6[i, 2]
  table6[i, 4] <- 2 * max( mean(temp$t[, i]-temp$t0[i]>=abs(temp$t0[i])), mean(temp$t[, i]-temp$t0[i]<=-1*abs(temp$t0[i])) )
  table6[i, 5] <- 2 * temp$t0[i] - quantile(temp$t[, i], 0.975)
  table6[i, 6] <- 2 * temp$t0[i] - quantile(temp$t[, i], 0.025)
}
filename <- paste("logistic_boot_MAR2.txt")
latex(round(table6, 4),
      file=paste0(filename),append=FALSE,table.env=FALSE,center="none",title="",
      n.cgroup=c(1, 1, 1, 1, 2),
      cgroup=c("Estimate", "Std.Error", "t", "p-value", "CI"),
      colheads=rep("", 6),
      n.rgroup=c(4), 
      rgroup=c("covariates"),
      rowname=c("dpisofirme", "S\\_age", "S\\_HHpeople", "log(S\\_incomepc+1)") 
)