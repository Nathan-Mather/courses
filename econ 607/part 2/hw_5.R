#==============================#
# ==== hw 5 macro 607 pt 2 ====
#==============================#
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")


library(data.table)
library(ggplot2)

#====================#
# ==== set parms ====
#====================#

B <- .99
k <- 0.023
theta <- 1/.16
phi_pi <- 1.5
phi_y <- .5
rho <- .9
eps <- 0

gamma_y1 <- -1/(theta*(1-rho*eps) + (phi_pi-rho*eps)*k/(1-B*rho*eps) + phi_y)
gamma_pi1 <- gamma_y1*k/(1-B*rho*eps)
gamma_y
gamma_pi

#================#
# ==== shock ====
#================#

# set initial eps for a shock 
eps <- .01

# start list for ouput
out_list <- list()

for( i in 1:50){
  
  
  gamma_y <- -1/(theta*(1-rho*eps) + (phi_pi-rho*eps)*k/(1-B*rho*eps) + phi_y)
  gamma_pi <- gamma_y*k/(1-B*rho*eps)
  # print(gamma_y)
  print(gamma_pi)
  
  # update eps
  eps <- rho*eps
  
  # put output in list 
  out_list[[i]] <- data.table(gamma_y = gamma_y, gamma_pi = gamma_pi)
  
}

# stack data
out_dt <- rbindlist(out_list)

# center these at zero 
out_dt[, gamma_y := gamma_y - gamma_y1]
out_dt[, gamma_pi := gamma_pi - gamma_pi1]

out_dt[, time := 1:nrow(out_dt)]

#=========================#
# ==== plot that shizz ====
#=========================#

plot_attributes <- theme(plot.background    = element_rect(fill  = "white"),
                         panel.grid.major.x = element_line(color = "gray90"), 
                         panel.grid.minor   = element_blank(),
                         panel.background   = element_rect(fill  = "white", colour = "black") , 
                         panel.grid.major.y = element_line(color = "gray90"),
                         text               = element_text(size  = 14),
                         plot.title         = element_text( colour = "#0B6357",face = "bold", size = 18),
                         legend.position = "none")
# create plots.
plot_1 <-  ggplot(data = out_dt, aes(x = time, y = gamma_y)) + geom_line()

# add titles 
plot_1 <- plot_1 + labs(x = "Time", y = "Output Gap",  title = "Output Gap Over Time") + plot_attributes
print(plot_1)


# create plots.
plot_2 <-  ggplot(data = out_dt, aes(x = time, y = gamma_pi)) + geom_line()

# add titles 
plot_2 <- plot_2 + labs(x = "Time", y = "Inflation",  title = "Inflation Over Time") + plot_attributes
print(plot_2)

#===============================#
# ==== plug in other values ====-
#===============================#

#note this is some ugly coding. Not proud of what is about to happen

# plug in values they asked for and compare, then return to normal
B <- .9

gamma_y <- -1/(theta*(1-rho*eps) + (phi_pi-rho*eps)*k/(1-B*rho*eps) + phi_y)
gamma_pi <- gamma_y*k/(1-B*rho*eps)
print("change in output: new - old")
gamma_y - gamma_y1

print("change in inflation: new - old")
gamma_pi -gamma_pi1

B <- .99

theta <- 1/.5

gamma_y <- -1/(theta*(1-rho*eps) + (phi_pi-rho*eps)*k/(1-B*rho*eps) + phi_y)
gamma_pi <- gamma_y*k/(1-B*rho*eps)
print("change in output: new - old")
gamma_y - gamma_y1

print("change in inflation: new - old")
gamma_pi -gamma_pi1

theta <- 1/.16

k <- 0.033

gamma_y <- -1/(theta*(1-rho*eps) + (phi_pi-rho*eps)*k/(1-B*rho*eps) + phi_y)
gamma_pi <- gamma_y*k/(1-B*rho*eps)
print("change in output: new - old")
gamma_y - gamma_y1

print("change in inflation: new - old")
gamma_pi -gamma_pi1

k <- 0.023

phi_pi <- 1

gamma_y <- -1/(theta*(1-rho*eps) + (phi_pi-rho*eps)*k/(1-B*rho*eps) + phi_y)
gamma_pi <- gamma_y*k/(1-B*rho*eps)
print("change in output: new - old")
gamma_y - gamma_y1

print("change in inflation: new - old")
gamma_pi -gamma_pi1

phi_pi <- 1.5

phi_y <- 1.5

gamma_y <- -1/(theta*(1-rho*eps) + (phi_pi-rho*eps)*k/(1-B*rho*eps) + phi_y)
gamma_pi <- gamma_y*k/(1-B*rho*eps)
print("change in output: new - old")
gamma_y - gamma_y1

print("change in inflation: new - old")
gamma_pi -gamma_pi1

phi_y <- .5



#=====================#
# ==== save sturf ====
#=====================#
ggsave(paste0("C:/Users/Nmath_000/Documents/MI_school/macro 607/part 2/plots/pset5_ir_y.png"),  plot_1, height = 6, width = 9)
ggsave(paste0("C:/Users/Nmath_000/Documents/MI_school/macro 607/part 2/plots/pset5_ir_inf.png"),  plot_2, height = 6, width = 9)

