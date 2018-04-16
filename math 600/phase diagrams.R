########################################
# make some phase diagrams for math hw #
########################################

  # install and library phaseR
  install.packages("phaseR")
  library(phaseR)
  library(data.table)
  # check out this weird function 
  ??flowField
  
##########################
# create functions of DE #
##########################


  dif_a <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dX <- (-2*x + y - x^3)
      dY <- (y + x^2)
      list(c(dX, dY))
    })
  }
  
  z <- Lorenz(times, state, parameters)
  parameters <- c(a = -8/3, b = -10, c = 28)
  state <- c(X = 1, Y = 1, Z = 1)
  times <- seq(0, 100, by = 0.01)
  out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
  
  plot(out)
  
  