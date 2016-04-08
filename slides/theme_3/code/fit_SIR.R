require(deSolve)

de_sir <- function(time, state, params){
  with(as.list(c(state, params)), {
    dSdt <- -beta * S * I 
    dIdt <- beta * S * I - gamma * I
    dRdt <- gamma * I
    return(list(c(dSdt, dIdt, dRdt)))
  })
}

get_fit <- function(pars, N, infected, times){
  beta = pars[1]
  gamma = pars[2]
  pars <- c(beta = beta, gamma = gamma)
  yini <- c(S = N - infected[1], I = infected[1], R = 0.0)
  out <- as.data.frame(ode(yini, times, de_sir, pars))
  sum_squares <- sum(abs(out$I - infected)^2)
  #browser()
  return(sum_squares)
}

get_best_pars <- function(initial.beta, initial.gamma, N, infected, times){
  tmp <- optim(c(initial.beta, initial.gamma), fn = get_fit, N = N, infected = infected, times = times
               )
               #, method = "L-BFGS-B", lower = c(10^-8, 5), upper = c(6*10^-5, 12))#
               #, method = "SANN", control = list(maxit = 1000, temp = 1000, trace = TRUE,  REPORT = 50))
  return(list(beta_hat = tmp$par[1],
              gamma_hat = tmp$par[2]))
  
}

best_predict <- function(beta, gamma, N, infected, times){
  pars <- c(beta = beta, gamma = gamma)
  yini <- c(S = N - infected[1], I = infected[1], R = 0.0)
  out <- as.data.frame(ode(yini, times, de_sir, pars))
  return(out$I)
}