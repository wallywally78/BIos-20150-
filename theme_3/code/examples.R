rm(list = ls())
require(dplyr)
source("fit_SIR.R")

a <- read.csv("../data/BoardingSchool.csv")
times <- a[,1]
infected <- a[,2]
N <- 763
initial.gamma <- 1/2
# Try to get a good initial beta
initial.beta <- optimize(function(x, N, infected, times, initial.gamma){
  get_fit(c(x, initial.gamma), N, infected, times)},
  c(0,1/100), N = N, infected = infected, times = times, initial.gamma = initial.gamma)$minimum
# Optimize both parameters
best_pars <- get_best_pars(initial.beta, initial.gamma, N, infected, times)
best_sol <- best_predict(best_pars$beta, best_pars$gamma, N, infected, times)
plot(times, infected)
lines(times, best_sol)


b <- read.csv("../data/Dalziel2016_data.csv")
b <- b %>% filter(is.na(cases) == FALSE)
city <- "LOS ANGELES"
#city <- "BALTIMORE"
yr <- 1910
for (yr in 1910:1940){
b1 <- b %>% filter(loc == city, year == yr, biweek < 20)
times <- b1$biweek
infected <- b1$cases
N <- max(b1$pop)
Sinf <- N - sum(infected)
S0 <- N - infected[1]
beta_over_gamma <- log(S0 / Sinf) / (N - Sinf)
best_pars <- get_best_pars(10^-5, 7, N, infected, times)
#tmp <- optimize(function(x){get_fit(c(beta_over_gamma * x, x), N, infected, times)}, interval = c(0.01,7))
#best.initial.gamma <- tmp$minimum
#best.initial.beta <- tmp$minimum * beta_over_gamma
best_sol <- best_predict(best_pars$beta_hat, best_pars$gamma_hat, N, infected, times)
plot(times, infected, main = paste(yr, best_pars))
lines(times, best_sol)
}

