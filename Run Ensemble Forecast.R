#sample rows from mcmc chain to run foreward in time
Nmc = 1000         ## set number of Monte Carlo draws
prow = sample.int(nrow(as.matrix(params)), Nmc, replace = TRUE)

#initialize variables
N <- matrix(NA,Nmc,30)  ## storage
predict_matrix <- as.matrix(ef.out.combined$predict)
Nprev <- predict_matrix[prow, ncol(predict_matrix)]## initialize

#set parameters
param_matrix <- as.matrix(params)
Betatemp <- param_matrix[prow, "betatemp"]
Betaprecip <- param_matrix[prow, "betaprecip"]
BetaX <- param_matrix[prow, "betaX"]
Betaintercept <- param_matrix[prow, "betaIntercept"]
tau_add <- param_matrix[prow, "tau_add"]

#find driver values
weather_forecast_cleaned <- weather_forecast %>%
  pivot_wider(
    names_from = variable,  # Use the variable.y column to create new columns
    values_from = daily_val  # Use the daily_val column for the values
  )
precipitation <- weather_forecast_cleaned$precipitation_flux
temperature <- weather_forecast_cleaned$precipitation_flux

#Run ensemble
for(t in 1:30){
  mu = Nprev + Betaintercept + Betatemp*temperature[t] + Betaprecip*precipitation[t] + BetaX*Nprev   ## calculate mean
  N[,t] <- rnorm(Nmc,mu,tau_add)                         ## predict next step
  Nprev <- N[,t]                                  ## update IC
}
## graph the ensemble
time = 1:30

forecast_chla <- for(t in 1:30){
  mu = Nprev + Betaintercept + Betatemp*temperature[t] + Betaprecip*precipitation[t] + BetaX*Nprev   ## calculate mean
  N[,t] <- rnorm(Nmc,mu,tau_add)                         ## predict next step
  Nprev <- N[,t] 
}
plot.run(time, forcast_chla, col = 'green', lwd = 3)
