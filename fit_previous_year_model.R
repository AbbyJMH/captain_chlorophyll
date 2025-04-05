pyear_model = "
model{
  ###Process Model
  for (t in 366:n){
    x[t] ~ dnorm(x[t-365], tau_add)
  }
  
  ###Observation Model
  for (t in 1:n){
    y[t] ~ dnorm(x[t], tau_obs)
  }
  
  ###Priors
  for (t in 1:365){
    x[t] ~ dnorm(x_ic, tau_ic)
  }
  tau_obs ~ dgamma(a_obs, r_obs)
  tau_add ~ dgamma(a_add, r_add)
}"

pyear.inputs = list(y = cleaned_data$chla,
                    n = length(cleaned_data$chla),
                    x_ic=1,tau_ic=0.01,      ## initial condition prior
                    a_obs=1,r_obs=1,           ## obs error prior
                    a_add=1,r_add=1)            ## process error prior
pyear.nchain = 3
pyear.init <- list()
for(i in 1:pyear.nchain){
  pyear.init[[i]] <- list(tau_add=rgamma(1,1,1),  
                          tau_obs=rgamma(1,1,1))
}
pyear.j.model <- jags.model(file = textConnection(pyear_model),
                            data = pyear.inputs,
                            inits = pyear.init,
                            n.chains = pyear.nchain)
pyear.jags.out   <- coda.samples (model = pyear.j.model,
                                  variable.names = c("tau_add","tau_obs","x"),
                                  n.iter = 3000)
pyear.out <- as.matrix(pyear.jags.out)