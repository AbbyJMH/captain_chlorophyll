rwalk_model = "
model{
  ###Process Model
  for (t in 2:n){
    x[t] ~ dnorm(x[t-1], tau_add)
  }
  
  ###Observation Model
  for (t in 1:n){
    y[t] ~ dnorm(x[t], tau_obs)
  }
  
  ###Priors
  x[1] ~ dnorm(x_ic, tau_ic)
  tau_obs ~ dgamma(a_obs, r_obs)
  tau_add ~ dgamma(a_add, r_add)
}"

rwalk.inputs = list(y = cleaned_data$chla,
              n = length(cleaned_data$chla),
              x_ic=1,tau_ic=0.01,      ## initial condition prior
              a_obs=1,r_obs=1,           ## obs error prior
              a_add=1,r_add=1)            ## process error prior
rwalk.nchain = 3
rwalk.init <- list()
for(i in 1:rwalk.nchain){
  rwalk.init[[i]] <- list(tau_add=rgamma(1,1,1),  
                    tau_obs=rgamma(1,1,1))
}
rwalk.j.model <- jags.model(file = textConnection(rwalk_model),
                      data = rwalk.inputs,
                      inits = rwalk.init,
                      n.chains = rwalk.nchain)
rwalk.jags.out   <- coda.samples (model = rwalk.j.model,
                            variable.names = c("tau_add","tau_obs","x"),
                            n.iter = 3000)
rwalk.out <- as.matrix(rwalk.jags.out)