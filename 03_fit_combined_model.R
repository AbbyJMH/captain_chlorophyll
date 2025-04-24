load("cleaned_data.RData")

# set up data object for ecoforecastR
data <- list(y = cleaned_data$chla,
             n = length(cleaned_data$chla),      ## data
             temp = cleaned_data$temperature,
             longrad = cleaned_data$surface_downwelling_longwave_flux_in_air,
             shortrad = cleaned_data$surface_downwelling_shortwave_flux_in_air,
             precip = cleaned_data$precipitation_flux,
             x_ic=1,tau_ic=100,      ## initial condition prior
             a_obs=1,r_obs=1,           ## obs error prior
             a_add=1,r_add=1            ## process error prior
)

## fit the model
model2 <- "~ 1 + X + temp + precip"
ef.out.combined <- ecoforecastR::fit_dlm(model=list(obs="y",fixed=model2, n.iter = 40000), data)

save(ef.out.combined, file = "combined_factors.RData")