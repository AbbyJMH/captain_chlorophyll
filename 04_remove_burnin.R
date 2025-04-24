load("combined_factors.RData")

burnin = 5000

params <- window(ef.out.combined$params,start = burnin)

predict <- window(ef.out.combined$predict, start = burnin)

save(params, predict, file = "combined_factors_noburnin.RData")
