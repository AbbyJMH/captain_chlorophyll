library(tidyverse)

our_site <- "BARC"

# Download aquatics targets data
url <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz"
aquatics_targets <- read_csv(url, show_col_types = FALSE) #download data from NEON for all aquatic (?seems to be missing a couple?) sites

# Pull just the data from study site
aquatics_targets <- aquatics_targets |> 
                      filter(site_id == our_site)





# Define which weather variables we are interested in
weather_variables <- c("surface_downwelling_longwave_flux_in_air",
                      "surface_downwelling_shortwave_flux_in_air",
                      "precipitation_flux")

# Download historical weather data
historical_weather <- neon4cast::noaa_stage3()
historical_weather <- historical_weather |> 
                        filter(site_id == our_site,
                                      variable %in% weather_variables) |>
                        collect()
# Aggregate to daily mean
historical_weather <- historical_weather |>
                        mutate(datetime = as.Date(datetime)) |>
                          group_by(datetime, site_id, variable) |> 
                          summarise(daily_mean = mean(prediction)) |>
                          ungroup()


                          
# Download forecasted weather data
yest_date <- Sys.Date() - lubridate::days(1) # Need to use yesterdays forecast (today's not available yet)
weather_forecast <- neon4cast::noaa_stage2(start_date = yest_date)
weather_forecast <- weather_forecast |> 
                      filter(datetime >= Sys.Date(),
                             site_id == our_site,
                             variable %in% weather_variables) |>
                      collect()
# Aggregate to daily mean
weather_forecast <- weather_forecast |>
                      mutate(datetime = as.Date(datetime)) |>
                      group_by(datetime, site_id, variable) |> 
                      summarise(daily_mean = mean(prediction)) |>
                      ungroup()
