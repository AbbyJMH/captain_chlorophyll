library(tidyverse)
library(neonUtilities) # need this so you can reference NEON data products

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
                      "precipitation_flux",
                      "air_temperature")

# Download historical weather data
historical_weather <- neon4cast::noaa_stage3()

# Find daily average short and longwave flux
historical_weather_rad <- historical_weather |> 
                            filter(site_id == our_site,
                                      variable %in% weather_variables[1:2]) |>
                            collect()
# Aggregate to daily mean
historical_weather_rad <- historical_weather_rad |>
                            mutate(datetime = as.Date(datetime)) |>
                              group_by(datetime, site_id, variable) |> 
                              summarise(daily_val = mean(prediction)) |>
                              ungroup()

# Find daily sum of precipitation flux
historical_weather_precip <- historical_weather |> 
                              filter(site_id == our_site,
                                     variable %in% weather_variables[3]) |>
                              collect()

# Find to daily sum
historical_weather_precip <- historical_weather_precip |>
  mutate(datetime = as.Date(datetime)) |>
  group_by(datetime, site_id, variable) |> 
  summarise(daily_val = sum(prediction)) |>
  ungroup()

# Find daily mean of air temperature
historical_weather_temp <- historical_weather |> 
  filter(site_id == our_site,
         variable %in% weather_variables[4]) |>
  collect()

# Aggregate to daily mean
historical_weather_temp <- historical_weather_temp |>
  mutate(datetime = as.Date(datetime)) |>
  group_by(datetime, site_id, variable) |> 
  summarise(daily_val = mean(prediction)) |>
  ungroup()



# Combine into one data from
historical_weather <- rbind(historical_weather_rad, historical_weather_precip, historical_weather_temp)


# Loading the data from NEON
our_site <- "BARC"  # Site of interest
data_product_id <- "DP1.20288.001"  # NEON Water Quality Data
water_quality_data <- loadByProduct(dpID = data_product_id, 
                                    site = our_site, 
                                    check.size = FALSE)  # Set FALSE to skip size confirmation

# Specifically looking at instantaneous measurements
waq_data <- water_quality_data$waq_instantaneous

# Filtering for just pH and turbidity
waq_filtered <- waq_data |> 
                 filter(siteID == our_site) |> 
                 select(siteID, startDateTime, pH, turbidity) |> 
                 mutate(date = as.Date(startDateTime)) |> 
                 group_by(date, siteID) |> 
                 summarise(daily_pH = mean(pH, na.rm = TRUE),
                           daily_turbidity = mean(turbidity, na.rm = TRUE)) |> 
                 ungroup()



##############################################
## Download and save forecast ensemble as well
## Note: Only need precipitation forecast

# Download forecasted weather data
yest_date <- Sys.Date() - lubridate::days(1) # Need to use yesterdays forecast (today's not available yet)
weather_forecast <- neon4cast::noaa_stage2(start_date = yest_date)

# filter down to just our site and just precipitation
weather_forecast <- weather_forecast |>
  filter(datetime >= Sys.Date(),
         site_id == our_site,
         variable == "precipitation_flux") |>
  collect()


precip_ens_forecast <- weather_forecast %>%
  mutate(date = as_date(datetime)) %>%  # convert to date
  group_by(date, parameter) %>%
  summarise(daily_avg = sum(prediction, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = parameter, values_from = daily_avg, names_prefix = "ens_") %>%
  rowwise() %>%
  mutate(ens_mean = mean(c_across(starts_with("ens_")), na.rm = TRUE)) %>%
  ungroup()


# Separately save weather forecast data to an .RData file
save(precip_ens_forecast, file = "precip_ens_forecast.RData")
