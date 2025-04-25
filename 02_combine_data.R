
# Remove pH outliers
waq_filtered_clean <- waq_filtered |>
  filter(daily_pH >= 0, daily_pH <= 14)


# Step 1: Pivot aquatics_targets to separate temperature, chla, and oxygen into their own columns
aquatics_targets_clean <- aquatics_targets %>%
  pivot_wider(
    names_from = variable,  # Use the variable.x column to create new columns
    values_from = observation # Use the observation column for the values
  )

# Step 2: Pivot historical_weather to separate precipitation_flux, longwave, shortwave, and air temperature into their own columns
historical_weather_clean <- historical_weather %>%
  pivot_wider(
    names_from = variable,  # Use the variable.y column to create new columns
    values_from = daily_val  # Use the daily_val column for the values
  )

# Step 3: Merge aquatics_targets_clean, waq_filtered, and historical_weather_clean
merged_data <- aquatics_targets_clean %>%
  full_join(waq_filtered_clean, by = c("datetime" = "date")) %>%
  full_join(historical_weather_clean, by = "datetime") %>%
  arrange(datetime)

# Step 4: Select and clean up the final columns
cleaned_data <- merged_data %>%
  select(
    datetime,
    oxygen,                     # From aquatics_targets
    chla,                       # From aquatics_targets
    daily_pH,                   # From waq_filtered
    daily_turbidity,            # From waq_filtered
    temperature,                # From aquatics_targets
    precipitation_flux,         # From historical_weather
    surface_downwelling_longwave_flux_in_air, # From historical_weather
    surface_downwelling_shortwave_flux_in_air, # From historical_weather
    air_temperature             # From historical_weather
  )


##########################################################################
## Making sure all the dates are correct

# Get the last date for which chla is available
last_chla_day <- tail(as_date(aquatics_targets_clean$datetime), 1)

if (last_chla_day == Sys.Date()-1) {
  # If this is yesterday, will fit model up through yesterday and start forecast on today's date
  
  # If the weather data has included partial data for today, remove that row so don't fit model through today
  cleaned_data <- cleaned_data %>%
    filter(as_date(datetime) != Sys.Date())
} else if (last_chla_day == Sys.Date()-2) {
  # If this is two days ago, will fit model up through 2 days ago and start forecast on yesterday

  
  # For meterological drivers, will use the observed value of that driver to make the forecast on yesterday's date
  yest_val <- cleaned_data$precipitation_flux[which(as_date(cleaned_data$datetime) == Sys.Date()-1)]
  
  ensemble_names <- c(paste0("ens_", 0:30), "ens_mean")
  new_row <- tibble(
    date = as_date(Sys.Date()-1),
    !!!setNames(as.list(rep(yest_val, length(ensemble_names))), ensemble_names)
  )
  
  precip_ens_forecast <- bind_rows(precip_ens_forecast, new_row)
  precip_ens_forecast <- precip_ens_forecast %>%
    arrange(date)

  # remove rows from cleaned data with any meterological data from yesterday and today
  cleaned_data <- cleaned_data %>%
    filter(!as_date(datetime) %in% c(Sys.Date(), Sys.Date()-1))
  
} else {
  stop("Chlorophyll-a data not up to date. Check data before proceeding with forecast.")
}



# Save the cleaned data to a RData file
save(cleaned_data, file = "cleaned_data.RData")

# Separately save weather forecast data to an .RData file
save(precip_ens_forecast, file = "precip_ens_forecast.RData")





