
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


# Optional: Save the cleaned data to a CSV file
write.csv(cleaned_data, "cleaned_data.csv", row.names = FALSE)




