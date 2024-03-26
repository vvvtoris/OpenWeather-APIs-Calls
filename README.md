# OpenWeather-APIs-Calls

# Get forecast data for a given city list
get_weather_forecast_by_cities <- function(city_names) {
  city <- character()
  weather <- character()
  visibility <- numeric()
  temp <- numeric()
  temp_min <- numeric()
  temp_max <- numeric()
  pressure <- numeric()
  humidity <- numeric()
  wind_speed <- numeric()
  wind_deg <- numeric()
  forecast_datetime <- character()
  season <- character()
  
  df <- data.frame()
  
  for (city_name in city_names) {
    # Forecast API URL
    forecast_url <- 'https://api.openweathermap.org/data/2.5/forecast'
    # Create query parameters
    forecast_query <- list(q = city_name, appid = "f49cf23933137bdcbb518ae84e96207b", units = "metric")
    # Make HTTP GET call for the given city
    forecast_response <- GET(forecast_url, forecast_query)
    # Note that the 5-day forecast JSON result is a list of lists. You can print the reponse to check the results
    #results <- json_list$list
    forecast_json_list <- content(forecast_response, as = "parsed")
    results <- forecast_json_list$list
    
    # Loop the json result
    for(result in results) {
      city <- c(city, city_name)
      weather <- c(weather, result$weather[[1]]$main)
      visibility <- c(visibility, result$visibility)
      temp <- c(temp, result$main$temp)
      temp_min <- c(temp_min, result$main$temp_min)
      temp_max <- c(temp_max, result$main$temp_max)
      pressure <- c(pressure, result$main$pressure)
      humidity <- c(humidity, result$main$humidity)
      wind_speed <- c(wind_speed, result$wind$speed)
      wind_deg <- c(wind_deg, result$wind$deg)
      forecast_datetime <- c(forecast_datetime, result$dt_txt)
      season <- c(season, "Spring")
    }
  }
  
  # Add the R Lists into a data frame
  df <- data.frame(city = city, 
                   weather = weather,
                   visibility = visibility,
                   temp = temp,
                   temp_min = temp_min,
                   temp_max = temp_max,
                   pressure = pressure,
                   humidity = humidity,
                   wind_speed = wind_speed,
                   wind_deg = wind_deg,
                   forecast_datetime = forecast_datetime,
                   season = season)
  
  # Return a data frame
  return(df)
}


cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
cities_weather_df <- get_weather_forecast_by_cities("Paris")
print(cities_weather_df)


