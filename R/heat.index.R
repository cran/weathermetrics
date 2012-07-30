heat.index <-
function (t = NA, dp = c(), rh = c(), temperature.metric = "fahrenheit", 
    output.metric = NULL, round = 0) 
{
  if(length(output.metric) == 0){
    output.metric <- temperature.metric
  }
    if (length(dp) == 0 & length(rh) == 0) {
        stop("You must give values for either dew point temperature ('dp') or relative humidity ('rh').")
    }
    else if (length(dp) > 0 & length(rh) > 0) {
        stop("You can give values for either dew point temperature ('dp') or relative humidity ('rh'), but you cannot specify both to this function.")
    }
    if (length(dp) != length(t) & length(rh) != length(t)) {
        stop("The vectors for temperature ('t') and moisture (either relative humidity, 'rh', or dew point temperature, 'dp') must be the same length.")
    }
    if (length(dp) > length(rh)) {
        rh <- dewpoint.to.humidity(t = t, dp = dp, temperature.metric = temperature.metric)
    }
    else if (length(rh[!is.na(rh) & (rh > 100 | rh < 0)]) > 0) {
        rh[!is.na(rh) & (rh > 100 | rh < 0)] <- NA
        warning("There were observations with an impossible values for relative humidity (below 0% or above 100%). For these observations, heat index was set to NA.")
    }
    if (temperature.metric == "celsius") {
        t <- celsius.to.fahrenheit(t)
    }
    hi <- mapply(heat.index.algorithm, t = t, rh = rh)
    if (output.metric == "celsius") {
        hi <- fahrenheit.to.celsius(hi)
    }
  hi <- round(hi, digits = round)
    return(hi)
}
