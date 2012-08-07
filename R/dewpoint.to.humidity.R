dewpoint.to.humidity <-
function (dp = NA, t = NA, temperature.metric = "fahrenheit") 
{
    if (!(temperature.metric %in% c("celsius", "fahrenheit"))) {
        stop("The 'temperature.metric' option can onnly by 'celsius' or 'fahrenheit'.")
    }
    if (length(dp) != length(t)) {
        stop("The vectors for temperature('t') and dewpoint temperature ('dp') must have the same length.")
    }
    if (length(dp[dp > t & !is.na(dp) & !is.na(t)]) > 0) {
        dp[dp > t] <- NA
        warning("For some observations, dew point temperature was higher than temperature. Since dew point temperature cannot be higher than air temperature, relative humidty for these observations was set to 'NA'.")
    }
    if (temperature.metric == "fahrenheit") {
        t <- fahrenheit.to.celsius(t)
        dp <- fahrenheit.to.celsius(dp)
    }
    beta <- (112 - (0.1 * t) + dp)/(112 + (0.9 * t))
    relative.humidity <- 100 * beta^8
    return(relative.humidity)
}
