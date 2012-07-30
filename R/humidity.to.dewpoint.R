humidity.to.dewpoint <-
function (t = NA, rh = NA, temperature.metric = "fahrenheit") 
{
    if (!(temperature.metric %in% c("celsius", "fahrenheit"))) {
        stop("The 'temperature.metric' option can onnly by 'celsius' or 'fahrenheit'.")
    }
    if (length(rh) != length(t)) {
        stop("The vectors for temperature('t') and relative humidity ('rh') must have the same length.")
    }
    if (length(rh[!is.na(rh) & (rh < 0 | rh > 100)]) > 0) {
        rh[!is.na(rh) & (rh < 0 | rh > 100)] <- NA
        warning("For some observations, relative humidity was below 0% or above 100%. Since these values are impossible for relative humidity, dew point temperature for these observations was set to 'NA'.")
    }
    if (temperature.metric == "fahrenheit") {
        t <- fahrenheit.to.celsius(t)
    }
    dewpoint <- (rh/100)^(1/8) * (112 + (0.9 * t)) - 112 + (0.1 * 
        t)
    if (temperature.metric == "fahrenheit") {
        dewpoint <- celsius.to.fahrenheit(dewpoint)
    }
    return(dewpoint)
}
