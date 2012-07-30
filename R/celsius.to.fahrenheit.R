celsius.to.fahrenheit <-
function (T.celsius, round = 2) 
{
    T.fahrenheit <- (9/5) * T.celsius + 32
    T.fahrenheit <- round(T.fahrenheit, digits = round)
    return(T.fahrenheit)
}
