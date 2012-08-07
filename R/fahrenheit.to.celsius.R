fahrenheit.to.celsius <-
function (T.fahrenheit, round = 2) 
{
    T.celsius <- (5/9) * (T.fahrenheit - 32)
    T.celsius <- round(T.celsius, digits = round)
    return(T.celsius)
}
