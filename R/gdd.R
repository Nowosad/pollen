#' Growing Degree Days Function
#' 
#' This function calculates growing degree days (GDD) using the average of the daily maximum
#' and minimum temperatures, a base temperature and a maximum base temperature
#'
#' @param tmax daily maximum temperature 
#' @param tmin daily minimum temperature
#' @param tbase base temperature  
#' @param tbase_max maximum base temperature
#' @param x data.frame object containing the data
#'
#' @return a numeric vector with GDD values
#' 
#' @keywords meteo, temperature
#'
#' @export
#' 
#' @examples
#' set.seed(25)
#' df <- data.frame(tmax=runif(100, 6, 10), tmin=runif(100, 4,6))
#' 
#' gdd(df, tmax='tmax', tmin='tmin', tbase=5, tbase_max=30)
#' 
#' 

gdd <- function(x, tmax, tmin, tbase, tbase_max){
        adjust_for_tbase <- function(x, tbase) {
                ifelse(test = x < tbase, yes = tbase, no = x)
        }
        adjust_for_tbase_max <- function(x, tbase_max) {
                ifelse(test = x > tbase_max, yes = tbase_max, no = x)
        } 
        
        x <- as.data.frame(x)

        tmax_adjusted <- adjust_for_tbase(x[, tmax], tbase)
        tmin_adjusted <- adjust_for_tbase(x[, tmin], tbase)
        
        tmax_adjusted <- adjust_for_tbase_max(tmax_adjusted, tbase_max)
        tmin_adjusted <- adjust_for_tbase_max(tmin_adjusted, tbase_max)
        
        gdd_temp <- (tmax_adjusted + tmin_adjusted)/2 - tbase
        cumsum(gdd_temp)
}