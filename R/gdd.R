#' Growing Degree Days Function
#'
#' This function calculates growing degree days (GDD) using the average of the daily maximum
#' and minimum temperatures, a base temperature and a maximum base temperature
#'
#' @param tmax daily maximum temperature
#' @param tmin daily minimum temperature
#' @param tbase base temperature
#' @param tbase_max maximum base temperature
#' @param type either "B", "C", or "D". The default is "C". Type "B" - The heat units are calculated based on the difference between the mean daily temperature and the threshold (`tbase`). In the case when the value of `tmin` is lower than `tbase`, then it is replaced by `tbase`. Type `"C"` - same as type `"B"` and when the value of `tmax` is larger than `tbase_max`, then it is replaced by `tbase_max`. Type `"D"`- same as type `"B"` and when the value of `tmax` is larger than `tbase_max`, then no heat units are added.
#'
#' @return a numeric vector with GDD values
#'
#' @references Baskerville, G., & Emin, P. (1969). Rapid Estimation of Heat Accumulation from Maximum and Minimum Temperatures. Ecology, 50(3), 514-517. doi:10.2307/1933912
#'
#' @export
#' 
#' @seealso [base_temp()] for determining a base temperature
#'
#' @examples
#' set.seed(25)
#' df <- data.frame(tmax = runif(100, 6, 10), tmin = runif(100, 4, 6))
#'
#' gdd(tmax = df$tmax, tmin = df$tmin, tbase = 5, tbase_max = 30)
#'
#'
gdd <- function(tmax, tmin, tbase, tbase_max, type = "C") {
  
  if (!type %in% c("A", "B", "C", "D")){
    stop('The type argument must be either "A", "B", "C", or "D"', call. = FALSE)
  }
  
  # if (type == "A"){
  #   tbase = 0
  # }
  
  if (type %in% c("A", "B", "C", "D")){
    tmax <- adjust_for_tbase(tmax, tbase)
    tmin <- adjust_for_tbase(tmin, tbase)
  }
  
  if (type %in% c("C")){
    tmax <- adjust_for_tbase_max(tmax, tbase_max)
    tmin <- adjust_for_tbase_max(tmin, tbase_max)
  }
  
  if (type == "D"){
    gdd_temp <- ifelse(tmax > tbase_max, yes = 0, no = (tmax + tmin) / 2 - tbase)
  } else if (type %in% c("A", "B", "C")){
    gdd_temp <- (tmax + tmin) / 2 - tbase
  }

  return(cumsum(gdd_temp))
}

adjust_for_tbase <- function(x, tbase) {
  ifelse(test = x < tbase, yes = tbase, no = x)
}

adjust_for_tbase_max <- function(x, tbase_max) {
  ifelse(test = x > tbase_max, yes = tbase_max, no = x)
}