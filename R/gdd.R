#' Growing Degree Days Function
#'
#' This function calculates growing degree days (GDD) using the average of the daily maximum
#' and minimum temperatures, a base temperature and a maximum base temperature
#'
#' @param tmax daily maximum temperature
#' @param tmin daily minimum temperature
#' @param tbase base temperature
#' @param tbase_max maximum base temperature
#' @param case either "A", "B", "C", or "D". The default is "C".
#'
#' @return a numeric vector with GDD values
#'
#' @keywords meteo, temperature
#'
#' @export
#'
#' @examples
#' set.seed(25)
#' df <- data.frame(tmax = runif(100, 6, 10), tmin = runif(100, 4, 6))
#'
#' gdd(tmax = df$tmax, tmin = df$tmin, tbase = 5, tbase_max = 30)
#'
#'
gdd <- function(tmax, tmin, tbase, tbase_max, case = "C") {
  
  if (!case %in% c("A", "B", "C", "D")){
    stop('The case argument must be either "A", "B", "C", or "D"', call. = FALSE)
  }
  
  # if (case == "A"){
  #   tbase = 0
  # }
  
  if (case %in% c("B", "C", "D")){
    tmax <- adjust_for_tbase(tmax, tbase)
    tmin <- adjust_for_tbase(tmin, tbase)
  }
  
  if (case %in% c("C")){
    tmax <- adjust_for_tbase_max(tmax, tbase_max)
    tmin <- adjust_for_tbase_max(tmin, tbase_max)
  }
  
  if (case == "D"){
    gdd_temp <- ifelse(tmax > tbase_max, yes = 0, no = (tmax + tmin) / 2 - tbase)
  } else if (case %in% c("A", "B", "C")){
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