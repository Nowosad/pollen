#' A Outliers Replacer Function
#'
#' This function finds outliers in pollen time-series and replace them with backgroud values
#' @param value pollen concentration values
#' @param date dates 
#' @param threshold anumber indicating how many times outling value needs to be larger than the backgroud to be replaces (default is 5)
#' @param sum_percent a sum_percent parameter
#'
#' @return a new data.frame object with replaced outliers
#' @importFrom lubridate year
#' @importFrom purrr %>% map map_df map_dbl
#' @importFrom dplyr lead lag
#'
#' @references  Kasprzyk, I. and A. Walanus.: 2014. Gamma, Gaussian and Logistic Distribution Models for Airborne Pollen Grains and Fungal Spore Season Dynamics, Aerobiologia 30(4), 369-83.
#'
#' @keywords pollen, pollen outliers
#'
#' @export
#'
#' @examples
#'
#' data(pollen_count)
#' df <- subset(pollen_count, site=='Shire')
#' new_df <- outliers_replacer(df$birch, df$date)
#' identical(df, new_df)
#' 
#' library('purrr')
#' new_pollen_count <- pollen_count %>% split(., .$site) %>%
#'        map_df(~outliers_replacer(value=.$hazel, date=.$date, threshold=4))
#'

outliers_replacer <- function(value, date, threshold=5, sum_percent=100) {
        data.frame(value = value, date = date) %>%
                split(., year(.[["date"]])) %>%
                map_df(~outliers_replacer_single_year(., value = "value", threshold = threshold, sum_percent = sum_percent))
}

outliers_replacer_single_year <- function(x, value, threshold, sum_percent) {
  indx <- outliers_detector(value = x[[value]], threshold = threshold, sum_percent = sum_percent)
  new_value <- indx %>% map_dbl(~(single_outlier_replacer(., value = x[[value]], threshold = threshold)))
  x[[value]][indx] <- new_value
  return(x)
}

single_outlier_replacer <- function(indx, value, threshold) {
  return(threshold * (((value[indx - 2] + value[indx + 2]) / 6) + ((value[indx - 1] + value[indx + 1]) / 3)))
}

outliers_detector <- function(value, sum_percent=100, threshold) {
  df <- data.frame(
    value, value_m2 = lag(value, 2), value_p2 = lead(value, 2),
    value_m1 = lag(value, 1), value_p1 = lead(value, 1)
  )
  df$backgroud <- threshold * (((df$value_m2 + df$value_p2) / 6) + ((df$value_m1 + df$value_p1) / 3))
  return(which(df$value > (sum(df$value) / sum_percent) & df$value > df$backgroud))
}
