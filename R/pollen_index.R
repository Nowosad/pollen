#' A Pollen Index Function
#'
#' This function calculates the Pollen Index (PI), which is implemented as
#' the average amount of annual pollen collected based on the input data
#' @param value pollen concentration values
#' @param date dates 
#' 
#' @importFrom lubridate year is.Date
#' 
#' @keywords pollen
#'
#' @export
#' 
#' @examples
#' data(pollen_count)
#' df <- subset(pollen_count, site == 'Oz')
#' pollen_index(value = df$birch, date = df$date)

pollen_index = function(value, date){
        sum(value)/length(unique(year(date)))
}

