#' A Pollen Season 2 Function
#' 
#' This function calculates the start and the end of pollen season for each year
#' @param df - data.frame object with dates and pollen count values
#' @param value - name of the column with pollen count values
#' @param date name of the column with dates
#' @param method - the pollen season method (95, 99, etc.)
#'
#' @return data.frame object with year, date of pollen season start and date of pollen season end
#'
#' @keywords pollen, pollen season
#'
#' @export
#' 
#' @examples
#' df <- structure(list(Date = structure(c(11323, 11324, 11325, 11326, 
#'11327, 11328, 11329, 11330, 11331, 11332, 11333, 11334, 11335, 
#'11336, 11337, 11338, 11339, 11340, 11341, 11342, 11343, 11344, 
#'11345, 11346, 11347, 11348, 11349, 11350, 11351, 11352, 11353, 
#'11354, 11355, 11356, 11357, 11358, 11359, 11360, 11361, 11362, 
#'11363, 11364, 11365, 11366, 11367, 11368, 11369, 11370, 11371, 
#'11372, 11373, 11374, 11375, 11376, 11377, 11378, 11379, 11380, 
#'11381, 11382, 11383, 11384, 11385, 11386, 11387, 11388, 11389, 
#'11390, 11391, 11392, 11393, 11394, 11395, 11396, 11397, 11398, 
#'11399, 11400, 11401, 11402, 11403, 11404, 11405, 11406, 11407, 
#'11408, 11409, 11410, 11411, 11412, 11413, 11414, 11415, 11416, 
#'11417, 11418, 11419, 11420, 11421, 11422), class = "Date"),
#' value = c(0, 
#' 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#' 0, 0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 18, 0.6,
#' 0, 13.2, 0.6, 0, 1.8, 3.6, 4.2, 6.6, 2.4, 0, 0, 3, 0.6, 0, 0.6,
#' 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0, 3, 23, 14.4, 19.8, 41, 
#' 12.6, 43, 18.6, 0.6, 1.8, 1.2, 1.8, 0, 4.2, 3, 0, 0.6, 0, 0, 
#' 0, 0, 0.6, 1.2, 0.6, 0, 0, 0, 1.2, 0, 3.6, 4.2, 0, 0, 0, 0.6,0)),
#' .Names = c("Date", "value"), row.names = c(NA, 100L), class = "data.frame")
#'
#'pollen_season2(df, "value", "Date", 95)

pollen_season2 <- function(df=df, value="betula", date="date", method=95){
  library('plyr')
  library('dplyr')
  library('lubridate')
  beginning_fun <- function(x, value) x[cumsum(x[,value])>(sum(x[,value])*((100-method)/100)), ]  
  end_fun <- function(x, value) x[cumsum(x[,value])>(sum(x[,value])*(method/100)), ]
  beg_end_fun <- function(x, value) data.frame(beginning_fun(x, value)[1,date], end_fun(x, value)[1,date])
  df[ , "year"] <- as.factor(year(df[, date]))
  df <- df %>%  arrange_(date)
  pollen_sez <- ddply(df, .variables=.(year), beg_end_fun, value)
  colnames(pollen_sez) <- c("year", "beginning", "end")
  pollen_sez
}