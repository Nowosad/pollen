#' A Pollen Season Function
#' 
#' This function calculates the start and the end of pollen season for each year
#' @param x A data.frame with dates and pollen count values
#' @param value The name of the column with pollen count values
#' @param date The name of the dates column
#' @param method The pollen season method - "95", "98", "99", "Mesa", "Jager", or "Lejoly"
#' 
#' @return A data.frame object with year, date of pollen season start and date of pollen season end
#' @importFrom lubridate year
#' @importFrom purrr %>% map map_df
#'  
#' @references Nilsson S. and Persson S.: 1981, Tree pollen spectra in the Stockholm region (Sweden) 1973-1980, Grana 20, 179-182.
#' @references Andersen T.B.: 1991, A model to predict the beginning of the pollen season, Grana 30, 269-275.
#' @references Torben B.A.: 1991, A model to predict the beginning of the pollen season, Grana 30, 269-275.
#' @references Galan C., Emberlin J., Dominguez E., Bryant R.H. and Villamandos F.: 1995, A comparative analysis of daily variations in the Gramineae pollen counts at Cordoba, Spain and London, UK, Grana 34, 189-198.
#' @references Sanchez-Mesa J.A., Smith M., Emberlin J., Allitt U., Caulton E. and Galan C.: 2003, Characteristics of grass pollen seasons in areas of southern Spain and the United Kingdom, Aerobiologia 19, 243-250.
#' @references Jager S., Nilsson S., Berggren B., Pessi A.M., Helander M. and Ramfjord H.: 1996, Trends of some airborne tree pollen in the Nordic countries and Austria, 1980-1993. A comparison between Stockholm, Trondheim, Turku and Vienna, Grana 35, 171-178.
#' @references Lejoly-Gabriel and Leuschner: 1983, Comparison of air-borne pollen at Louvain-la-Neuve (Belgium) and Basel (Switzerland) during 1979 and 1980, Grana 22, 59-64.
#' 
#' @keywords pollen, pollen season
#'
#' @export
#' 
#' @examples
#' x <- structure(list(Date = structure(c(11323, 11324, 11325, 11326, 
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
#' .Names = c("Date", "Value"), row.names = c(NA, 100L), class = "data.frame")
#'
#' pollen_season(x, "Value", "Date", method="95")

pollen_season <- function(x, value, date, method){
        x %>% split(., year(.[[date]])) %>%
                map(~pollen_season_single_year(., value=value, date=date, method=method)) %>% 
                map_df(rbind)
}

pollen_season_start <- function(method, value, date, threshold=NULL){
        if (method=='90') {
                indx <- match(TRUE, cumsum(value)>(sum(value)*0.05))
        } else if (method=='95'){
                indx <- match(TRUE, cumsum(value)>(sum(value)*0.025))
        } else if (method=='98'){
                indx <- match(TRUE, cumsum(value)>(sum(value)*0.01))
        } else if (method=='Mesa'){
                indx <- match(TRUE, value>threshold)
        } else if (method=='Jager'){
                indx <- match(TRUE, cumsum(value)>(sum(value)*0.01))
                seven_subsequent_zeros <- subsequent_zeros(value=value, indx=indx, length=7)
                if(seven_subsequent_zeros){
                        indx <- indx + 8
                        while(is_zero(value[indx])){
                                indx <- indx + 1
                        }
                }
        } else if (method=='Lejoly'){
                indx <- match(TRUE, cumsum(value)>(sum(value)*0.05))
                while(!(value[indx]>(sum(value)*0.01))){
                        indx <- indx + 1
                }
        }
        date[indx]
}     

pollen_season_end <- function(method, value, date, threshold=NULL){
        if (method=='90' | method=='Jager') {
                indx <- match(TRUE, cumsum(value)>(sum(value)*0.95))
        } else if (method=='95'){
                indx <- match(TRUE, cumsum(value)>(sum(value)*0.975))
        } else if (method=='98'){
                indx <- match(TRUE, cumsum(value)>(sum(value)*0.99))
        } else if (method=='Mesa'){
                above_threshold <- which(value>threshold)
                len_ab_thres <- length(above_threshold)
                indx <- above_threshold[[len_ab_thres]]
        } else if (method=='Lejoly'){
                above_threshold <- which(value>(sum(value)*0.01))
                len_ab_thres <- length(above_threshold)
                indx <- above_threshold[[len_ab_thres]]
                while (!(sum(value[indx:(indx+2)]) >= (sum(value)*0.03))){
                        len_ab_thres <- len_ab_thres - 1
                        indx <- above_threshold[[len_ab_thres]]
                }
        }
        date[indx]
}

pollen_season_single_year <- function(x, value, date, method){
        start <- pollen_season_start(method = method, x[[value]], x[[date]])
        end <- pollen_season_end(method = method, x[[value]], x[[date]])
        year <- unique(year(x[[date]]))
        data.frame(year=year, start=start, end=end)     
}

subsequent_zeros <- function(value, indx, length=6){
        is_zero(value[indx + seq_len(length)]) %>% all(.)
}

is_zero <- function(value){
        value==0.0
}