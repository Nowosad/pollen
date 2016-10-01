#' A Pollen Season Function
#' 
#' This function calculates the start and the end of pollen season for each year
#' @param x A data.frame with dates and pollen count values
#' @param value The name of the column with pollen count values
#' @param date The name of the dates column
#' @param method The pollen season method - "90", "95", "98", "Mesa", "Jager", or "Lejoly"
#' 
#' @return A data.frame object with year, date of pollen season start and date of pollen season end
#' @importFrom lubridate year is.Date
#' @importFrom purrr %>% map map_df
#' @importFrom dplyr arrange_
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
#'
#' data(pollen_count)
#' df <- subset(pollen_count, site=='Oz')
#' pollen_season(df, value="birch", date="date", method="95")
#' 
#' df2 <- subset(pollen_count, site=='Atlantis')
#' pollen_season(df2, value="alder", date="date", method="95")
#' 
#' library('purrr')
#' pollen_count %>% split(., .$site) %>% 
#'                 map(~pollen_season(., value="hazel", date="date", method="95"))
#'                 

pollen_season <- function(x, value, date, method){
        if (!(is.character(date))) stop("Object data should be a name of column containg dates.")
        x %>% split(., year(.[[date]])) %>%
                map(~arrange_(., date)) %>% 
                map(~pollen_season_single_year(., value=value, date=date, method=method)) %>% 
                map_df(rbind)
}

pollen_season_start <- function(method, value, date, threshold=NULL){
        if (!(is.character(method))) stop("Object method should be of class character.")
        if (!(is.numeric(value))) stop("Object value should be of class numeric.")
        if (!(is.Date(date))) stop("Object date should be of class Date")
        
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
        } else {
                stop("There isn't a method called ", method, "!")
        }
        date[indx]
}     

pollen_season_end <- function(method, value, date, threshold=NULL){
        if (!(is.character(method))) stop("Object method should be of class character.")
        if (!(is.numeric(value))) stop("Object value should be of class numeric.")
        if (!(is.Date(date))) stop("Object data should be of class Date")
        
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
        } else {
                stop("There isn't a method called: ", method)
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