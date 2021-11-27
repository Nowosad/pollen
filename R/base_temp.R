#' Determining base temperatures 
#' 
#' This function determines a base temperature ("tbase") based on the mean temperature of the entire season and the number of days of the ith planting to reach a given developmental stage under study. 
#' It allows to use one of four methods to calculate tbase, including:
#' (1) the least standard deviation in GDD (Magoon and Culpepper, 1932; Stier, 1939) - `"sd_gdd"`;
#' (2) the least standard deviation in days (Arnold, 1959) - `"sd_day"`;
#' (3) the coefficient of variation in days (Nuttonson, 1958) - `"cv_day"`;
#' (4) the regression coefficient (Hoover, 1955) - `"y_i"`.
#'
#' @param tavg the mean temperature of the entire season for the plantings (a numerical vector, where one value is a planting)
#' @param d the number of days of the ith planting to reach a given developmental stage under study (e.g., flowering ) (a numerical vector, where one value is a planting)
#' @param type either `"sd_gdd"`, `"sd_day"`, `"cv_day"`, or `"y_i"`. For the explanation of each type, see the Yang et al. 1995
#'
#' @return a numeric value representing base temperature that could be then used, for example, in GDD calculations
#'
#' @references Yang, S., Logan, J., & Coffey, D. L. (1995). Mathematical formulae for calculating the base temperature for growing degree days. In Agricultural and Forest Meteorology (Vol. 74, Issues 1-2, pp. 61-74). Elsevier BV
#' @references Magoon, C. A., & Culpepper, C. W. (1932). Response of sweet corn to varying temperatures from time of planting to canning maturity (No. 1488-2016-124513).
#' @references Stier, H. S. (1939). A physiological study of growth and fruiting in the tomato (Lycopersicon esculentum L.) with reference to the effect of climatic and edaphic conditions (Doctoral dissertation, Ph. D. Dissertation, University of Maryland, College Park, MD, USA).
#' @references Arnold, C. Y. (1959, January). The determination and significance of the base temperature in a linear heat unit system. In Proceedings of the american Society for horticultural Science (Vol. 74, No. 1, pp. 430-445).
#' @references Nuttonson, M. Y. (1955). Wheat-climate relationships and the use of phenology in ascertaining the thermal and photo-thermal requirements of wheat. Amer. Inst. of Crop Ecology, Washington, D.
#' @references Hoover, M. W. (1955). Some effects of temperature on the growth of southern peas. In Proc. Am. Soc. Hortic. Sci (Vol. 66, pp. 308-312).
#' 
#' @export
#' 
#' @seealso [gdd()] for calculation of growing degree days (GDD)
#'
#' @examples
#' library(pollen)
#' tavg <- c(25, 20, 15, 10)
#' d <- c(6, 11, 16, 21)
#' base_temp(tavg = tavg, d = d, type = "sd_gdd")
#' base_temp(tavg = tavg, d = d, type = "sd_day")
#' base_temp(tavg = tavg, d = d, type = "cv_day")
#' base_temp(tavg = tavg, d = d, type = "y_i")
base_temp = function(tavg, d, type){
        if (type == "sd_gdd"){
                base_temp = sd_gdd_method(tavg, d)
        } else if (type == "sd_day") {
                base_temp = sd_day_method(tavg, d)
        } else if (type == "cv_day") {
                base_temp = cv_day_method(tavg, d)
        } else if (type == "y_i"){
                base_temp = y_i_method(tavg, d)
        } else {
                stop(call. = FALSE, 'The type argument must be either "sd_gdd", "sd_day", "cv_day", or "y_i"')
        }
        return(base_temp)
}

sd_gdd_method = function(tavg, d){
        n = length(tavg)
        
        divid = ((sum(tavg * d)) * (sum(d))) - (n * sum(d^2 * tavg))
        divis = ((sum(d))^2) - (n * (sum(d^2)))
        base_temp = divid / divis
        return(base_temp)
}

sd_day_method = function(tavg, d){
        tavg_overall = mean(tavg)
        t_i = tavg_overall - tavg
        n = length(tavg)
        
        divid = ((sum(t_i*d))^2) - (n * sum(t_i^2 * d^2))
        divis = (n * sum(d^2 * t_i)) - (n * sum(t_i * d) * sum(d))
        base_temp = tavg_overall - divid / divis
        return(base_temp)
}

cv_day_method = function(tavg, d){
        divid = (sum(tavg * d^2) * sum(tavg * d)) - ((sum(d)) * sum(tavg^2*d^2))
        divis = (sum(d^2) * sum(tavg * d)) - (sum(d) * sum(tavg * d^2))
        base_temp = divid / divis
        return(base_temp)
}

y_i_method = function(tavg, d){
        n = length(tavg)
        
        divid = (sum(tavg) * sum(d * tavg)) - (n * sum(d * tavg^2))
        divis = (sum(d) * sum(tavg)) - (n * sum(d * tavg))
        
        base_temp = divid / divis
        return(base_temp)
}
