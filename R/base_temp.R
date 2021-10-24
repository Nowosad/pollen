#' Title
#'
#' @param tavg 
#' @param d 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
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
        base_temp = divid/divis
        return(base_temp)
}

sd_day_method = function(tavg, d){
        tavg_overall = mean(tavg)
        t_i = tavg_overall - tavg
        n = length(tavg)
        
        divid = ((sum(t_i*d))^2) - (n * sum(t_i^2 * d^2))
        divis = (n * sum(d^2 * t_i)) - (n * sum(t_i * d) * sum(d))
        base_temp = tavg_overall - divid/divis
        return(base_temp)
}

cv_day_method = function(tavg, d){
        divid = (sum(tavg * d^2) * sum(tavg * d)) - ((sum(d)) * sum(tavg^2*d^2))
        divis = (sum(d^2) * sum(tavg * d)) - (sum(d) * sum(tavg * d^2))
        base_temp = divid/divis
        return(base_temp)
}

y_i_method = function(tavg, d){
        n = length(tavg)
        
        divid = (sum(tavg) * sum(d * tavg)) - (n * sum(d * tavg^2))
        divis = (sum(d) * sum(tavg)) - (n * sum(d * tavg))
        
        base_temp = divid/divis
        return(base_temp)
}
