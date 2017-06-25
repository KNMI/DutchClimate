#' Calculates annual mean
#'
#' @param dt data frame
#' @param method WMO or normal
#' @export
CalculateAnnualMean <- function(dt, method = "WMO") {
  if (method == "normal") {
    return(dt[, .(TG = round(mean(tg), 2)), by = year(date)])
  } else if (method == "WMO")
    return(dt[, .(TG = mean(tg)), by = list(year = year(date),
        month = month(date))][, .(TG = round(mean(TG), 2)), by = year])
}
