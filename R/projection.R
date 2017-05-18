#' Compute daily climatology
#'
#' @param data data.table daily data
#' @param startYear int starting year
#' @param endYear int end year
#' @export
ComputeDailyClimatology <- function(data, startYear, endYear) {
  tmp <- data[year(date) %in% seq.int(startYear, endYear), .(tg = mean(tg)),
              by = .(month(date), mday(date))]
  tmp[, date := as.Date(paste(endYear + 1, month, mday, sep = "-"),
                        format="%Y-%m-%d")]
  na.omit(tmp)
}

#' Hindcast
#'
#' Necessary to compute, bias, sd, rmse and other statistics of projection
#' @param year integer year for hindcast
#' @param dailyData of hindcast year
#' @param dailyClimatology daily climatology
#' @export
Hindcast <- function(year, dailyData, dailyClimatology) {
  # check that both data have same number of rows
  stopifnot(nrow(dailyData) == nrow(dailyClimatology))
  annualMean <- dailyData[year(date) == year, mean(tg)]
  nDays <- nrow(dailyData)
  projection <- numeric(nDays)
  for (i in 1 : (nDays-1)) {
    projection[i] = mean(c(dailyData[1 :i, tg],
                           dailyClimatology[(i+1) : (nDays-1), tg]))
  }
  projection[nDays] <- annualMean
  projection
  tmp <- copy(dailyData)
  tmp[, tg := projection]
  tmp[, res := annualMean - tg]
}

#' Projection
#'
#' @param day date of projection
#' @param measurements daily data
#' @param forecast 14-day forecast (not implemented yet)
#' @param climatology daily climatology
#' @param statistics statistics for bias correction and uncertainty
#' @param sdfactor factor to determine cofidence interval (default = 1.96)
#' @export
MeanProjection <- function(day, measurements, forecast, climatology,
                           statistics, sdfactor = 1.96) {
  stopifnot(as.Date(day) %in% measurements$date)
  statistics[, date := as.Date(paste(year(day), month, mday, sep = "-"), format="%Y-%m-%d")]
  statistics <- na.omit(statistics)
  projection <- rbind(measurements[year(date) == year(day),
                                   .(date, tg)][date <= day, ],
                      climatology[year(date) == year(day),
                                  .(date, tg)][date > day, ])
  projection <- projection[, mean(tg)] #+ statistics[date == day, bias]
  uncertainty <- sdfactor * statistics[date == day, sd]
  projection  <- projection + cbind(-1, 0, 1) * uncertainty
  colnames(projection) <- c("lower", "mean", "upper")
  projection <- as.data.table(projection)
  projection[, date := day]
  return(projection)
}
