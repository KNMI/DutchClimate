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

PredictMovingWindowBasis <- function(Date, dt, forecast = NULL, k = 12) {
  stopifnot(as.Date(Date) %in% dt$date)

  current     <- dt[year >= year(Date) & date <= Date]

  ndays <- yday(paste0(year(Date), "-12-31"))
  mdays <- yday(Date)
  lambda <- mdays / ndays

  if (ndays == mdays) {
    return(current[, mean(tg)])
  }

  remainder <- dt[year < year(Date) &
                    year > (year(Date) - (k+1))][month > month(Date) |
                    (month == month(Date) & day > mday(Date)),
                    mean(tg)]

  prediction <- lambda * current[, mean(tg)] + (1 - lambda) * remainder

  prediction
}

#' Moving window prediction
#' @param Date date from which to predict
#' @param dt data.table with daily measurements (at least last 30 years)
#' @param forecast data.table with operational forecast
#' @param probs probabilities to predict
#' @param k integer size of moving window in years
#' @export
PredictMovingWindow <- function(Date, dt, forecast = NULL, probs = c(0.05, 0.50, 0.95), k = 12L) {
  stopifnot(as.Date(Date) %in% dt$date)
  dt <- copy(dt)
  dt[, year := year(date)]
  dt[, month := month(date)]
  dt[, day := mday(date)]

  currentPrediction <- PredictMovingWindowBasis(Date, dt,
                                                forecast = forecast, k = k)

  # startDate <- as.Date(Date) - 365.25*30 #
  startDate <- as.Date(paste0(as.integer(substr(Date, 0, 4))-30, substr(Date, 5, 10)))
  dates     <- seq.Date(startDate, as.Date(Date), by = "year")[-31]

  hindcast    <- map_dbl(dates, PredictMovingWindowBasis, dt = dt,
                         forecast = NULL, k = k)
  actualMeans <- map_dbl(dates, CalcActualMean, dt = dt)
  res         <- actualMeans - hindcast
  stdDev      <- sd(res)

  prediction <- qnorm(probs, currentPrediction, stdDev)
  prediction <- as.data.frame(t(prediction))
  colnames(prediction) <- paste0("p", probs*100)
  cbind(date = Date, prediction)
}

CalcActualMean <- function(Date, dt) {
  dt[year(date) == year(Date), mean(tg)]
}

#' Gamlss projection
#'
#' @inheritParams PredictMovingWindow
#' @export
PredictGamlss <- function(Date, dt, forecast, probs = c(0.05, 0.50, 0.95)) {
  stopifnot(as.Date(Date) %in% dt$date)
  dt <- copy(dt)
  dt[, year := year(date)]
  dt[, month := month(date)]
  dt[, day := mday(date)]
  current <- dt[year >= year(Date) & date <= Date]
  past    <- dt[year < year(Date)]

  ndays <- yday(paste0(year(Date), "-12-31"))
  mdays <- yday(Date)
  lambda <- mdays / ndays
  if (ndays == mdays) {
    prediction <- as.data.frame(t(rep(mean(current$tg), length(probs))))
    colnames(prediction) <- paste0("p", probs*100)
    prediction <- cbind(date = Date, prediction)
    return(prediction)
  }
  tmp <- past[date < Date & (month > month(Date) | (month == month(Date) & day > mday(Date))), .(TG = mean(tg)), by = year]
  fit <- gamlss(TG ~ pb(year), data = tmp, family = "NO", control = gamlss.control(trace=FALSE))

  f = file()
  sink(file = f)
  params <- predictAll(fit, newdata = data.frame(year = year(Date)), data = tmp)
  sink()
  close(f)

  remainder  <- qnorm(probs, params$mu, params$sigma)
  prediction <- lambda * mean(current$tg) + (1 - lambda) * remainder


  prediction <- as.data.frame(t(prediction))
  colnames(prediction) <- paste0("p", probs*100)
  prediction <- cbind(date = Date, prediction)

  # list(current, past, fit, params, lambda, current[, mean(tg)], remainder, prediction)
  prediction
}

#' Produces trend envelope data frame
#'
#' @description uses linear interpolation
#' @param start dt with year, lower, and upper
#' @param end dt with year, lower, and upper
#' @export
MakeTrendEnvelope <- function(start, end) {
  combined <- rbind(start, end)
  years <- seq.int(start[, year], end[, year], by = 1)
  lower <- approx(combined[, year], combined[, lower], xout = years)$y
  upper <- approx(combined[, year], combined[, upper], xout = years)$y
  data.table(year = years, lower = lower, upper = upper)
}
