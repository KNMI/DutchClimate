library(data.table)
library(knmiR)
library(tidyr)
library(ggplot2)
library(doParallel)
registerDoParallel(2)


# tmp <- knmiR::KIS("TG", "260_H", "1900/2017")
# save(tmp, file = "data/datasample.rda", compress = "xz")

load("data/datasample.rda")

setnames(tmp, "REH1.TG", "tg")
tmp[, REH1.Q_TG := NULL]
tmp[, DS_CODE := NULL]

today <- "2017-04-17"
startOfYear <- "2017-01-01"

dailyData <- separate(tmp, IT_DATETIME, c("date", "hms", "nano"), sep = "_")
dailyData[, hms := NULL]
dailyData[, nano := NULL]
dailyData[, date := as.Date(date, format = "%Y%m%d")]


annualMeans <- dailyData[date < startOfYear, mean(tg), by = year(date)]



currentYear <- dailyData[year(date) == year(today)]



projection <- dailyData[year(date) == 2017, .(date, tg)]

projection <- rbind(projection, dailyClimatology[date > projection[, max(date)], .(date, tg)])

k <- 12
dailyClimatology <- foreach(i = (1956 : (2016-(k-1))), .combine = "rbind") %dopar% {
  ComputeDailyClimatology(dailyData, i, i + (k-1))
}

setkey(dailyClimatology, date)


hindcastProjections <- foreach(i = 1986 : 2016, .combine = "rbind") %dopar% {
  HindcastProjection(i,
                     dailyData[year(date)==i],
                     dailyClimatology[year(date)==i])
}


setkey(hindcastProjections, date)

## calendar bias
statistics <- hindcastProjections[, .(bias = mean(res), sd = sd(res), rmse = sqrt(mean(res^2))), by = .(month(date), mday(date))]
statistics[, date := as.Date(paste(2017, month, mday, sep = "-"), format="%Y-%m-%d")]
statistics <- na.omit(statistics)

ggplot(tmp, aes(x = date, y = bias)) + geom_line()
ggplot(tmp, aes(x = date, y = sd)) + geom_line()
ggplot(tmp, aes(x = date, y = rmse)) + geom_line()


tmp[, summary(rmse)]

# > tmp[, summary(rmse)] k = 10
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.2974  0.3980  0.3853  0.4520  0.7248

# > tmp[, summary(rmse)] k = 12
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.2893  0.3862  0.3789  0.4434  0.7283

# > tmp[, summary(rmse)] k = 15
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.2871  0.3933  0.3830  0.4517  0.7410

# > tmp[, summary(rmse)] k = 20
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.2900  0.4056  0.3991  0.4789  0.7803

# > tmp[, summary(rmse)] k = 25
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.2969  0.4208  0.4180  0.5146  0.8184

# > tmp[, summary(rmse)] k = 30
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.2938  0.4360  0.4367  0.5530  0.8784

years   <- 5 : 30
rmseVec <- numeric(length(years))
for (k in seq_along(years)) {
  dailyClimatology <- foreach(i = (1956 : (2016-(years[k]-1))), .combine = "rbind") %dopar% {
    ComputeDailyClimatology(dailyData, i, i + (years[k]-1))
  }
  setkey(dailyClimatology, date)

  hindcastProjections <- foreach(i = 1986 : 2016, .combine = "rbind") %dopar% {
    HindcastProjection(i,
                       dailyData[year(date)==i],
                       dailyClimatology[year(date)==i])
  }
  setkey(hindcastProjections, date)

  tmp <- hindcastProjections[, .(bias = mean(res), sd = sd(res), rmse = sqrt(mean(res^2))), by = .(month(date), mday(date))]

  rmseVec[k] <- tmp[, mean(rmse)]
}

##



firstTry <- ComputeDailyClimatology(dailyData, 1987, 2016)

tmp3 <- dailyData[, .(TG = round(mean(tg), 2)), by = year(date)]

climateNormal <- data.frame(period = "1981/2010", year = 1981 : 2010,
                            value = tmp3[year %in% seq.int(1981, 2010, by = 1), mean(TG)])
climateNormal <- rbind(climateNormal,
                       data.frame(period = "1961/1990", year = 1961 : 1990,
                            value = tmp3[year %in% seq.int(1961, 1990, by = 1), mean(TG)]))



p1 <- ggplot(tmp3[year != 2017], aes(x = year, y = TG)) + geom_line() +
  geom_line(aes(x = year, y = value, col = period), data = climateNormal) +
  geom_smooth()

projection <- MeanProjection("2017-02-01", dailyData, NULL, dailyClimatology, statistics)

p1 + #geom_point(aes(x = year(date), y = mean), col = 2,
    #            data = projection) +
  geom_pointrange(aes(x = year(date), ymin = lower, y = mean, ymax = upper), col = 2,
                data = projection)

tmp4 <- dailyData[year(date) %in% seq.int(1986, 2016), mean(tg), by = .(month(date), mday(date))]

tmp4[, date := as.Date(paste("2017", month, mday, sep = "-"), format="%Y-%m-%d")]

tmp4 <- na.omit(tmp4)

projection <- dailyData[year(date) == 2017, .(date, tg)]

projection <- rbind(projection, dailyClimatology[date > projection[, max(date)], .(date, tg)])

p1 + geom_point(aes(x = year, y = proj), col = 2,
                data = data.frame(year = 2017, proj = projection[, mean(tg)]))

meanJanuary <- tmp2[month(date) == 1, mean(tg), by = year(date)]
meanFebrurary <- tmp2[month(date) == 2, mean(tg), by = year(date)]
meanMarch <- tmp2[month(date) == 3, mean(tg), by = year(date)]


fn <- function(data) {
  ggplot(data[year != 2017], aes(x = V1)) + geom_histogram(binwidth = 0.2) +
    geom_vline(xintercept = data[year == 2017, V1], col = 2, lty = 2) +
    xlab("TG")
}

fn(meanMarch)

# > hindcastProjections2[, summary(res)]
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -1.643000 -0.481000 -0.210700 -0.240000  0.003233  1.099000
# > hindcastProjections2[month(date) == 4 & mday(date) == 17, summary(res)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -1.5470 -0.5215 -0.3726 -0.3531 -0.1614  0.6313
# > hindcastProjections2[month(date) == 1 & mday(date) == 1, summary(res)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -1.5260 -1.1120 -0.6849 -0.5720 -0.2497  1.0640
