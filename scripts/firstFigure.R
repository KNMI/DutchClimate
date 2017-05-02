library(data.table)
library(knmiR)
library(tidyr)
library(ggplot2)


# tmp <- knmiR::KIS("TG", "260_H", "1900/2017")

save(tmp, file = "data/datasample.rda", compress = "xz")
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

ComputeDailyClimatology <- function(data, startYear, endYear) {
  tmp <- data[year(date) %in% seq.int(startYear, endYear), .(tg = mean(tg)),
              by = .(month(date), mday(date))]
  tmp[, date := as.Date(paste(endYear + 1, month, mday, sep = "-"), format="%Y-%m-%d")]
  na.omit(tmp)
}

dailyClimatology <- list()

k <- 10
for (i in (1956 : (2016-(k-1)))) {
  # print(i)
  dailyClimatology[[paste0("Years", i, "-", i+(k-1))]] <- ComputeDailyClimatology(dailyData, i, i + (k-1))
}

dailyClimatology2 <- rbindlist(dailyClimatology)
setkey(dailyClimatology2, date)

HindcastProjection <- function(year, dailyData, dailyClimatology) {
  # check that both data have same number of rows
  stopifnot(nrow(dailyData) == nrow(dailyClimatology))
  annualMean <- dailyData[year(date) == year, mean(tg)]
  nDays <- nrow(dailyData)
  projection <- numeric(nDays)
  for (i in 1 : (nDays-1)) {
    projection[i] = mean(c(dailyData[1 :i, tg], dailyClimatology[(i+1) : (nDays-1), tg]))
  }
  projection[nDays] <- annualMean
  projection
  tmp <- copy(dailyData)
  tmp[, tg := projection]
  tmp[, res := tg - annualMean]
}

hindcastProjections <- list()

for (i in 1986 : 2016) {
  hindcastProjections[[paste0("Year", i)]] <-
    HindcastProjection(i,
                       dailyData[year(date)==i],
                       dailyClimatology2[year(date)==i])
}

hindcastProjections2 <- rbindlist(hindcastProjections)
setkey(hindcastProjections2, date)

## calendar bias
tmp <- hindcastProjections2[, .(bias = mean(res), sd = sd(res), rmse = sqrt(mean(res^2))), by = .(month(date), mday(date))]
tmp[, date := as.Date(paste(2017, month, mday, sep = "-"), format="%Y-%m-%d")]
tmp <- na.omit(tmp)

ggplot(tmp, aes(x = date, y = bias)) + geom_line()
ggplot(tmp, aes(x = date, y = sd)) + geom_line()
ggplot(tmp, aes(x = date, y = rmse)) + geom_line()


##



firstTry <- ComputeDailyClimatology(dailyData, 1987, 2016)

tmp3 <- tmp2[, .(TG = round(mean(tg), 2)), by = year(date)]

climateNormal <- data.frame(period = "1981/2010", year = 1981 : 2010,
                            value = tmp3[year %in% seq.int(1981, 2010, by = 1), mean(TG)])
climateNormal <- rbind(climateNormal,
                       data.frame(period = "1961/1990", year = 1961 : 1990,
                            value = tmp3[year %in% seq.int(1961, 1990, by = 1), mean(TG)]))



p1 <- ggplot(tmp3[year != 2017], aes(x = year, y = TG)) + geom_line() +
  geom_line(aes(x = year, y = value, col = period), data = climateNormal) +
  geom_smooth()

tmp4 <- dailyData[year(date) %in% seq.int(1986, 2016), mean(tg), by = .(month(date), mday(date))]

tmp4[, date := as.Date(paste("2017", month, mday, sep = "-"), format="%Y-%m-%d")]

tmp4 <- na.omit(tmp4)

projection <- tmp2[year(date) == 2017, .(date, tg)]

projection <- rbind(projection, tmp4[date > projection[, max(date)], .(date, tg = V1)])

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
