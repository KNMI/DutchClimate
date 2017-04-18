library(data.table)
library(knmiR)


tmp <- knmiR::KIS("TG", "260_H", "1900/2017")

saveRDS(tmp, file = "data/datasample.rds")

tmp <- readRDS("data/datasample.rds")

library(tidyr)
# library(lubridate)

tmp2 <- separate(tmp, IT_DATETIME, c("date", "hms", "nano"), sep = "_")
tmp2[, hms := NULL]
tmp2[, nano := NULL]
tmp2[, date := as.Date(date, format = "%Y%m%d")]



setnames(tmp2, "REH1.TG", "tg")

tmp3 <- tmp2[, .(TG = round(mean(tg), 2)), by = year(date)]

climateNormal <- data.frame(period = "1981/2010", year = 1981 : 2010,
                            value = tmp3[year %in% seq.int(1981, 2010, by = 1), mean(TG)])
climateNormal <- rbind(climateNormal,
                       data.frame(period = "1961/1990", year = 1961 : 1990,
                            value = tmp3[year %in% seq.int(1961, 1990, by = 1), mean(TG)]))


library(ggplot2)

p1 <- ggplot(tmp3[year != 2017], aes(x = year, y = TG)) + geom_line() +
  geom_line(aes(x = year, y = value, col = period), data = climateNormal) +
  geom_smooth()

tmp4 <- tmp2[year(date) %in% seq.int(1986, 2016), mean(tg), by = .(month(date), mday(date))]

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


