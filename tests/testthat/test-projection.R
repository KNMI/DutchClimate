context("Temperature projection")

library(data.table)
library(DutchClimate)

## TODO: Rename context
## TODO: Add more tests

dailyData  <- readRDS("./testInput/tg_sample.rds")
statistics <- readRDS("./testInput/tg_stats.rds")

test_that("climatology and hindcast", {
  climatology <- ComputeDailyClimatology(dailyData, 2004, 2015)
  expect_equal_to_reference(climatology,
                            "./testOutput/dailyClimatology.rds")

  hindcast <- Hindcast(2016,
                                 dailyData[year(date)==2016],
                                 climatology[year(date)==2016])

  expect_equal_to_reference(hindcast,
                            "./testOutput/dailyHindcast.rds")

  climatology <- ComputeDailyClimatology(dailyData, 2005, 2016)
  projection <- MeanProjection("2017-04-17", dailyData, NULL, climatology, statistics)

  expect_equal_to_reference(projection,
                            "./testOutput/projection20170417.rds")
})
