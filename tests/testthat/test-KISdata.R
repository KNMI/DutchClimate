library(futile.logger)
flog.threshold(DEBUG)
flog.appender(appender.file('DutchClimate.log'))

context("KIS data extraction")

test_that("Obtain temperature", {
  skip_on_travis()
  expect_match(WriteKISRecipe('TG', '260_H', '2016'), 'KIStable.txt')
  expect_error(KIS('rr', '260_H', '2016'), "Must be element of set {'TG'}.", fixed = TRUE)
  result <- KIS('TG', '260_H', '2016-08/2016-09')
  expect_equal_to_reference(result, file = "testOutput/temperatureDeBilt.rds")
  result <- KIS('TG', '310_H', '2016-08/2016-09')
  expect_equal_to_reference(result, file = "testOutput/temperatureVlissingen.rds")
})
