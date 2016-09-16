context("KIS data extraction")

test_that("De Bilt temperature", {
  expect_match(WriteKISRecipe('TG', '260_H', '2016'), 'KIStable.txt')
  expect_error(KIS('rr', '260_H', '2016'), "Must be element of set {'TG'}.", fixed = TRUE)
  expect_error(KIS('TG', '260_H', '2016'), "Not implemented")
})
