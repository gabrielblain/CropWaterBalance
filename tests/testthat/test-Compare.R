

Tavg <- DataForCWB[, 2]
Tmax <- DataForCWB[, 3]
Tmin <- DataForCWB[, 4]
Ra <- DataForCWB[, 5]
Rn <- DataForCWB[, 6]
WS <- DataForCWB[, 7]
RH <- DataForCWB[, 8]
G <- DataForCWB[, 9]


test_that("Compare() works as expected in example", {
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G, Alt = 700)
  Sample2 <- ET0_PT(Tavg, Rn, G)
  Tes <- Compare(Sample1 = Sample1, Sample2 = Sample2)
  expect_s3_class(Tes, "data.frame")
  expect_length(Tes, 6)
  expect_equal(nrow(Tes), 1)
  expect_named(Tes,
               c("AME",
                 "RMSE",
                 "dorig",
                 "dmod",
                 "dref",
                 "RQuad"))
  expect_equal(Tes[, "AME"], c(1.69), tolerance = 0.1)
  expect_equal(Tes[, "RMSE"], c(1.81), tolerance = 0.1)
  expect_equal(Tes[, "dorig"], c(0.64), tolerance = 0.1)
  expect_equal(Tes[, "dmod"], c(0.38), tolerance = 0.1)
  expect_equal(Tes[, "dref"], c(-0.05), tolerance = 0.1)
  expect_equal(Tes[, "RQuad"], c(0.87), tolerance = 0.1)
})

test_that("Compare errors when there is just one Sample", {
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G, Alt = 700)
  expect_error(
    Compare(Sample1 = Sample1, Sample2 = "Sample2"),
      "`Sample1` and `Sample2` must be numerical single column variables with at
      least 5 records each. Missing data are not allowed.")
})

test_that("Compare errors when there is just one Sample", {
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G, Alt = 700)
  Sample2 <- ET0_PT(Tavg, Rn, G)
  expect_error(
    Compare(Sample1 = Sample1[1:4, 1], Sample2 = Sample2[1:4, 1]),
    "`Sample1` and `Sample2` must be numerical single column variables with at
      least 5 records each. Missing data are not allowed.")
})

test_that("Compare errors when there is just one Sample", {
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G, Alt = 700)
  Sample2 <- ET0_PT(Tavg, Rn, G)
  expect_error(
    Compare(Sample1 = Sample1, Sample2 = Sample2[1:4, 1]),
    "`Sample1` and `Sample2` must be numerical single column variables with at
      least 5 records each. Missing data are not allowed.")
})

test_that("Compare errors when there is just one Sample", {
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G, Alt = 700)
  expect_error(
    Compare(Sample1 = Sample1[1:5, 1], Sample2 = c(NA, 1, 2, 3, 4)),
    "`Sample1` and `Sample2` must be numerical single column variables with at
      least 5 records each. Missing data are not allowed.")
})
test_that("Compare errors when there is just one Sample", {
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G, Alt = 700)
  expect_error(
    Compare(Sample1 = Sample1[1:5, 1], Sample2 = c(1, 2, 3, 4)),
    "`Sample1` and `Sample2` must be numerical single column variables with at
      least 5 records each. Missing data are not allowed.")
})
