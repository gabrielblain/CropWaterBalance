test_that("Campare() works as expected in example", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
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
  expect_equal(Tes[, "AME"], c(1.69222), tolerance = 0.01)
  expect_equal(Tes[, "RMSE"], c(1.813449), tolerance = 0.01)
  expect_equal(Tes[, "dorig"], c(0.6403158), tolerance = 0.01)
  expect_equal(Tes[, "dmod"], c(0.376103), tolerance = 0.01)
  expect_equal(Tes[, "dref"], c(-0.05737454), tolerance = 0.01)
  expect_equal(Tes[, "RQuad"], c(0.8675223), tolerance = 0.01)
})

test_that("Compare errors when there is just one Sample", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 7]
  G <- DataForCWB[, 9]
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  expect_error(
    Compare(Sample1 = Sample1, Sample2 = "Sample2"),
    "Samples 1 and 2 must be numerical single column variables with at least 5*"
  )
})

test_that("Compare errors when there is just one Sample", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 7]
  G <- DataForCWB[, 9]
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Sample2 <- ET0_PT(Tavg, Rn, G)
  expect_error(
    Compare(Sample1 = Sample1[1:4, 1], Sample2 = Sample2[1:4, 1]),
    "Samples 1 and 2 must be numerical single column variables with at least 5*"
  )
})

test_that("Compare errors when there is just one Sample", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 7]
  G <- DataForCWB[, 9]
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Sample2 <- ET0_PT(Tavg, Rn, G)
  expect_error(
    Compare(Sample1 = Sample1, Sample2 = Sample2[1:4, 1]),
    "Samples 1 and 2 must be numerical single column variables with at least 5*"
  )
})

test_that("Compare errors when there is just one Sample", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 7]
  G <- DataForCWB[, 9]
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  expect_error(
    Compare(Sample1 = Sample1[1:5, 1], Sample2 = c(NA, 1, 2, 3, 4)),
    "Samples 1 and 2 must be numerical single column variables with at least 5*"
  )
})
test_that("Compare errors when there is just one Sample", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 7]
  G <- DataForCWB[, 9]
  Sample1 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  expect_error(
    Compare(Sample1 = Sample1[1:5, 1], Sample2 = c(1, 2, 3, 4)),
    "Samples 1 and 2 must be numerical single column variables with at least 5*"
  )
})
