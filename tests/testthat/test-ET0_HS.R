test_that("ET0_HS() works as expected in example", {
  ET0 <- ET0_HS(
    Ra = DataForCWB[, 5],
    Tavg = DataForCWB[, 2],
    Tmax = DataForCWB[, 3],
    Tmin = DataForCWB[, 4]
  )
  expect_length(ET0, 129)
  expect_equal(
    ET0[1:10, 1],
    c(
      4.703700,
      5.331592,
      5.664174,
      6.163377,
      5.291303,
      6.251883,
      6.729301,
      5.842178,
      5.402246,
      5.879287
    ),
    tolerance = 0.01
  )
})

test_that("Missing Ra value", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Ra[1] <- NA
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*")
})

test_that("Missing Tmin value", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Tmin[1] <- NA
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*")
})

test_that("Missing Tmax value", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Tmax[1] <- NA
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*")
})

test_that("Missing Tavg value", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Ra <- DataForCWB[, 5]
  Tavg[1] <- NA
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*")
})

test_that("Tavg and Tmax different length", {
  Tavg <- DataForCWB[1:10, 2]
  Tmax <- DataForCWB[1:9, 3]
  Tmin <- DataForCWB[1:10, 4]
  Ra <- DataForCWB[1:10, 5]
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*")
})

test_that("Tavg and Tmin different length", {
  Tavg <- DataForCWB[1:10, 2]
  Tmax <- DataForCWB[1:10, 3]
  Tmin <- DataForCWB[1:9, 4]
  Ra <- DataForCWB[1:10, 5]
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*")
})

test_that("Tavg and Ra different length", {
  Tavg <- DataForCWB[1:10, 2]
  Tmax <- DataForCWB[1:10, 3]
  Tmin <- DataForCWB[1:10, 4]
  Ra <- DataForCWB[1:9, 5]
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*")
})

test_that("Tmax and Tmin different length", {
  Tavg <- DataForCWB[1:10, 2]
  Tmax <- DataForCWB[1:10, 3]
  Tmin <- DataForCWB[1:9, 4]
  Ra <- DataForCWB[1:10, 5]
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*")
})

test_that("Tmax and Ra different length", {
  Tavg <- DataForCWB[1:10, 2]
  Tmax <- DataForCWB[1:10, 3]
  Tmin <- DataForCWB[1:10, 4]
  Ra <- DataForCWB[1:9, 5]
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*")
})

test_that("Tavg and Ra different length", {
  Tavg <- DataForCWB[1:10, 2]
  Tmax <- DataForCWB[1:10, 3]
  Tmin <- DataForCWB[1:10, 4]
  Ra <- DataForCWB[1:9, 5]
  expect_error(ET0_HS(Ra, Tavg, Tmax, Tmin),
               regexp = "`Ra`, `Tavg`, `Tmax`, and `Tmin` must be.*"))
})
