test_that("ETr_PM() works as expected in example", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  expect_length(ETr, 129)
  expect_equal(
    ETr[1:10,1],
    c(
      5.310138,
      6.967714,
      7.628712,
      6.193776,
      7.283319,
      7.618046,
      9.165304,
      6.326861,
      7.135267,
      7.362805
    ),
    tolerance = 0.01
  )})

test_that("Missing Tavg value", {
  Tavg <- DataForCWB[,2]
  Tavg <- NA
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Tavg wrong format", {
  Tavg <- cbind(DataForCWB[,2],DataForCWB[,2])
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Physically impossible Tavg value", {
  Tavg <- DataForCWB[,2]
  Tavg[1] <- 71
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Another physically impossible Tavg value", {
  Tavg <- DataForCWB[,2]
  Tavg[1] <- -71
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Missing Tmax value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmax <- NA
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tmax wrong format", {
  Tavg <- DataForCWB[,2]
  Tmax <- cbind(DataForCWB[,3],DataForCWB[,3])
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Physically impossible Tmax value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmax[1] <- 81
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Another physically impossible Tmax value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmax[1] <- -81
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Missing Tmin value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Tmin <- NA
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tmin wrong format", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- cbind(DataForCWB[,4],DataForCWB[,4])
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Physically impossible Tmin value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Tmin[1] <- 71
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Another physically impossible Tmin value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Tmin[1] <- -81
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  expect_error(
    ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})
