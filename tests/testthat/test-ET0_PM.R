test_that("ET0_PM() works as expected in example", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  expect_length(ET0, 129)
  expect_equal(
    ET0[1:10,1],
    c(
      2.440372,
      4.171917,
      4.290477,
      3.665459,
      4.848520,
      5.669878,
      5.757218,
      3.009252,
      3.588073,
      3.929134
    ),
    tolerance = 0.01
  )})

test_that("ET0_PM() works as expected when G is NULL", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  expect_warning(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS),
    "The first 3 G values were set to zero")
  expect_length(ET0, 129)
  expect_equal(
    ET0[1:10,1],
    c(
      2.293183,
      4.104242,
      4.333359,
      3.665304,
      4.848440,
      5.670052,
      5.757965,
      3.009972,
      3.587387,
      3.930271
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
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Tavg wrong format", {
  Tavg <- cbind(DataForCWB[,2],DataForCWB[,2])
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
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
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
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
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
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
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tmax wrong format", {
  Tavg <- DataForCWB[,2]
  Tmax <- cbind(DataForCWB[,3],DataForCWB[,3])
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
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
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
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
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
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
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tmin wrong format", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- cbind(DataForCWB[,4],DataForCWB[,4])
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
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
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
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
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Missing Rn value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  Rn <- NA
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Rn wrong format", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- cbind(DataForCWB[,6],DataForCWB[,6])
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})
test_that("Missing RH value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  RH <- NA
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("RH wrong format", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- cbind(DataForCWB[,8],DataForCWB[,8])
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Physically impossible RH value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  RH[1] <- 110
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Another physically impossible RH value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  RH[1] <- -11
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Missing WS value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  WS[1] <- NA
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("WS wrong format", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- cbind(DataForCWB[,7],DataForCWB[,8])
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Physically impossible WS value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  WS[1] <- 160
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Another physically impossible WS value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  WS[1] <- (-10)
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Missing G value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  G[1] <- NA
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("G wrong format", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- cbind(DataForCWB[,9],DataForCWB[,9])
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Physically impossible G value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  G[1] <- 21
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Another physically impossible G value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  G[1] <- -21
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tavg Tmax different lengths", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[1:10,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tavg Tmax different lengths", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[1:10,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tavg Tmin different lengths", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[1:10,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tavg Rn different lengths", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[1:10,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tavg WS different lengths", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[1:10,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tavg RH different lengths", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[1:10,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("Tavg G different lengths", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[1:10,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("One more physically impossible Tavg value", {
  Tavg <- DataForCWB[,2]
  Tavg[1] <- "Quente"
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("One more physically impossible Tmax value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmax[1] <-"Quente"
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("One more physically impossible Tmin value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Tmin[1] <-"frio"
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("One more physically impossible Rn value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  Rn[1] <-"frio"
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("One more physically impossible WS value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  WS[1] <-"frio"
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("One more physically impossible RH value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  RH[1] <-"frio"
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})

test_that("One more physically impossible G value", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  G[1] <-"frio"
  expect_error(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G),
    "Physically impossible or missing Tmax, Tmin, Rn, RH, WS or G values"
  )
})
