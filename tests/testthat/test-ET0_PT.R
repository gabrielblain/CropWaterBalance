test_that("ET0_PT() works as expected in example", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,6]
  G <- DataForCWB[,9]
  ET0 <- ET0_PT(Tavg, Rn,G)
  expect_length(ET0, 129)
  expect_equal(
    ET0[1:10,1],
    c(
      3.432709,
      5.849554,
      6.432616,
      5.695334,
      7.023900,
      7.817355,
      7.969599,
      4.393576,
      4.653462,
      5.978814
    ),
    tolerance = 0.01
  )})

test_that("ET0_PT() works as expected When G is NULL", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,6]
  expect_warning(
    ET0 <- ET0_PT(Tavg, Rn),
    "The first 3 G values were set to zero")
  expect_length(ET0, 129)
  expect_equal(
    ET0[1:10,1],
    c(
      3.104035,
      5.695479,
      6.530203,
      5.695009,
      7.023712,
      7.817749,
      7.971362,
      4.395117,
      4.651870,
      5.981313
    ),
    tolerance = 0.01
  )
})

test_that("Missing Tavg value", {
  Tavg <- DataForCWB[,2]
  Tavg[1] <- NA
  Rn <- DataForCWB[,4]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Tavg wrong format", {
  Tavg <- cbind(DataForCWB[,2],DataForCWB[,2])
  Rn <- DataForCWB[,4]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Physically impossible Tavg value", {
  Tavg <- DataForCWB[,2]
  Tavg[1] <- 71
  Rn <- DataForCWB[,6]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Another physically impossible Tavg value", {
  Tavg <- DataForCWB[,2]
  Tavg[1] <- -71
  Rn <- DataForCWB[,6]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Not mumber Tavg", {
  Tavg <- DataForCWB[,2]
  Tavg[1] <- "frio"
  Rn <- DataForCWB[,6]
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Tavg values"
  )
})

test_that("Missing Rn value", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,4]
  G <- DataForCWB[,9]
  Rn[1] <- NA
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})

test_that("Rn wrong format", {
  Tavg <- DataForCWB[,2]
  Rn <- cbind(DataForCWB[,4],DataForCWB[,4])
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})

test_that("Physically impossible Rn value", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,6]
  Rn[1] <- 71
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})

test_that("Another physically impossible Rn value", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,6]
  Rn[1] <- (-71)
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})

test_that("Not mumber Rn", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,6]
  Rn[1] <- "value"
  G <- DataForCWB[,9]
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})

test_that("Missing G value", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,4]
  G <- DataForCWB[,9]
  G[1] <- NA
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})

test_that("G wrong format", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,4]
  G <- cbind(DataForCWB[,9],DataForCWB[,9])
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})

test_that("Physically impossible G value", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,6]
  G <- DataForCWB[,9]
  G[1] <- 21
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})

test_that("Another physically impossible G value", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,6]
  G <- DataForCWB[,9]
  G[1] <- (-21)
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})

test_that("Not number G", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,6]
  G <- DataForCWB[,9]
  G[1] <- "value"
  expect_error(
    ET0 <- ET0_PT(Tavg, Rn, G),
    "Physically impossible or missing Coeff, Rn or G values"
  )
})
