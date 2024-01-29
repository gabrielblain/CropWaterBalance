test_that("CWB() works as expected in example", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  tes <- CWB(Rain, ETr, AWC,Drz,MAD)
  expect_s3_class(tes, "data.frame")
  expect_length(tes, 17)
  expect_equal(nrow(tes), 129)
  expect_named(
    tes,
    c("DaysSeason","Rain","Irrig","ETr","Kc","Ks","ETc", "P-ETc","ActualCropEvap",
      "StoredWaterRoot","DeltaWaterRoot","Perc","ET_Defict",
      "TAW","SoilWaterDeficit","d_MAD", "D>=d_MAD"))
  expect_equal(tes[1:3, "ActualCropEvap"], c(0.7321117,1.2407705,1.2418008),tolerance = 0.01)
  expect_equal(tes[1:3, "StoredWaterRoot"], c(45.72000,44.73323,43.49143),tolerance = 0.01)
  expect_equal(tes[1:3, "DeltaWaterRoot"], c(0.00000000,-0.98677054,-1.24180079),tolerance = 0.01)
  expect_equal(tes[1:3, "Perc"], c(44.7378883,0.0000000,0.0000000),tolerance = 0.01)
  expect_equal(tes[1:3, "ET_Defict"], c(0.000000e+00,1.080443e-02,4.534235e-02),tolerance = 0.01)
  expect_equal(tes[1:3, "TAW"], c(45.72,45.72,45.72),tolerance = 0.01)
  expect_equal(tes[1:3, "SoilWaterDeficit"], c(0.00000000,0.99757497,2.28471811),tolerance = 0.01)
  expect_equal(tes[1:3, "d_MAD"], c(13.716,13.716,13.716),tolerance = 0.01)
  expect_equal(tes[1:3, "D>=d_MAD"], c("No","No","No"))
  })

test_that("Physically impossible rain values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- (-1*DataForCWB[,10])
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz),
    "Physically impossible or missing rain values"
  )
})

test_that("Missing rain values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Rain[1] <- NA
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz),
    "Physically impossible or missing rain values"
  )
})

test_that("Physically impossible Kc values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- rep(8,129)
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz,Kc = Kc),
    "Physically impossible Ks or Kc values"
  )
})

test_that("Missing Kc values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- rep(8,129)
  Kc[1] <- NA
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz,Kc = Kc),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Physically impossible Ks values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Ks <- rep(-1,129)
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz,Ks = Ks),
    "Physically impossible Ks or Kc values"
  )
})

test_that("Missing Ks values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Ks <- rep(0.8,129)
  Ks[1] <- NA
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz,Ks = Ks),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})
test_that("Negative Irrig values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Irrig <- rep(-0.8,129)
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz, Irrig = Irrig),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Missing Irrig values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Irrig <- rep(0,129)
  Irrig[1] <- NA
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz, Irrig = Irrig),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Wrong MAD values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  MAD[1:4] <- 4
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz, MAD=MAD),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Wrong MAD values. Single number", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- 4
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz, MAD=MAD),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Wrong Drz values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  Drz[1] <- (-4)
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz, Drz = Drz),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Wrong Drz values. Character", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  Drz[1] <- "profunda"
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz, Drz = Drz),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Wrong AWC values", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  AWC[1] <- 0
  MAD <- DataForCWB[,13]
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz, MAD=MAD),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Wrong AWC values. Single number", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ETr <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  AWC <- 0.4
  MAD <- DataForCWB[,13]
  expect_error(
    tes <- CWB(Rain, ETr, AWC, Drz, MAD=MAD),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})
