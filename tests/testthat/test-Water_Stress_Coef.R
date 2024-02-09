test_that("Water_Stress_Coef() works as expected in example", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- DataForCWB[,14]
  Ks <- DataForCWB[,15]
  Irrig <- DataForCWB[,16]
  CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
                  Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
  D <- CWB.been$SoilWaterDeficit
  Ks <- Water_Stress_Coef(AWC = AWC,MAD = MAD, Drz = Drz, SoilWaterDeficit = D)
  expect_length(Ks, 129)
  expect_equal(
    Ks[1:10,1],
    c(
      1,
      1,
      1,
      1,
      1,
      1,
      0.91,
      0.84,
      1,
      1
    ),
    tolerance = 0.01
  )})

test_that("Water_Stress_Coef() works when G is NULL", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  expect_warning(
    ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS),
    "The first 3 G values were set to zero")
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- DataForCWB[,14]
  Ks <- DataForCWB[,15]
  Irrig <- DataForCWB[,16]
  CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
                  Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
  D <- CWB.been$SoilWaterDeficit
  Ks <- Water_Stress_Coef(AWC = AWC,MAD = MAD, Drz = Drz, SoilWaterDeficit = D)
  expect_length(Ks, 129)
  expect_equal(
    Ks[1:10,1],
    c(
      1,
      1,
      1,
      1,
      1,
      1,
      0.91,
      0.84,
      1,
      1
    ),
    tolerance = 0.01
  )})

test_that("Water_Stress_Coef() errors when AWC is a single number", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- DataForCWB[,14]
  Ks <- DataForCWB[,15]
  Irrig <- DataForCWB[,16]
  CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
                  Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
  D <- CWB.been$SoilWaterDeficit
  expect_error(
    Ks <- Water_Stress_Coef(AWC = 1, MAD = MAD, Drz = Drz, SoilWaterDeficit = D),
    "Physically impossible, negative or missing AWC values"
  )})

test_that("Water_Stress_Coef() errors when AWC is negative", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- DataForCWB[,14]
  Ks <- DataForCWB[,15]
  Irrig <- DataForCWB[,16]
  CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
                  Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
  D <- CWB.been$SoilWaterDeficit
  AWC <- -1*AWC
  expect_error(
    Ks <- Water_Stress_Coef(AWC = AWC, MAD = MAD, Drz = Drz, SoilWaterDeficit = D),
    "Physically impossible, negative or missing AWC values"
  )})

test_that("Water_Stress_Coef() errors when AWC has wrong format", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- DataForCWB[,14]
  Ks <- DataForCWB[,15]
  Irrig <- DataForCWB[,16]
  CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
                  Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
  D <- CWB.been$SoilWaterDeficit
  AWC <- cbind(AWC,AWC)
  expect_error(
    Ks <- Water_Stress_Coef(AWC = AWC, MAD = MAD, Drz = Drz, SoilWaterDeficit = D),
    "Physically impossible, negative or missing AWC values"
  )})

test_that("Water_Stress_Coef() errors when MAD is a single number", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- DataForCWB[,14]
  Ks <- DataForCWB[,15]
  Irrig <- DataForCWB[,16]
  CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
                  Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
  D <- CWB.been$SoilWaterDeficit
  MAD <- 1
  expect_error(
    Ks <- Water_Stress_Coef(AWC = AWC, MAD = MAD, Drz = Drz, SoilWaterDeficit = D),
    "AWC, MAD and SoilWaterDeficit must be numerical variables with same length and no missing nor negative value.
        MAD varies between 0 and 1."
  )})

test_that("Water_Stress_Coef() errors when Drz is negative", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- DataForCWB[,14]
  Ks <- DataForCWB[,15]
  Irrig <- DataForCWB[,16]
  CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
                  Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
  D <- CWB.been$SoilWaterDeficit
  Drz <- -1*Drz
  expect_error(
    Ks <- Water_Stress_Coef(AWC = AWC, MAD = MAD, Drz = Drz, SoilWaterDeficit = D),
    "AWC, MAD and SoilWaterDeficit must be numerical variables with same length and no missing nor negative value.
        MAD varies between 0 and 1."
  )})

test_that("Water_Stress_Coef() errors when SoilDefict is negative", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- DataForCWB[,14]
  Ks <- DataForCWB[,15]
  Irrig <- DataForCWB[,16]
  CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
                  Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
  D <- -1*CWB.been$SoilWaterDeficit
  expect_error(
    Ks <- Water_Stress_Coef(AWC = AWC, MAD = MAD, Drz = Drz, SoilWaterDeficit = D),
    "AWC, MAD and SoilWaterDeficit must be numerical variables with same length and no missing nor negative value.
        MAD varies between 0 and 1."
  )})

test_that("Water_Stress_Coef() errors when SoilDefict has wrong length", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,8]
  G <- DataForCWB[,9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Rain <- DataForCWB[,10]
  Drz <- DataForCWB[,11]
  AWC <- DataForCWB[,12]
  MAD <- DataForCWB[,13]
  Kc <- DataForCWB[,14]
  Ks <- DataForCWB[,15]
  Irrig <- DataForCWB[,16]
  CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
                  Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
  D <- CWB.been$SoilWaterDeficit[1:10]
  expect_error(
    Ks <- Water_Stress_Coef(AWC = AWC, MAD = MAD, Drz = Drz, SoilWaterDeficit = D),
    "AWC, MAD and SoilWaterDeficit must be numerical variables with same length and no missing nor negative value.
        MAD varies between 0 and 1."
  )})

