test_that("CWB() works as expected in example", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  tes <- CWB(
    Rain = Rain,
    ET0 = ET0,
    AWC = AWC,
    Drz = Drz,
    Kc = Kc,
    Irrig = Irrig,
    MAD = MAD,
    start.date = "2023-11-23"
  )
  expect_s3_class(tes, "data.frame")
  expect_length(tes, 14)
  expect_equal(nrow(tes), 129)
  expect_named(
    tes,
    c(
      "DaysSeason",
      "Rain",
      "Irrig",
      "ET0",
      "Kc",
      "WaterStressCoef_Ks",
      "ETc",
      "(P+Irrig)-ETc",
      "NonStandardCropEvap",
      "ET_Defict",
      "TAW",
      "SoilWaterDeficit",
      "d_MAD",
      "D>=dmad-(MAD*dmad)"
    )
  )
  expect_equal(tes[1:3, "NonStandardCropEvap"], c(2.440372, 4.171917, 4.290477), tolerance = 0.01)
  expect_equal(tes[1:3, "ET_Defict"], c(0.000000000, 0.000000000, 0.000000000), tolerance = 0.01)
  expect_equal(tes[1:3, "TAW"], c(45.72, 45.72, 45.72), tolerance = 0.01)
  expect_equal(tes[1:3, "SoilWaterDeficit"], c(0.0000000, 3.9179166, 8.2083937), tolerance = 0.01)
  expect_equal(tes[1:3, "d_MAD"], c(13.716, 13.716, 13.716), tolerance = 0.01)
  expect_equal(tes[1:3, "D>=dmad-(MAD*dmad)"], c("No", "No", "No"))
})

test_that("CWB() works as expected when `InitialD` is provided", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  tes <- CWB(
    Rain = Rain,
    ET0 = ET0,
    AWC = AWC,
    Drz = Drz,
    Kc = Kc,
    Irrig = Irrig,
    MAD = MAD,
    InitialD = 0,
    start.date = "2023-11-23"
  )
  expect_s3_class(tes, "data.frame")
  expect_length(tes, 14)
  expect_equal(nrow(tes), 129)
  expect_named(
    tes,
    c(
      "DaysSeason",
      "Rain",
      "Irrig",
      "ET0",
      "Kc",
      "WaterStressCoef_Ks",
      "ETc",
      "(P+Irrig)-ETc",
      "NonStandardCropEvap",
      "ET_Defict",
      "TAW",
      "SoilWaterDeficit",
      "d_MAD",
      "D>=dmad-(MAD*dmad)"
    )
  )
  expect_equal(tes[1:3, "NonStandardCropEvap"], c(2.440372, 4.171917, 4.290477), tolerance = 0.01)
  expect_equal(tes[1:3, "ET_Defict"], c(0.000000000, 0.000000000, 0.000000000), tolerance = 0.01)
  expect_equal(tes[1:3, "TAW"], c(45.72, 45.72, 45.72), tolerance = 0.01)
  expect_equal(tes[1:3, "SoilWaterDeficit"], c(0.0000000, 3.9179166, 8.2083937), tolerance = 0.01)
  expect_equal(tes[1:3, "d_MAD"], c(13.716, 13.716, 13.716), tolerance = 0.01)
  expect_equal(tes[1:3, "D>=dmad-(MAD*dmad)"], c("No", "No", "No"))
})

test_that("Wrong date format", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  start.date <- "date"
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      Kc = Kc,
      Irrig = Irrig,
      MAD = MAD,
      start.date = start.date
    ),
    "`date` is not in a valid date format. Please enter a valid date format."
  )
})


test_that("CWB() works as expected When `Kc` is NULL", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Irrig <- DataForCWB[, 15]
  tes <- CWB(
    Rain = Rain,
    ET0 = ET0,
    AWC = AWC,
    Drz = Drz,
    Irrig = Irrig,
    MAD = MAD,
    start.date = "2023-11-23"
  )
  expect_s3_class(tes, "data.frame")
  expect_length(tes, 14)
  expect_equal(nrow(tes), 129)
  expect_named(
    tes,
    c(
      "DaysSeason",
      "Rain",
      "Irrig",
      "ET0",
      "Kc",
      "WaterStressCoef_Ks",
      "ETc",
      "(P+Irrig)-ETc",
      "NonStandardCropEvap",
      "ET_Defict",
      "TAW",
      "SoilWaterDeficit",
      "d_MAD",
      "D>=dmad-(MAD*dmad)"
    )
  )
  expect_equal(tes[1:3, "NonStandardCropEvap"], c(2.440372, 4.171917, 4.290477), tolerance = 0.01)
  expect_equal(tes[1:3, "ET_Defict"], c(0.000000000, 0.000000000, 0.000000000), tolerance = 0.01)
  expect_equal(tes[1:3, "TAW"], c(45.72, 45.72, 45.72), tolerance = 0.01)
  expect_equal(tes[1:3, "SoilWaterDeficit"], c(0.0000000, 3.9179166, 8.2083937), tolerance = 0.01)
  expect_equal(tes[1:3, "d_MAD"], c(13.716, 13.716, 13.716), tolerance = 0.01)
  expect_equal(tes[1:3, "D>=dmad-(MAD*dmad)"], c("No", "No", "No"))
})

test_that("CWB() works as expected When Irrig is NULL", {
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
  #Irrig <- DataForCWB[,15]
  tes <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz, Kc=Kc,
             MAD=MAD, start.date = "2023-11-23")
  expect_s3_class(tes, "data.frame")
  expect_length(tes, 14)
  expect_equal(nrow(tes), 129)
  expect_named(
    tes,
    c("DaysSeason","Rain","Irrig","ET0","Kc","WaterStressCoef_Ks","ETc", "(P+Irrig)-ETc","NonStandardCropEvap",
      "ET_Defict","TAW","SoilWaterDeficit","d_MAD", "D>=dmad-(MAD*dmad)"))
  expect_equal(tes[1:3, "NonStandardCropEvap"], c(2.440372,4.171917,4.290477),tolerance = 0.01)
  expect_equal(tes[1:3, "ET_Defict"], c(0.000000000,0.000000000,0.000000000),tolerance = 0.01)
  expect_equal(tes[1:3, "TAW"], c(45.72,45.72,45.72),tolerance = 0.01)
  expect_equal(tes[1:3, "SoilWaterDeficit"], c(0.0000000,3.9179166,8.2083937),tolerance = 0.01)
  expect_equal(tes[1:3, "d_MAD"], c(13.716,13.716,13.716),tolerance = 0.01)
  expect_equal(tes[1:3, "D>=dmad-(MAD*dmad)"], c("No","No","No"))
})

test_that("CWB() works as expected When MAD is NULL", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  #MAD <- DataForCWB[,13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  tes <- CWB(
    Rain = Rain,
    ET0 = ET0,
    AWC = AWC,
    Drz = Drz,
    Kc = Kc,
    Irrig = Irrig,
    start.date = "2023-11-23"
  )
  expect_s3_class(tes, "data.frame")
  expect_length(tes, 14)
  expect_equal(nrow(tes), 129)
  expect_named(
    tes,
    c(
      "DaysSeason",
      "Rain",
      "Irrig",
      "ET0",
      "Kc",
      "WaterStressCoef_Ks",
      "ETc",
      "(P+Irrig)-ETc",
      "NonStandardCropEvap",
      "ET_Defict",
      "TAW",
      "SoilWaterDeficit",
      "d_MAD",
      "D>=dmad-(MAD*dmad)"
    )
  )
  expect_equal(tes[1:3, "NonStandardCropEvap"], c(2.440372, 4.171917, 4.290477), tolerance = 0.01)
  expect_equal(tes[1:3, "ET_Defict"], c(0.000000000, 0.000000000, 0.000000000), tolerance = 0.01)
  expect_equal(tes[1:3, "TAW"], c(45.72, 45.72, 45.72), tolerance = 0.01)
  expect_equal(tes[1:3, "SoilWaterDeficit"], c(0.0000000, 3.9179166, 8.2083937), tolerance = 0.01)
  expect_equal(tes[1:3, "d_MAD"], c(13.716, 13.716, 13.716), tolerance = 0.01)
  expect_equal(tes[1:3, "D>=dmad-(MAD*dmad)"], c("No", "No", "No"))
})

test_that("CWB() works as expected when G is NULL", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  expect_warning(ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS),
                 "The first 3 G values were set to zero")
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  tes <- CWB(
    Rain = Rain,
    ET0 = ET0,
    AWC = AWC,
    Drz = Drz,
    MAD = MAD,
    Kc = Kc,
    Irrig = Irrig,
    start.date = "2023-11-23"
  )
  expect_s3_class(tes, "data.frame")
  expect_length(tes, 14)
  expect_equal(nrow(tes), 129)
  expect_named(
    tes,
    c(
      "DaysSeason",
      "Rain",
      "Irrig",
      "ET0",
      "Kc",
      "WaterStressCoef_Ks",
      "ETc",
      "(P+Irrig)-ETc",
      "NonStandardCropEvap",
      "ET_Defict",
      "TAW",
      "SoilWaterDeficit",
      "d_MAD",
      "D>=dmad-(MAD*dmad)"
    )
  )
  expect_equal(tes[1:3, "NonStandardCropEvap"], c(2.293183, 4.104242, 4.333359), tolerance = 0.01)
  expect_equal(tes[1:3, "ET_Defict"], c(0.000000000, 0.000000000, 0.000000000), tolerance = 0.01)
  expect_equal(tes[1:3, "TAW"], c(45.72, 45.72, 45.72), tolerance = 0.01)
  expect_equal(tes[1:3, "SoilWaterDeficit"], c(0.0000000, 3.8502425, 8.1836018), tolerance = 0.01)
  expect_equal(tes[1:3, "d_MAD"], c(13.716, 13.716, 13.716), tolerance = 0.01)
  expect_equal(tes[1:3, "D>=dmad-(MAD*dmad)"], c("No", "No", "No"))
})

test_that("CWB() works as expected when P<ET0 on the very first day", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  ET0 <- ET0[2:10]
  ET0 <- as.matrix(ET0)
  Rain <- DataForCWB[2:10, 10]
  Drz <- DataForCWB[2:10, 11]
  AWC <- DataForCWB[2:10, 12]
  MAD <- DataForCWB[2:10, 13]
  Kc <- DataForCWB[2:10, 14]
  Ks <- DataForCWB[2:10, 15]
  Irrig <- DataForCWB[2:10, 16]
  tes1 <- CWB(
    Rain = Rain,
    ET0 = ET0,
    AWC = AWC,
    Drz = Drz,
    Kc = Kc,
    Irrig = Irrig,
    MAD = MAD,
    start.date = "2023-11-23"
  )
  expect_s3_class(tes1, "data.frame")
  expect_length(tes1, 14)
  expect_equal(nrow(tes1), 9)
  expect_named(
    tes1,
    c(
      "DaysSeason",
      "Rain",
      "Irrig",
      "ET0",
      "Kc",
      "WaterStressCoef_Ks",
      "ETc",
      "(P+Irrig)-ETc",
      "NonStandardCropEvap",
      "ET_Defict",
      "TAW",
      "SoilWaterDeficit",
      "d_MAD",
      "D>=dmad-(MAD*dmad)"
    )
  )
  expect_equal(tes1[1:3, "NonStandardCropEvap"], c(4.171917, 4.290477, 3.665459), tolerance = 0.01)
  expect_equal(tes1[1:3, "ET_Defict"], c(0.0000000, 0.0000000, 0.0000000), tolerance = 0.01)
  expect_equal(tes1[1:3, "TAW"], c(45.72, 45.72, 45.72), tolerance = 0.01)
  expect_equal(tes1[1:3, "SoilWaterDeficit"], c(3.9179166, 8.2083937, 0.4438527), tolerance = 0.01)
  expect_equal(tes1[1:3, "d_MAD"], c(13.716, 13.716, 13.716), tolerance = 0.01)
  expect_equal(tes1[1:3, "D>=dmad-(MAD*dmad)"], c("No", "No", "No"))
})

test_that("Physically impossible rain values", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- (-1 * DataForCWB[, 10])
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <-
      CWB(Rain, ET0, AWC, Drz,  Kc, Ks, Irrig, start.date = "2023-11-23"),
    "Physically impossible or missing rain values"
  )
})

test_that("Missing rain values", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Rain[1] <- NA
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <-
      CWB(Rain, ET0, AWC, Drz,  Kc, Ks, Irrig, start.date = "2023-11-23"),
    "Physically impossible or missing rain values"
  )
})

test_that("Wrong format rain", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- cbind(DataForCWB[, 10], DataForCWB[, 10])
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <-
      CWB(Rain, ET0, AWC, Drz, Kc, Ks, Irrig, MAD, start.date = "2023-11-23"),
    "Physically impossible or missing rain values"
  )
})

test_that("Physically impossible Kc values", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  Kc[1] <- 40
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Missing Kc values", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  Kc[1] <- NA
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})


test_that("Negative Irrig values", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- rep(-0.8, 129)
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Missing Irrig values", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  Irrig[1] <- NA
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Physically impossible MAD values", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  MAD[1:4] <- 4
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Wrong MAD values. Single number", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- 4
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Physically impossible Drz values", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  Drz[1] <- (-4)
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Wrong Drz values. Character", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  Drz[1] <- "profunda"
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Physically impossible AWC values", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  AWC[1] <- 0
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Wrong AWC values. Single number", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  AWC <- 0.4
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})

test_that("Rain and ET0 different lengths", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  ET0 <- ET0[1:20]
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  AWC <- 0.4
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      start.date = "2023-11-23"
    ),
    "Inputs must be numerical variables with no missing value.
        Also check if the input are physically sound."
  )
})


test_that("Wrong `InitialD` larger than `TAW`", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      InitialD = 80,
      start.date = "2023-11-23"
    ),
    "`InitialD` must be a single positive number no larger than `TAW`."
  )
})

test_that("Wrong InitialD negative value", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      InitialD = -80,
      start.date = "2023-11-23"
    ),
    "`InitialD` must be a single positive number no larger than `TAW`."
  )
})

test_that("Wrong InitialD length", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      InitialD = c(80, 40),
      start.date = "2023-11-23"
    ),
    "`InitialD` must be a single positive number no larger than `TAW`."
  )
})

test_that("Wrong InitialD character", {
  Tavg <- DataForCWB[, 2]
  Tmax <- DataForCWB[, 3]
  Tmin <- DataForCWB[, 4]
  Rn <- DataForCWB[, 6]
  WS <- DataForCWB[, 7]
  RH <- DataForCWB[, 8]
  G <- DataForCWB[, 9]
  ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS, G)
  Rain <- DataForCWB[, 10]
  Drz <- DataForCWB[, 11]
  AWC <- DataForCWB[, 12]
  MAD <- DataForCWB[, 13]
  Kc <- DataForCWB[, 14]
  Irrig <- DataForCWB[, 15]
  expect_error(
    tes <- CWB(
      Rain = Rain,
      ET0 = ET0,
      AWC = AWC,
      Drz = Drz,
      MAD = MAD,
      Kc = Kc,
      Irrig = Irrig,
      InitialD = c("Berger"),
      start.date = "2023-11-23"
    ),
    "`InitialD` must be a single positive number no larger than `TAW`."
  )
})
