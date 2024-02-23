test_that("InitialD() works as expected in example", {
 teta_FC <- 0.30
 teta_Obs <- 0.17
 Drz <- 0.3048
  tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz)
  expect_length(tes, 1)
  expect_equal(tes,39.624,tolerance = 0.01)
})

test_that("Physically impossible teta_FC", {
  teta_FC <- -0.30
  teta_Obs <- 0.17
  Drz <- 0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("Missing teta_FC", {
  teta_FC <- NA
  teta_Obs <- 0.17
  Drz <- 0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("teta_FC is character", {
  teta_FC <- "character"
  teta_Obs <- 0.17
  Drz <- 0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("teta_FC wrong length", {
  teta_FC <- c(0.30, 0.30)
  teta_Obs <- 0.17
  Drz <- 0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("Physically impossible teta_Obs", {
  teta_FC <- 0.30
  teta_Obs <- -0.17
  Drz <- 0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("Missing teta_Obs", {
  teta_FC <- 0.30
  teta_Obs <- NA
  Drz <- 0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("teta_Obs is character", {
  teta_FC <- 0.30
  teta_Obs <- "character"
  Drz <- 0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("teta_Obs wrong length", {
  teta_FC <- 0.30
  teta_Obs <- c(0.30, 0.30)
  Drz <- 0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("Physically impossible Drz", {
  teta_FC <- 0.30
  teta_Obs <- 0.17
  Drz <- -0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("Missing Drz", {
  teta_FC <- 0.30
  teta_Obs <- 0.17
  Drz <- NA
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("Drz is character", {
  teta_FC <- 0.30
  teta_Obs <- 0.17
  Drz <- "character"
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("Drz wrong length", {
  teta_FC <- 0.30
  teta_Obs <- 0.17
  Drz <- c(0.3048,0.30)
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})

test_that("teta_Obs > teta_FC", {
  teta_FC <- 0.30
  teta_Obs <- 0.301
  Drz <- 0.3048
  expect_error(
    tes <- Dinitial(teta_FC=teta_FC,teta_Obs=teta_Obs,Drz=Drz),
    "Physically impossible or missing teta_FC,teta_Ob or Drz values.
         Also: teta_Obs cannot be larger than teta_FC"
  )
})
