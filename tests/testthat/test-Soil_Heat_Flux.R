test_that("Soil_Heat_Flux() works as expected in example", {
  G <- Soil_Heat_Flux(Tavg=DataForCWB[,2])
  expect_length(G, 129)
  expect_equal(
    G[1:10,1],
    c(
      NA,
      NA,
      NA,
       0.3806333333,
      -0.7796333333,
      -0.2007666667,
       0.7365666667,
       0.8170000000,
      -0.0969000000,
       0.1051333333
    ),
    tolerance = 0.01
  )})

test_that("Tavg. Wrong format", {
  Tavg <- cbind(DataForCWB[,2],DataForCWB[,2])
  expect_error(
    G <- Soil_Heat_Flux(Tavg=Tavg),
    "Tavg must be a single column variable with no missing value")
})

test_that("Tavg. Wrong format", {
  Tavg <- DataForCWB[,2]
  Tavg[1] <- NA
  expect_error(
    G <- Soil_Heat_Flux(Tavg=Tavg),
    "Tavg must be a single column variable with no missing value")
})

test_that("Tavg. Wrong format", {
  Tavg <- DataForCWB[1:3,2]
  expect_error(
    G <- Soil_Heat_Flux(Tavg=Tavg),
    "At least four days of Tavg are required.")
})
