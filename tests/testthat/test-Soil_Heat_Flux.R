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
