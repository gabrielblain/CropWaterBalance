test_that("Campare() works as expected in example", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Ra <- DataForCWB[,5]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  Method1 <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Method2 <- ETr_PT(Tavg, Rn,G)
  G <- Compare(Method1=Method1, Method2=Method2)
  expect_s3_class(G, "data.frame")
  expect_length(G, 6)
  expect_equal(nrow(G), 1)
  expect_named(
    G,
    c("AME",
      "RMSE",
      "dorig",
      "dmod",
      "dref",
      "RQuad"))
  expect_equal(G[, "AME"], c(2.407347),tolerance = 0.01)
  expect_equal(G[, "RMSE"], c(2.604165),tolerance = 0.01)
  expect_equal(G[, "dorig"], c(0.5034527),tolerance = 0.01)
  expect_equal(G[, "dmod"], c(0.2969626),tolerance = 0.01)
  expect_equal(G[, "dref"], c(-0.155201),tolerance = 0.01)
  expect_equal(G[, "RQuad"], c(0.4294195),tolerance = 0.01)
})

test_that("Compare errors when there is just one Method", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Ra <- DataForCWB[,5]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  Method1 <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  expect_error(
    G <- Compare(Method1=Method1, Method2="Method2"),
    "Methods 1 and 2 must be numerical single column variables with at least 5 records each. Missing data are not allowed."
  )
})

test_that("Compare errors when there is just one Method", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Ra <- DataForCWB[,5]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  Method1 <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Method2 <- ETr_PT(Tavg, Rn,G)
  expect_error(
    G <- Compare(Method1=Method1[1:4,1], Method2=Method2[1:4,1]),
    "Methods 1 and 2 must be numerical single column variables with at least 5 records each. Missing data are not allowed."
  )
})

test_that("Compare errors when there is just one Method", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Ra <- DataForCWB[,5]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  Method1 <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  Method2 <- ETr_PT(Tavg, Rn,G)
  expect_error(
    G <- Compare(Method1=Method1, Method2=Method2[1:4,1]),
    "Methods 1 and 2 must be numerical single column variables with at least 5 records each. Missing data are not allowed."
  )
})

test_that("Compare errors when there is just one Method", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Ra <- DataForCWB[,5]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  Method1 <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  expect_error(
    G <- Compare(Method1=Method1[1:5,1], Method2=c(NA,1,2,3,4)),
    "Methods 1 and 2 must be numerical single column variables with at least 5 records each. Missing data are not allowed."
  )
})
test_that("Compare errors when there is just one Method", {
  Tavg <- DataForCWB[,2]
  Tmax <- DataForCWB[,3]
  Tmin <- DataForCWB[,4]
  Ra <- DataForCWB[,5]
  Rn <- DataForCWB[,6]
  WS <- DataForCWB[,7]
  RH <- DataForCWB[,7]
  G <- DataForCWB[,9]
  Method1 <- ETr_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
  expect_error(
    G <- Compare(Method1=Method1[1:5,1], Method2=c(1,2,3,4)),
    "Methods 1 and 2 must be numerical single column variables with at least 5 records each. Missing data are not allowed."
  )
})
