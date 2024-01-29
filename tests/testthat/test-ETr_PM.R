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
