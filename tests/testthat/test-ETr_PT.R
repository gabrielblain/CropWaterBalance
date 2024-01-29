test_that("ETr_PT() works as expected in example", {
  Tavg <- DataForCWB[,2]
  Rn <- DataForCWB[,4]
  G <- DataForCWB[,9]
  ETr <- ETr_PT(Tavg, Rn,G)
  expect_length(ETr, 129)
  expect_equal(
    ETr[1:10,1],
    c(
      7.106369,
      6.949454,
      7.045719,
      6.672529,
      6.393929,
      6.010509,
      6.466821,
      7.404860,
      7.372624,
      7.495234
    ),
    tolerance = 0.01
  )})
