test_that("Descriptive() works as expected in example", {
  G <- Descriptive(Sample = DataForCWB[, 10])
  expect_s3_class(G, "data.frame")
  expect_length(G, 8)
  expect_equal(nrow(G), 1)
  expect_named(G,
               c(
                 "SampleSize",
                 "Avg",
                 "Med",
                 "SD",
                 "SE",
                 "MaxValue",
                 "MinValue",
                 "FreqZero%"
               ))
  expect_equal(G[, "SampleSize"], c(129), tolerance = 0)
  expect_equal(G[, "Avg"], c(6.53), tolerance = 0.01)
  expect_equal(G[, "Med"], c(0.25), tolerance = 0.01)
  expect_equal(G[, "SD"], c(13.06), tolerance = 0.01)
  expect_equal(G[, "SE"], c(1.15), tolerance = 0.01)
  expect_equal(G[, "MaxValue"], c(71.37), tolerance = 0.01)
  expect_equal(G[, "MinValue"], c(0), tolerance = 0.01)
  expect_equal(G[, "FreqZero%"], c(48.06), tolerance = 0.01)
})
test_that("Descriptive errors when Samples has negative values", {
  expect_error(G <- Descriptive(Sample = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -10)),
               "Negative or missing data in the sample is not allowed.")
})

test_that("Descriptive errors when Samples has less than 10 data", {
  expect_error(
    Descriptive(Sample = c(0, 1, 2, 3)),
    "Sample must be a numerical single-column variable with at least
         10 records."
  )
})

test_that("Descriptive errors when Samples has less than 10 data", {
  expect_error(Descriptive(Sample = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA)),
               "Negative or missing data in the sample is not allowed.")
})
