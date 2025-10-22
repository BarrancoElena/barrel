test_that("barrel_prepare works and errors as expected", {
  library(vegan)
  data(dune)
  data(dune.env)

  ord <- rda(dune)
  out <- barrel_prepare(ord, dune.env)
  expect_s3_class(out, "barrel_ord")
  expect_equal(attr(out, "barrel_meta"), dune.env)

  expect_error(
    barrel_prepare(lm(mpg ~ wt, data = mtcars), dune.env),
    "Unsupported ordination object"
  )

  bad_meta <- dune.env[-1, ]
  expect_error(barrel_prepare(ord, bad_meta), "Rows in ordination scores")

  expect_message(
    suppressWarnings(barrel_prepare(ord, dune.env)),
    regexp = "barrel_prepare\\(\\) successful"
  )
})
