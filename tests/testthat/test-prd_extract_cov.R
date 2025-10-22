library(testthat)

test_that("ord_extract_cov returns 2x2 matrix for classic and robust methods", {
  library(vegan)
  library(robustbase)
  data(dune)
  data(dune.env)

  scores <- as.data.frame(vegan::scores(rda(dune), display = "sites"))
  scores$Group <- dune.env$Management

  cov_classic <- ord_extract_cov(scores, "PC1", "PC2", method = "classic")
  cov_robust <- ord_extract_cov(scores, "PC1", "PC2", method = "robust")

  expect_true(is.matrix(cov_classic))
  expect_true(is.matrix(cov_robust))
  expect_equal(dim(cov_classic), c(2, 2))
  expect_equal(dim(cov_robust), c(2, 2))
})

test_that("ord_extract_cov returns a 2x2 covariance matrix", {
  df <- data.frame(A1 = rnorm(10), A2 = rnorm(10))
  cov_mat <- ord_extract_cov(df, "A1", "A2", method = "classic")
  expect_true(is.matrix(cov_mat))
  expect_equal(dim(cov_mat), c(2, 2))
})

test_that("ord_extract_cov warns when robust skipped due to low n", {
  library(vegan)
  data(dune)
  ord <- rda(dune)
  scores <- as.data.frame(scores(ord, display = "sites"))
  df <- scores[1:2, c(1, 2)]

  expect_warning(
    {
      cov_mat <- ord_extract_cov(df, names(df)[1], names(df)[2], method = "robust")
      expect_true(is.matrix(cov_mat))
      expect_equal(dim(cov_mat), c(2, 2))
    },
    regexp = "falling back to classic"
  )
})
