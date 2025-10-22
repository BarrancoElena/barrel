library(testthat)

test_that("ord_ellipse_groups returns combined ellipse dataframe with groups", {
  library(vegan)
  library(robustbase)
  data(dune)
  data(dune.env)

  scores <- as.data.frame(vegan::scores(rda(dune), display = "sites"))
  scores$Group <- dune.env$Management

  df_classic <- ord_ellipse_groups(scores, "Group", "PC1", "PC2", method = "classic")
  df_robust <- ord_ellipse_groups(scores, "Group", "PC1", "PC2", method = "robust")

  expect_true(all(c("PC1", "PC2", "Group") %in% colnames(df_classic)))
  expect_true(all(c("PC1", "PC2", "Group") %in% colnames(df_robust)))
  expect_equal(length(unique(df_classic$Group)), length(unique(scores$Group)))
})
