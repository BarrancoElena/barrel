library(testthat)

test_that("ord_ellipse_group returns ellipse dataframe with correct columns", {
  library(vegan)
  library(robustbase)
  data(dune)
  data(dune.env)

  scores <- as.data.frame(vegan::scores(rda(dune), display = "sites"))
  scores$Group <- dune.env$Management
  grp_name <- unique(scores$Group)[1]

  df_classic <- ord_ellipse_group(scores, "Group", grp_name, "PC1", "PC2", method = "classic")
  df_robust <- ord_ellipse_group(scores, "Group", grp_name, "PC1", "PC2", method = "robust")

  expect_true(all(c("PC1", "PC2", "Group") %in% colnames(df_classic)))
  expect_true(all(c("PC1", "PC2", "Group") %in% colnames(df_robust)))
  expect_true(nrow(df_classic) > 0)
})
