library(testthat)

test_that("ord_extract_centroids returns centroids with correct columns", {
  library(vegan)
  library(robustbase)
  data(dune)
  data(dune.env)

  scores <- as.data.frame(vegan::scores(rda(dune), display = "sites"))
  scores$Group <- dune.env$Management

  cent_classic <- ord_extract_centroids(scores, "Group", "PC1", "PC2", method = "classic")
  cent_robust <- ord_extract_centroids(scores, "Group", "PC1", "PC2", method = "robust")

  expect_true(all(c("Group", "PC1", "PC2") %in% colnames(cent_classic)) ||
                all(c("group", "PC1", "PC2") %in% colnames(cent_robust)))
})
