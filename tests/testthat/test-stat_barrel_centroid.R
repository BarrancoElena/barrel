library(testthat)

test_that("stat_barrel_centroid draws centroids with classic and robust methods", {
  library(ggplot2)
  library(vegan)
  library(robustbase)

  data(dune)
  data(dune.env)
  pca <- rda(dune, scale = TRUE)
  scores <- as.data.frame(vegan::scores(pca, display = "sites"))
  scores$Group <- dune.env$Management

  p <- ggplot(scores, aes(x = PC1, y = PC2, group = Group, color = Group)) +
    stat_barrel_centroid(method = "classic")
  expect_s3_class(p, "ggplot")

  p2 <- ggplot(scores, aes(x = PC1, y = PC2, group = Group, color = Group)) +
    stat_barrel_centroid(method = "robust")
  expect_s3_class(p2, "ggplot")
})
