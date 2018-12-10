context("Clustering")

test_that("Test Clustering with EGATUR dataset.", {

  modelFit <- Clustering(data=EGATUR[,c("A13","gastototal")])
  expect_equal(class(modelFit), "MLA")
  expect_equal(modelFit[[1]], "Clustering")

})
