context("AsociationRules")

test_that("Test AsociationRules with EGATUR dataset.", {

  modelFit <- AsociationRules(EGATUR[,c(2,4,5,8)])
  expect_equal(class(modelFit), "MLA")
  expect_equal(modelFit[[1]], "Association")

})
