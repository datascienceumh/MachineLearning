context("VariableRanker")

test_that("Test VarRanker with EGATUR dataset.", {

  modelFit <- VariableRanker(GastoTotalD~pais+aloja+motivo,data=EGATUR)
  expect_equal(class(modelFit), "MLA")
  expect_equal(modelFit[[1]], "Var-Rank")

})
