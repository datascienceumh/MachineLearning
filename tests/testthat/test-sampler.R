context("sampler")

test_that("Test sampler with a easy example", {
  simply <- data.frame("R"=c(1,2,3,4,5,6,7,8,9),"Z"=rep("A",9))
  set.seed(22071993)
  sample <-  sampler(simply,p=0.7)
  sample2 <-  sampler(simply,p=0.7, seed=22071993)

  expect_equal(sample[[2]], c(5,6,9,4,8,1))
  expect_equal(sample[[2]], sample2[[2]])

})
