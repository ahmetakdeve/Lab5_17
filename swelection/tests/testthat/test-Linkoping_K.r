context("Linkoping_K")

library(testthat)

test_that("outputs of votes_spec should be a data.frame.",{
  expect_true(class(votes_spec("0580K")) == "data.frame")
})

test_that("",{
  expect_true(class(votes_sum("00K")) == "data.frame")
})

test_that("linkoping_k[1,1] should be 05803101",{
  expect_equal(votes_spec("0580K")[1,1], "05803101")
  
})

test_that("linkoping_k[2,2] should be Bankekind",{
  expect_equal(votes_spec("0580K")[2,2], "Bankekind")
  
})

test_that("linkoping_k[3,3] should be FP",{
  expect_equal(votes_spec("0580K")[3,3], "FP")
  
})

test_that("sum_k[1,1] should be 1082",{
  expect_equal(votes_sum("00K")[1,1], "1082")
  
})

test_that("sum_k[2,2] should be Karlskrona",{
  expect_equal(votes_sum("00K")[2,2], "Karlskrona")
  
})

test_that("sum_k[3,3] should be 9,9",{
  expect_equal(votes_sum("00K")[3,3], "9,9")
  
})

test_that("the strcture of linkoping_k should have 855 obs of 4 vars",{
  expect_output(str(votes_spec("0580K")), "855 obs. of  4 variables")
})

test_that("the strcture of sum_k should have 290 obs of 11 vars",{
  expect_output(str(votes_sum("00K")), "290 obs. of  11 variables")
})

