library(dplyr)

data <- demo_brain_trt %>% select("ROI.1")
subID <- demo_brain_trt %>% select("subID")
session <- demo_brain_trt %>% select("visit")

test_that("discriminability_wraper", {
  discr <- discriminability_wraper(data, subID, session, method.dist = "euclidean")
  expect_equal(discr$Discr[1,1],      0.55655737704918029074)
})

test_that("fingerprinting_wraper", {
  irate <- fingerprinting_wraper(data, subID, session,  method.dist = "euclidean")
  expect_equal(irate$FP[1,1],      0.03225806451612903136)
})
