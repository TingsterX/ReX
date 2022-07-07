library(dplyr)

data <- demo_brain_trt %>% select("ROI.1")
subID <- demo_brain_trt %>% select("subID")
session <- demo_brain_trt %>% select("visit")
cov <- demo_brain_trt %>% select("age", "gender")

test_that("lme_ICC_1wayR", {
  icc1 <- lme_ICC_1wayR(data, subID[,1], session[,1], cov)
  expect_equal(icc1[1,"ICC"],      0.14251299074798579047)
  expect_equal(icc1[1,"ICCk"],     0.24947285834305432672)
  expect_equal(icc1[1,"sigma2_b"], 0.00333469737133181964)
  expect_equal(icc1[1,"sigma2_w"], 0.02006455454127987689)
  expect_equal(icc1[1,"var.data"], 0.02537166699920532717)
  expect_equal(icc1[1,"error.message"], 0)
})

test_that("lme_ICC_2wayR", {
  icc2 <- lme_ICC_2wayR(data, subID[,1], session[,1], cov)
  expect_equal(icc2[1,"ICC.a"],      0.17974406519166735707)
  expect_equal(icc2[1,"ICC.c"],      0.19682028202829462971)
  expect_equal(icc2[1,"ICCk.a"],     0.30471704922281633854)
  expect_equal(icc2[1,"ICCk.c"],     0.32890532519174248138)
  expect_equal(icc2[1,"sigma2_b"],   0.00439658926699800900)
  expect_equal(icc2[1,"sigma2_w"],   0.01794150120665531645)
  expect_equal(icc2[1,"sigma2_rep"], 0.00212218454187930103)
  expect_equal(icc2[1,"var.data"], 0.02537166699920532717)
  expect_equal(icc2[1,"error.message"], 0)
})

test_that("lme_ICC_2wayM", {
  icc3 <- lme_ICC_2wayM(data, subID[,1], session[,1], cov)
  expect_equal(icc3[1,"ICC.c"],      0.19679808927970915411)
  expect_equal(icc3[1,"ICCk.c"],     0.32887433735485277486)
  expect_equal(icc3[1,"sigma2_b"],   0.00439605483243466020)
  expect_equal(icc3[1,"sigma2_w"],   0.01794183903901724539)
  expect_equal(icc3[1,"var.data"],   0.02537166699920532717)
  expect_equal(icc3[1,"error.message"], 0)
})
