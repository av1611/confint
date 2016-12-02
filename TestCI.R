# Cleaning out the workspace
rm (list = ls())
# setwd() to the dir where the script have been stored
if (! require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Loading the function
source("CIR6.R")
source("CINormMuR6.R")
source("CINormSigmaR6.R")
source("CIBernoulliR6.R")
source("CIRhoMa1R6.R")

# Testing ciNormMu ------------------
ciNormMu <- CINormMuR6$new(
  superReplicationCount = 100,
  replicationCount = 150,
  sampleSize = 200,
  alpha = 0.25,
  sigma = 2,
  mu = 1)
ciNormMu
ciNormMu$createX()
ciNormMu$getX()
ciNormMu$createArraysToSave()
ciNormMu$getArraysToSave()

ciNormMu$createPathsAndNames()
ciNormMu$getPathsAndNames()

ciNormMu$createImageInfo()
ciNormMu$getImageInfo()
ciNormMu$getImageInfo()["subTitle"]

ciNormMu$getCRE()

ciNormMu$saveCSV()
ciNormMu$saveImageCI()
ciNormMu$saveImageCRE()
ciNormMu$doAll()

# Testing ciNormSigma ------------------
ciNormSigma <- CINormSigmaR6$new(
  superReplicationCount = 10,
  replicationCount = 500,
  sampleSize = 500,
  alpha = 0.25,
  sigma = 2,
  mu = 1)
ciNormSigma
ciNormSigma$createX()
ciNormSigma$getX()
ciNormSigma$createArraysToSave()
ciNormSigma$getArraysToSave()

ciNormSigma$createPathsAndNames()
ciNormSigma$getPathsAndNames()

ciNormSigma$createImageInfo()
ciNormSigma$getImageInfo()
ciNormSigma$getImageInfo()["subTitle"]

ciNormSigma$getCRE()

ciNormSigma$saveCSV()
ciNormSigma$saveImageCI()
ciNormSigma$saveImageCRE()
ciNormSigma$doAll()

# Testing ciBernoulli ------------------
ciBernoulli <- CIBernoulliR6$new(
  superReplicationCount = 10,
  replicationCount = 500,
  sampleSize = 500,
  alpha = 0.25,
  p = 0.5)
ciBernoulli
ciBernoulli$createX()
ciBernoulli$getX()
ciBernoulli$createArraysToSave()
ciBernoulli$getArraysToSave()
ciBernoulli$CRE
ciBernoulli$createPathsAndNames()
ciBernoulli$getPathsAndNames()

ciBernoulli$createImageInfo()
ciBernoulli$getImageInfo()
ciBernoulli$getImageInfo()["subTitle"]

ciBernoulli$getCRE()

ciBernoulli$saveCSV()
ciBernoulli$saveImageCI()
ciBernoulli$saveImageCRE()
ciBernoulli$doAll()


# Testing ciRhoMa1 ------------------
ciRhoMa1 <- CIRhoMa1R6$new(
  superReplicationCount = 10,
  replicationCount = 100,
  sampleSize = 100,
  mu = 0,
  sigma = 10,
  alpha = 0.05,
  theta = 0.5)
ciRhoMa1
ciRhoMa1$createX()
ciRhoMa1$getX()
ciRhoMa1$createArraysToSave()
ciRhoMa1$getArraysToSave()
ciRhoMa1$CRE
ciRhoMa1$createPathsAndNames()
ciRhoMa1$getPathsAndNames()

ciRhoMa1$createImageInfo()
ciRhoMa1$getImageInfo()
ciRhoMa1$getImageInfo()["subTitle"]

ciRhoMa1$getCRE()

ciRhoMa1$saveCSV()
ciRhoMa1$saveImageCI()
ciRhoMa1$saveImageCRE()
ciRhoMa1$doAll()
