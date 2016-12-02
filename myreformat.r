# Cleaning out the workspace
rm (list = ls())
# setwd() to the dir where the script have been stored
if (! require("formatR")) install.packages("formatR")
library(formatR)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#use the library "formatR"

# tidy_eval("file path")
tidy_source("test_reformat/CIR6.R", keep.source = TRUE)

