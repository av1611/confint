# class definition --------------------- 
CINormMu <- setClass("CINormMu",
                     contains = "CI",
                     # Define the slots
                     slots = c(mu = "numeric",
                               sigma = "numeric"),
                     # Set the default values for the slots.
                     prototype = list(mu = 0,
                                      sigma = 1),
                     # Make a function that can test to see if the data is consistent.
                     # This is not called if you have an initialize function defined!
                     validity = function(object) {
                       if(object@alpha < 0 && object@alpha > 1) {
                         return("Alpha is out of bounds.")
                         }
                       return(TRUE)
                       }
                     )



# mu ----------------------------------------------------------------------


# create a method to assign and get the mu parameter (Mu)
setGeneric(name <- "setMu", 
           def <- function(object, mu) { 
             standardGeneric("setMu") 
             } 
           )

setMethod(f <- "setMu",
          signature <- "CINormMu",
          definition <- function(object, mu) {
            object@mu <- mu
            validObject(object)
            return(object)
            }
          )

setGeneric(name <- "getMu", 
           def <- function(object) { 
             standardGeneric("getMu") 
           } 
)

setMethod(f <- "getMu",
          signature <- "CINormMu",
          definition <- function(object) {
            return(object@mu)
          }
)


# sigma -------------------------------------------------------------------


# create a method to assign and get the sigma parameter (Sigma)
setGeneric(name <- "setSigma", 
           def <- function(object, sigma) { 
             standardGeneric("setSigma") 
             } 
           )

setMethod(f <- "setSigma",
          signature <- "CINormMu",
          definition <- function(object, sigma) {
            object@sigma <- sigma
            validObject(object)
            return(object)
            }
          )

setGeneric(name <- "getSigma", 
           def <- function(object) { 
             standardGeneric("getSigma") 
             } 
           )

setMethod(f <- "getSigma",
          signature <- "CINormMu",
          definition <- function(object) {
            return(object@sigma)
            }
          )


# x -----------------------------------------------------------------------


setGeneric(name <- "createX",
           def  <- function(object) {
             standardGeneric("createX")
           } 
)

# Building confidence intervals as a method
setMethod(name <- "createX",
          signature <- "CINormMu",
          def <- function(object) {
            x <- replicate(n = getSuperReplicationCount(object),
                           expr = replicate(n = getSampleSize(object),
                                            expr = rnorm(getReplicationCount(object),
                                                         getMu(object),
                                                         getSigma(object))))
            object@x <- x
            return(object)
          }
)

setGeneric(name <- "getX",
           def <- function(object) {
             standardGeneric("getX")
           }
)

setMethod(f <- "getX",
          signature <- "CINormMu",
          definition <- function(object) {
            return(object@x)
          }
)



# arraysToSave ----------------------------------------------------------------------


setGeneric(name <- "createArraysToSave",
           def  <- function(object) {
             standardGeneric("createArraysToSave")
           } 
)

# Building confidence intervals as a method
setMethod(name <- "createArraysToSave",
          signature <- "CINormMu",
          def <- function(object) {
            createX(object)
            object <- createX(object)
            sigmaSq = getSigma(object)^2
            tQuantile  = - qt (p = getAlpha(object)/2, df = getSampleSize(object) - 1)
            xBar = apply (getX(object), c(1, 3), mean)
            xCentered = sweep(getX(object), c(1, 3), xBar) # subtract bar from x
            xCenteredByMu = sweep(getX(object), c(1, 3), getMu(object)) # subtract mu
            xCenteredSq = xCentered^2 
            # create a column vector with row sums of x.centered.sq
            ss = apply(xCenteredSq, c(1, 3), sum) # 
            # testing - should be close to s2.
            sigmaSqHat = ss / getSampleSize(object)
            marginOfError = tQuantile * sqrt (sigmaSqHat / getSampleSize(object))
            confintLower  = xBar - marginOfError
            confintUpper  = xBar + marginOfError
            confintWidth  = confintUpper - confintLower
            isMuCovered = (getMu(object) < confintUpper & getMu(object) > confintLower)
            sumCovered = apply(isMuCovered, 2, sum)
            coverageRatio = sumCovered / getReplicationCount(object) 
            coverageRatioError = coverageRatio + getAlpha(object) - 1
            arraysToSave <- cbind(as.vector(confintLower),
                               as.vector(confintUpper),
                               as.vector(isMuCovered),
                               as.vector(coverageRatioError),
                               getSuperReplicationCount(object),
                               getReplicationCount(object),
                               getSampleSize(object),
                               getAlpha(object),
                               getSigma(object),
                               getMu(object))
            colnames(arraysToSave) <- c("confintLower",
                                      "confintUpper",
                                      "isMuCovered",
                                      "coverageRatioError",
                                      "superReplicationCount",
                                      "replicationCount",
                                      "sampleSize",
                                      "alpha",
                                      "sigma",
                                      "mu")
            object@arraysToSave <- arraysToSave
            # object <- createArraysToSave(object)
            validObject(object)
            return(object)
           } 
)

setGeneric(name <- "getArraysToSave", 
           def <- function(object) { 
             standardGeneric("getArraysToSave") 
           } 
)

setMethod(f <- "getArraysToSave",
          signature <- "CINormMu",
          definition <- function(object) {
            return(object@arraysToSave)
          }
)


# pathsAndNames  --------------------------------------------------------------------

setGeneric(name <- "createPathsAndNames",
           def  <- function(object) {
             standardGeneric("createPathsAndNames")
           } 
)


setMethod(f <- "createPathsAndNames",
          signature <- signature("CINormMu"),
          definition <- function(object) {
            # object <- createX(object)
            # object <- createArraysToSave(object)
            
            paramsInfo <- paste("src", getArraysToSave(object)[1, 5],
                                "rc",  getArraysToSave(object)[1, 6],
                                "ss",  getArraysToSave(object)[1, 7],
                                "a",   getArraysToSave(object)[1, 8],
                                "sigma", getArraysToSave(object)[1, 9],
                                "mu",  getArraysToSave(object)[1, 10], sep = "_")
            sub.title = paste0("src = ", getArraysToSave(object)[1, 5], ", ",
                               "rc  = ", getArraysToSave(object)[1, 6],  ", ",
                               "ss  = ", getArraysToSave(object)[1, 7],  ", ", 
                               "a   = ", getArraysToSave(object)[1, 8],  ", ",
                               "sigma = ", getArraysToSave(object)[1, 9],  ", ")
            
            arraysCsvName = paste0("confintNormMu_", paramsInfo, ".csv")
            arraysCsvPath <- file.path("./", "csv_output", arraysCsvName)
            
            # Plotting confints
            dir.create(file.path("./", "plots"), showWarnings = FALSE)
            saveImageCIName <- paste0("confintNormMu_", paramsInfo, ".jpeg")
            saveImageCIPath <- file.path("./", "plots", saveImageCIName)
              
            # Plotting coverage.ratio.error
            # Exporting the plot to a file stored in a "./plots" folder
            dir.create(file.path("./", "plots"), showWarnings = FALSE)
            saveImageCREName <- paste0("coverageRatioErrorNormMu_", paramsInfo, ".jpeg")
            saveImageCREPath<- file.path("./", "plots", saveImageCREName)
            
            pathsAndNames <- list(arraysCsvPath, saveImageCIPath, saveImageCREPath)
            
            object@pathsAndNames <- pathsAndNames
            validObject(object)
            return(object)
            
            # assign('finalInfo',finalInfo,envir=parent.frame())
          }
)

setGeneric(name <- "getPathsAndNames",
           def <- function(object) {
             standardGeneric("getPathsAndNames")
           }
)

setMethod(f <- "getPathsAndNames",
          signature <- "CINormMu",
          definition <- function(object) {
            return(object@pathsAndNames)
          }
)

# ++++++++++++++ methods +++++++++++++++


# writeCSV ----------------------------------------------------------------

setGeneric(name <- "saveCSV",
           def  <- function(object) {
             standardGeneric("saveCSV")
           } 
)

setMethod(f <- "saveCSV",
          signature <- signature("CINormMu"),
          definition <- function(object) {
            
          }
)


# saveImageCI -------------------------------------------------------------

setGeneric(name <- "saveImageCI",
           def  <- function(object) {
             standardGeneric("saveImageCI")
           } 
)

setMethod(f <- "saveImageCI",
          signature <- signature("CINormMu"),
          definition <- function(object) {
            dir.create(file.path("./", "plots"), showWarnings = FALSE)
            finalInfoLength <- length(getArraysToSave(object))
            sub.title = paste0("src = ", getSuperReplicationCount(object), ", ",
                               "rc  = ", getReplicationCount(object),  ", ",
                               "ss  = ", getSampleSize(object),  ", ", 
                               "a   = ", getAlpha(object),  ", ",
                               "mu = ", getMu(object),  ", ")
            
            jpeg(getPathsAndNames(object)[2])
            plot (getArraysToSave(object)[, 1], col = "dark blue", 
                  type = "l",  
                  ylab = "intervals", ylim = range(min(getArraysToSave(object)[, 1]), 
                                                   max(getArraysToSave(object)[, 2])))
            lines (getArraysToSave(object)[, 2], col = "blue")
            abline(h = getSigma(object), col = "dark gray", lty = 5)
            cl = c("red", "green")
            arrows(1:finalInfoLength, getArraysToSave(object)[, 1][1:finalInfoLength], # confint lower
                   1:finalInfoLength, getArraysToSave(object)[, 2][1:finalInfoLength], # confint upper
                   # length = par("din")[1]/getrc(object) * 0.5,
                   angle = 90, code = 0, lwd = 0.1,
                   col = cl[getArraysToSave(object)[, 3][1:finalInfoLength] + 1])
            title(main = "Confidence intervals", sub = sub.title,  cex.main = 1, cex.sub = 0.7)
            graphics.off()
          }
)


# saveImageCRE -------------------------------------------------------------

setGeneric(name <- "saveImageCRE",
           def  <- function(object) {
             standardGeneric("saveImageCRE")
           } 
)

setMethod(f <- "saveImageCRE",
          signature <- signature("CINormMu"),
          definition <- function(object) {
            dir.create(file.path("./", "plots"), showWarnings = FALSE)
            sub.title = paste0("src = ", getSuperReplicationCount(object), ", ",
                               "rc  = ", getReplicationCount(object),  ", ",
                               "ss  = ", getSampleSize(object),  ", ", 
                               "a   = ", getAlpha(object),  ", ",
                               "mu = ", getMu(object),  ", ")
            
            jpeg(getPathsAndNames(object)[3])
            plot (getArraysToSave(object)[, 4], ylab = "difference",
                  ylim = range(min(getArraysToSave(object)[, 4]), max(getArraysToSave(object)[, 4])))
            title(main = "Difference between supposed and actual coverage probability 
                  in a confidence interval for a variance of normal distribution", 
                  sub = sub.title,  cex.main = 1, cex.sub = 1)
            graphics.off()
          }
)




# saveAll --------------------------------------------------------------------

# Building confidence intervals as a method
setGeneric(name <- "saveAll",
           def  <- function(object, 
                            is_csv_output, 
                            is_confint_image, 
                            is_cre_image) {
             standardGeneric("saveAll")
           } 
)

setMethod(f <- "saveAll",
          signature <- signature("CINormMu", 
                                 "logical", 
                                 "logical", 
                                 "logical"),
          definition <- function(object, 
                                 is_csv_output, 
                                 is_confint_image,
                                 is_cre_image) {
            
            if (is_csv_output == T) {
              saveCSV(object)
            }
            
            if (is_confint_image == T) {
              saveImageCI(object)
            }
            
            if (is_cre_image == T) {
              saveImageCRE(object)
            }
          }
)

