CINormSigma <- setClass("CINormSigma",
                     contains = "CINormMu",
                     # Define the slots
                     # slots = c(),
                     # Set the default values for the slots.
                     # prototype = list(),
                     # Make a function that can test to see if the data is consistent.
                     # This is not called if you have an initialize function defined!
                     validity = function(object) {
                       if(object@alpha < 0 && object@alpha > 1) {
                         return("Alpha is out of bounds.")
                         }
                       return(TRUE)
                       }
                     )




# Building confidence intervals as a method
setGeneric(name <- "doAll",
           def  <- function(object, 
                            is_csv_output, 
                            is_confint_image,
                            is_cre_image) {
             standardGeneric("doAll")
           } 
)

setMethod(f <- "doAll",
          signature <- signature("CINormSigma", 
                                 "logical", 
                                 "logical", 
                                 "logical"),
          definition <- function(object, 
                                 is_csv_output, 
                                 is_confint_image,
                                 is_cre_image) {
            # object <- callNextMethod(object)
            sigmaSq = getSigma(object)^2
            leftQuantile  = qchisq (p = getAlpha(object)/2, df = getSampleSize(object) - 1)
            rightQuantile = qchisq (p = 1 - getAlpha(object)/2, df = getSampleSize(object) - 1)
            
            x = replicate(n = getSuperReplicationCount(object), 
                          expr = replicate(n = getSampleSize(object), 
                                           expr = rnorm(getReplicationCount(object), 
                                                        getMu(object), 
                                                        getSigma(object))))
            xBar = apply (x, c(1, 3), mean)
            xCentered = sweep(x, c(1, 3), xBar) # subtract bar from x
            xCenteredByMu = sweep(x, c(1, 3), getMu(object)) # subtract mu
            xCenteredSq = xCentered^2 
            # create a column vector with row sums of x.centered.sq
            ss = apply(xCenteredSq, c(1, 3), sum) # 
            # testing - should be close to s2.
            sigmaSqHat = ss / getSampleSize(object)
            # marginOfError = tQuantile * sqrt (sigmaSqHat / getSampleSize(object))
            
            confintLower  = getSampleSize(object)  / rightQuantile
            confintUpper  = getSampleSize(object)  / leftQuantile
            confintWidth  = confintUpper - confintLower
            
            isSigmaSqCovered = (sigmaSq < confintUpper & sigmaSq > confintLower)
            sumCovered = apply(isSigmaSqCovered, 2, sum)
            
            coverageRatio = sumCovered / getReplicationCount(object) 
            coverageRatioError = coverageRatio + getAlpha(object) - 1
            
            finalInfo <- cbind(as.vector(confintLower),
                                as.vector(confintUpper),
                                as.vector(isSigmaSqCovered),
                               as.vector(coverageRatioError),
                                getSuperReplicationCount(object),
                                getReplicationCount(object),
                                getSampleSize(object),
                                getAlpha(object),
                                getSigma(object),
                                getMu(object))
            colnames(finalInfo) <- c("confintLower",
                                      "confintUpper",
                                      "isSigmaSqCovered",
                                      "coverageRatioError",
                                      "superReplicationCount",
                                      "replicationCount",
                                      "sampleSize",
                                      "alpha",
                                      "sigma",
                                      "mu")
            finalInfoLength <- length(finalInfo)
            
            paramsInfo <- paste("src", getSuperReplicationCount(object),
                                "rc",  getReplicationCount(object),
                                "ss",  getSampleSize(object),
                                "a",   getAlpha(object),
                                "sigma", getSigma(object),
                                "mu",  getMu(object), sep = "_")
            
            
            if (is_csv_output == T) {
              dir.create(file.path("./", "csv_output"), showWarnings = FALSE)
              finalInfoCsvFullName = paste0("confintNormSigma_", paramsInfo, ".csv")
              finalInfoCsvPath <- file.path("./", "csv_output", finalInfoCsvFullName)
              write.csv(finalInfo, finalInfoCsvPath)
            }
            
            if (is_confint_image == T) {
              # Plotting confints
              dir.create(file.path("./", "plots"), showWarnings = FALSE)
              fullName = paste0("confintNormSigma_", paramsInfo, ".jpeg")
              plotPath<- file.path("./", "plots", fullName)
              
              jpeg(plotPath)
              plot (finalInfo[, 1], col = "dark blue", 
                    type = "l",  
                    ylab = "intervals", ylim = range(min(finalInfo[, 1]), max(finalInfo[, 2])))
              lines (finalInfo[, 2], col = "blue")
              abline(h = getSigma(object), col = "dark gray", lty = 5)
              cl = c("red", "green")
              arrows(1:finalInfoLength, confintLower[1:finalInfoLength],
                     1:finalInfoLength, confintUpper[1:finalInfoLength],
                     # length = par("din")[1]/getrc(object) * 0.5,
                     angle = 90, code = 0, lwd = 0.1,
                     col = cl[isSigmaSqCovered[1:finalInfoLength] + 1])
              sub.title = paste0("src = ", getSuperReplicationCount(object), ", ",
                                 "rc  = ", getReplicationCount(object),  ", ",
                                 "ss  = ", getSampleSize(object),  ", ", 
                                 "a   = ", getAlpha(object),  ", ",
                                 "sigma = ", getSigma(object),  ", ")
              title(main = "Confidence intervals", sub = sub.title,  cex.main = 1, cex.sub = 0.7)
              if (dev.list != NULL) dev.off()
            }
              
            if (is_cre_image == T) {
              # Plotting coverage.ratio.error
              # Exporting the plot to a file stored in a "./plots" folder
              dir.create(file.path("./", "plots"), showWarnings = FALSE)
              fullName <- paste0("coverageRatioErrorNormSigma_", paramsInfo, ".jpeg")
              plotPath<- file.path("./", "plots", fullName)
              jpeg(plotPath)
              plot (coverageRatioError, ylab = "difference",
                    ylim = range(min(coverageRatioError), max(coverageRatioError)))
              title(main = "Difference between supposed and actual coverage probability 
                    in a confidence interval for a variance of normal distribution", 
                    sub = sub.title,  cex.main = 1, cex.sub = 1)
              dev.off()
            }
            
            assign('finalInfo', finalInfo, envir = parent.frame())
          }
)



