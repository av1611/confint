CIBernoulli <- setClass(
  # Set the name for the class
  "CIBernoulli",
  contains = "CI",
  
  # Define the slots
  slots = c(p = "numeric"),
  
  # Set the default values for the slots. (optional)
  prototype = list (p = 0.5),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity = function(object)
  {
    if(object@p < 0 | object@p > 1 | 
       object@alpha < 0 | object@alpha > 1 ) {
      return("Parameter p or alpha are out of bounds.")
    }
    return(TRUE)
  }
  
  
)


# create a method to assign and get the p parameter (P)
setGeneric(name <- "setP", 
           def <- function(object, p) { 
             standardGeneric("setP") 
           } 
)

setMethod(f <- "setP",
          signature <- "CIBernoulli",
          definition <- function(object, p) {
            object@p <- p
            validObject(object)
            return(object)
          }
)

setGeneric(name <- "getP", 
           def <- function(object) { 
             standardGeneric("getP") 
           } 
)

setMethod(f <- "getP",
          signature <- "CIBernoulli",
          definition <- function(object) {
            return(object@p)
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
          signature <- signature("CIBernoulli", 
                                 "logical", 
                                 "logical", 
                                 "logical"),
          definition <- function(object, 
                                 is_csv_output, 
                                 is_confint_image,
                                 is_cre_image) {
            # object <- callNextMethod(object)
            x = replicate(n = getSuperReplicationCount(object), 
                          expr = replicate(n = getSampleSize(object),
                                           expr = rbinom(n = getReplicationCount(object),
                                                         prob = getP(object),
                                                         size = 1)))
            xBar = apply (x, c(1, 3), mean)
            marginOrError = 1 / (2 * sqrt (getSampleSize(object) * getAlpha(object)))
            # smaller, but still guarantees probability
            marginOrError = sqrt (-log(getAlpha(object) / 2) / (2 * getSampleSize(object)))
            # compute margin of error
            confintLower  = xBar - marginOrError
            confintUpper  = xBar + marginOrError
            confintWidth  = confintUpper - confintLower
            isPCovered = (getP(object) < confintUpper & getP(object) > confintLower)
            sumCovered = apply(isPCovered, 2, sum)
            coverageRatio = sumCovered / getReplicationCount(object) 
            # should be of length = super.replication.count
            # vector minus scalar
            coverageRatioError = coverageRatio + getAlpha(object) - 1
            
            finalInfo <- cbind(as.vector(confintLower),
                               as.vector(confintUpper),
                               as.vector(isPCovered),
                               getSuperReplicationCount(object),
                               getReplicationCount(object),
                               getSampleSize(object),
                               getAlpha(object),
                               getP(object))
            colnames(finalInfo) <- c("confintLower",
                                     "confintUpper",
                                     "isMuCovered",
                                     "superReplicationCount",
                                     "replicationCount",
                                     "sampleSize",
                                     "alpha",
                                     "p")
            finalInfoLength <- length(finalInfo)
            
            paramsInfo <- paste("src", getSuperReplicationCount(object),
                                "rc",  getReplicationCount(object),
                                "ss", getSampleSize(object),
                                "p", getP(object),
                                "a", getAlpha(object), sep = "_")
            
            if (is_csv_output == T) {
              dir.create(file.path("./", "csv_output"), showWarnings = FALSE)
              finalInfoCsvFullName = paste0("confintBernoulli_", paramsInfo, ".csv")
              finalInfoCsvPath <- file.path("./", "csv_output", finalInfoCsvFullName)
              write.csv(finalInfo, finalInfoCsvPath)
            }
            
            if (is_confint_image == T) {
              # Plotting confints
              dir.create(file.path("./", "plots"), showWarnings = FALSE)
              fullName = paste0("confintBernoulli_", paramsInfo, ".jpeg")
              plotPath<- file.path("./", "plots", fullName)
              
              jpeg(plotPath)
              plot (finalInfo[, 1], col = "dark blue", 
                    type = "l",  
                    ylab = "intervals", ylim = range(min(finalInfo[, 1]), max(finalInfo[, 2])))
              lines (finalInfo[, 2], col = "blue")
              abline(h = getP(object), col = "dark gray", lty = 5)
              cl = c("red", "green")
              arrows(1:finalInfoLength, confintLower[1:finalInfoLength],
                     1:finalInfoLength, confintUpper[1:finalInfoLength],
                     # length = par("din")[1]/getRC(object) * 0.5,
                     angle = 90, code = 0, lwd = 0.1,
                     col = cl[isPCovered[1:finalInfoLength] + 1])
              sub.title = paste0("src = ", getSuperReplicationCount(object), ", ",
                                 "rc  = ", getReplicationCount(object),  ", ",
                                 "ss  = ", getSampleSize(object),  ", ", 
                                 "a   = ", getAlpha(object),  ", ",
                                 "p   = ", getP(object),  ", ")
              title(main = "Confidence intervals", sub = sub.title,  cex.main = 1, cex.sub = 0.7)
              dev.off()
            }
            
            if (is_cre_image == T) {
              # Plotting coverage.ratio.error
              # Exporting the plot to a file stored in a "./plots" folder
              dir.create(file.path("./", "plots"), showWarnings = FALSE)
              fullName <- paste0("coverageRatioErrorBernoulli_", paramsInfo, ".jpeg")
              plotPath<- file.path("./", "plots", fullName)
              jpeg(plotPath)
              plot (coverageRatioError, ylab = "difference",
                    ylim = range(min(coverageRatioError), max(coverageRatioError)))
              title(main = "Difference between supposed and actual coverage probability 
                            in a confidence interval for a mean of Bernoulli distribution", 
                    sub = sub.title,  cex.main = 1, cex.sub = 1)
              dev.off()
            }
            
            assign('finalInfo', finalInfo, envir = parent.frame())
          }
)





