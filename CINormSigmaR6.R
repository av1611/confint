# install.packages("R6")
library(R6)

# Defining CIR6 class:  ---------------------------------------------
CINormSigmaR6 <- R6Class(
  "CINormSigmaR6",
  inherit = CIR6,
  public = list(
    # CIR6 class fields ---------------------------------------------
    superReplicationCount = "numeric",
    replicationCount      = "numeric",
    sampleSize            = "numeric",
    mu    = "numeric",
    sigma = "numeric",
    alpha = "numeric",
    x     = "array",
    arraysToSave  = "array",
    pathsAndNames = "list",
    imageInfo = "list",
    CRE = "array",
    
    # CIR6 class initialize method ---------------------------------------------
    initialize = function(superReplicationCount = 3,
                          replicationCount = 4,
                          sampleSize = 5,
                          mu = 0,
                          sigma = 1,
                          alpha = 0.05,
                          x = array(),
                          arraysToSave = array(),
                          pathsAndNames = list(),
                          imageInfo = list(),
                          CRE = array()) {
      self$superReplicationCount <- superReplicationCount
      self$replicationCount <- replicationCount
      self$sampleSize <- sampleSize
      self$mu <- mu
      self$sigma <- sigma
      self$alpha <- alpha
      self$x <- x
      self$arraysToSave <- arraysToSave
      self$pathsAndNames <- pathsAndNames
      self$imageInfo <- imageInfo
      self$CRE <- CRE
    },
    
    # CIR6 class methods ---------------------------------------------
    setSuperReplicationCount = function(value) {
      self$superReplicationCount <- value
    },
    getSuperReplicationCount = function() {
      self$superReplicationCount
    },
    setReplicationCount = function(value) {
      self$replicationCount <- value
    },
    getReplicationCount = function() {
      self$replicationCount
    },
    setSampleSize = function(value) {
      self$sampleSize <- value
    },
    getSampleSize = function() {
      self$sampleSize
    },
    setMu = function(value) {
      self$mu <- value
    },
    getMu = function() {
      self$mu
    },
    setSigma = function(value) {
      self$sigma <- value
    },
    getSigma = function() {
      self$sigma
    },
    setAlpha = function(value) {
      self$alpha <- value
    },
    getAlpha = function() {
      self$alpha
    },
    
    # X ---------------------------
    createX = function() {
      self$x <- replicate(
        n = self$getSuperReplicationCount(),
        expr = replicate(
          n = self$getSampleSize(),
          expr = rnorm(
            self$getReplicationCount(),
            mean = self$getMu(),
            sd = self$getSigma()
          )
        )
      )
    },
    getX = function() {
      self$x
    },
    getCRE = function() {
      self$CRE
    },
    
    # createArraysToSave ---------------------------
    createArraysToSave = function() {
      sigmaSq = self$getSigma()^2
      targetCoverageProb = 1 - self$getAlpha()
      leftQuantile  = qchisq(p = self$getAlpha()/2, df = self$getSampleSize() - 1)
      rightQuantile = qchisq(p = 1 - self$getAlpha()/2, df = self$getSampleSize() - 1)
      xBar = apply(self$getX(), c(1, 3), mean)
      xCentered = sweep(self$getX(), c(1, 3), xBar) # subtract bar from x
      xCenteredByMu = sweep(self$getX(), c(1, 3), self$getMu()) # subtract mu
      xCenteredSq = xCentered^2 
      # create a column vector with row sums of x.centered.sq
      ss = apply(xCenteredSq, c(1, 3), sum) # 
      # testing - should be close to s2.
      sigmaSqHat = ss / self$getSampleSize()
      confintLower  = ss / rightQuantile
      confintUpper  = ss / leftQuantile
      confintWidth  = confintUpper - confintLower
      isSigmaSqCovered = (sigmaSq < confintUpper & sigmaSq > confintLower)
      sumCovered = apply(isSigmaSqCovered, 2, sum)
      coverageRatio = sumCovered / self$getReplicationCount() 
      coverageRatioError = coverageRatio - targetCoverageProb
      # assigning CRE for CRE plot
      self$CRE <- coverageRatioError
      arraysToSave <- cbind(as.vector(confintLower),
                            as.vector(confintUpper),
                            as.vector(isSigmaSqCovered),
                            as.vector(coverageRatioError),
                            self$getSuperReplicationCount(),
                            self$getReplicationCount(),
                            self$getSampleSize(),
                            self$getAlpha(),
                            self$getSigma(),
                            self$getMu())
      colnames(arraysToSave) <- c("confintLower",
                                  "confintUpper",
                                  "isSigmaSqCovered",
                                  "coverageRatioError",
                                  "superReplicationCount",
                                  "replicationCount",
                                  "sampleSize",
                                  "alpha",
                                  "sigma",
                                  "mu")
      self$arraysToSave <- arraysToSave
    },
    getArraysToSave = function() {
      self$arraysToSave
    },
    
    # createImageInfo -------------------------
    createImageInfo = function() {
      # Creating main title for CI image
      self$imageInfo[1] <- "Confidence intervals for a variance of normal distribution"
      # Creating main title for CRE image
      self$imageInfo[2] <- "Difference between supposed and actual coverage probability
      in a confidence interval for a variance of normal distribution"
      # Creating sub-title for CI & CRE images
      subTitle = paste0("src = ", self$getSuperReplicationCount(), ", ",
                        "rc = ", self$getReplicationCount(), ", ",
                        "ss = ", self$getSampleSize(), ", ",
                        "a = ", self$getAlpha(), ", ",
                        "mu = ", self$getMu())
      self$imageInfo[3] <- subTitle
      # Creating value for abline function for CI image:
      # - sigma -- for CINormMu, CINormSigma
      # 
      names(self$imageInfo) <- c("titleImageCI", # self$imageInfo[1]
                                 "titleImageCRE",# self$imageInfo[2]
                                 "subTitle")     # self$imageInfo[3]
      },
     getImageInfo = function() {
      self$imageInfo
    },
    
    # createPathsAndNames ----------------------
    createPathsAndNames = function() {
      paramsInfo <- paste("src", as.character(self$getSuperReplicationCount()),
                          "rc",  as.character(self$getReplicationCount()),
                          "ss",  as.character(self$getSampleSize()),
                          "a",   as.character(self$getAlpha()),
                          # be careful with the next one,
                          # used with abline at createImageCI
                          "sigma", as.character(self$getSigma()),
                          "mu",  as.character(self$getMu()), sep = "_")
      arraysCsvName = paste0("confintNormSigma_", paramsInfo, ".csv")
      arraysCsvPath <- file.path("./", "csv_output", arraysCsvName)
      # Plotting confints
      dir.create(file.path("./", "plots"), showWarnings = FALSE)
      saveImageCIName <-  paste0("confintNormSigma_", paramsInfo, ".jpeg")
      saveImageCIPath <-  file.path("./", "plots", saveImageCIName)
      # Plotting coverage.ratio.error
      # Exporting the plot to a file stored in a "./plots" folder
      saveImageCREName <-  paste0("coverageRatioErrorNormSigma_", paramsInfo, ".jpeg")
      saveImageCREPath <-  file.path("./", "plots", saveImageCREName)
      pathsAndNames <- list(arraysCsvPath, saveImageCIPath, saveImageCREPath)
      self$pathsAndNames <- pathsAndNames
      },
    getPathsAndNames = function() {
      self$pathsAndNames
    }
  ) # winding up the list of properties, fields and methods
) # ending up the class definition
