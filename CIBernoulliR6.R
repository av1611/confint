# install.packages("R6")
library(R6)

# Defining CIR6 class:  ---------------------------------------------
CIBernoulliR6 <- R6Class(
  "CIBernoulliR6",
  inherit = CIR6,
  public = list(
    # CIR6 class fields ---------------------------------------------
    superReplicationCount = "numeric",
    replicationCount      = "numeric",
    sampleSize            = "numeric",
    p = "numeric",
    alpha = "numeric",
    x     = "array",
    arraysToSave  = "array",
    pathsAndNames = "list",
    CRE = "array",
    
    # CIR6 class initialize method ---------------------------------------------
    initialize = function(superReplicationCount = 3,
                          replicationCount = 4,
                          sampleSize = 5,
                          p = 0.5,
                          alpha = 0.05,
                          x = array(0, dim =  rep(0, 3)),
                          arraysToSave = array(0, dim = rep(0, 10)),
                          pathsAndNames = list("a", "b", "c"),
                          CRE = array(0, dim =  rep(0, 3))) {
      self$superReplicationCount <- superReplicationCount
      self$replicationCount <- replicationCount
      self$sampleSize <- sampleSize
      self$p <- p
      self$alpha <- alpha
      self$x <- x
      self$arraysToSave <- arraysToSave
      self$pathsAndNames <- pathsAndNames
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
    setP = function(value) {
      self$p <- value
    },
    getP = function() {
      self$p
    },
    setAlpha = function(value) {
      self$alpha <- value
    },
    getAlpha = function() {
      self$alpha
    },
    # X ---------------------------
    createX = function() {
      self$x <- replicate(n = self$getSuperReplicationCount(),
                          expr = replicate(n = self$getSampleSize(),
                                           expr = rbinom(self$getReplicationCount(),
                                                         size = 1,
                                                         prob = self$getP())))
    },
    getX = function() {
      self$x
    },
    # createArraysToSave ---------------------------
    createArraysToSave = function() {
      targetCoverageProb = 1 - self$getAlpha()
      xBar = apply(self$getX(), c(1, 3), mean)
      # which formula to choose
      # marginOrError = 1 / (2 * sqrt (self$getSampleSize() * self$getAlpha()))
      # smaller, but still guarantees probability
      marginOrError = sqrt(-log(self$getAlpha()*0.5)/(2*self$getSampleSize()))
      # compute margin of error
      confintLower  = xBar - marginOrError
      confintUpper  = xBar + marginOrError
      # confintWidth  = confintUpper - confintLower
      isPCovered = (self$getP() < confintUpper & self$getP() > confintLower)
      sumCovered = apply(isPCovered, 2, sum)
      coverageRatio = sumCovered / self$getReplicationCount() 
      # should be of length = super.replication.count
      # vector minus scalar
      coverageRatioError = coverageRatio - targetCoverageProb
      # assigning CRE for CRE plot
      self$CRE <- coverageRatioError
      arraysToSave <- cbind(
        as.vector(confintLower),
        as.vector(confintUpper),
        as.vector(isPCovered),
        as.vector(coverageRatioError),
        self$getSuperReplicationCount(),
        self$getReplicationCount(),
        self$getSampleSize(),
        self$getAlpha(),
        self$getP()
      )
      colnames(arraysToSave) <- c(
        "confintLower",
        "confintUpper",
        "isPCovered",
        "coverageRatioError",
        "superReplicationCount",
        "replicationCount",
        "sampleSize",
        "alpha",
        "p"
      )
      self$arraysToSave <- arraysToSave
    },
    getArraysToSave = function() {
      self$arraysToSave
    },
    getCRE = function() {
      self$CRE
    },
    # createImageInfo -------------------------
    createImageInfo = function() {
      # Creating main title for CI image
      self$imageInfo[1] <- "Confidence intervals for a mean of Bernoulli distribution"
      # Creating main title for CRE image
      self$imageInfo[2] <- "Difference between supposed and actual coverage probability
      in a confidence interval for a mean of Bernoulli distribution"
      # Creating sub-title for CI & CRE images
      subTitle = paste0("src = ", self$getSuperReplicationCount(), ", ",
                        "rc = ", self$getReplicationCount(), ", ",
                        "ss = ", self$getSampleSize(), ", ",
                        "a = ", self$getAlpha(), ", ",
                        "p = ", self$getP())
      self$imageInfo[3] <- subTitle
      # Creating value for abline function for CI image:
      # - sigma -- for CINormMu, CINormSigma
      names(self$imageInfo) <- c("titleImageCI", # self$imageInfo[1]
                                 "titleImageCRE",# self$imageInfo[2]
                                 "subTitle")     # self$imageInfo[3]
    },
    getImageInfo = function() {
      self$imageInfo
    },
    
    # createPathsAndNames ----------------------
    createPathsAndNames = function() {
      paramsInfo <-
        paste(
          "src",
          as.character(self$getSuperReplicationCount()),
          "rc",
          as.character(self$getReplicationCount()),
          "ss",
          as.character(self$getSampleSize()),
          "a",
          as.character(self$getAlpha()),
          "p",
          as.character(self$getP()),
          sep = "_"
        )
      arraysCsvName = paste0("confintBernoulli_", paramsInfo, ".csv")
      arraysCsvPath <-
        file.path("./", "csv_output", arraysCsvName)
      # Plotting confints
      dir.create(file.path("./", "plots"), showWarnings = FALSE)
      saveImageCIName <-
        paste0("confintBernoulli_", paramsInfo, ".jpeg")
      saveImageCIPath <-
        file.path("./", "plots", saveImageCIName)
      # Plotting coverage.ratio.error
      # Exporting the plot to a file stored in a "./plots" folder
      saveImageCREName <-
        paste0("coverageRatioErrorBernoulli_", paramsInfo, ".jpeg")
      saveImageCREPath <-
        file.path("./", "plots", saveImageCREName)
      pathsAndNames <-
        list(arraysCsvPath, saveImageCIPath, saveImageCREPath)
      self$pathsAndNames <- pathsAndNames
    },
    getPathsAndNames = function() {
      self$pathsAndNames
    }
  ) # winding up the list of properties, fields and methods
) # ending up the class definition
