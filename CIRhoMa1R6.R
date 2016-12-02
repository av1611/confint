# install.packages("R6")
library(R6)

# Defining CIR6 class:  ---------------------------------------------
CIRhoMa1R6 <- R6Class(
  "CIRhoMa1R6",
  inherit = CIR6,
  public = list(
    # CIR6 class fields ---------------------------------------------
    superReplicationCount = "numeric",
    replicationCount      = "numeric",
    sampleSize            = "numeric",
    mu    = "numeric",
    sigma = "numeric",
    alpha = "numeric",
    theta = "numeric",
    x     = "array",
    arraysToSave  = "array",
    pathsAndNames = "list",
    CRE = "array",
    
    # CIR6 class initialize method ---------------------------------------------
    initialize = function(superReplicationCount = 3,
                          replicationCount = 4,
                          sampleSize = 5,
                          mu = 0,
                          sigma = 1,
                          alpha = 0.05,
                          theta = 0.5,
                          x = array(0, dim =  rep(0, 3)),
                          arraysToSave = array(0, dim = rep(0, 10)),
                          pathsAndNames = list("a", "b", "c"),
                          CRE = array(0, dim =  rep(0, 3))) {
      self$superReplicationCount <- superReplicationCount
      self$replicationCount <- replicationCount
      self$sampleSize <- sampleSize
      self$mu <- mu
      self$sigma <- sigma
      self$alpha <- alpha
      self$theta <- theta
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
    setTheta = function(value) {
      self$theta <- value
    },
    getTheta = function() {
      self$theta
    },
    
    # X ---------------------------
    createX = function() {
      z = replicate(n = self$getSuperReplicationCount(), 
                    expr = replicate(n = self$getSampleSize() + 1, 
                                     expr = rnorm(n = self$getReplicationCount(), 
                                                  mean = self$getMu(), 
                                                  sd = self$getSigma())))
      x = array(0, dim = c(self$getReplicationCount(), 
                           self$getSampleSize(), 
                           self$getSuperReplicationCount()))
      # 3 nested loops to make x
      for (super.replication.index in 1 : self$getSuperReplicationCount())
      {
        for (replication.index in 1 : self$getReplicationCount())
        {
          for (sample.index in 2 : self$getSampleSize())
          {
            # make x out of z recursively
            x [replication.index, sample.index, super.replication.index] = 
              z [replication.index, sample.index, super.replication.index]  +
              z [replication.index, sample.index - 1, super.replication.index] * self$getTheta()
          }
        }
      }
      
      self$x <- x
    },
    getX = function() {
      self$x
    },
    # createArraysToSave ---------------------------
    createArraysToSave = function() {
      targetCoverageProb = 1 - self$getAlpha()
      #derived
      gamma0 = self$getSigma()^2 * (1 + self$getTheta()^2)
      gamma1 = self$getSigma()^2 * self$getTheta()
      rho1 = self$getTheta() / (1 + self$getTheta()^2)
      xBar = apply (self$getX(), c(1, 3), mean)
      xCentered = sweep(self$getX(), c(1, 3), xBar)
      gamma0Hat = apply (xCentered^2, c(1, 3), mean)
      # gamma 1
      gamma1Hat = array(0, dim = c(self$getReplicationCount(), 
                                   self$getSuperReplicationCount()))
    
    for (super.replication.index in 1 : self$getSuperReplicationCount()) {
      for (replication.index in 1 : self$getReplicationCount()) {
        sScalar = 0
        for (sample.index in 1 : (self$getSampleSize() - 1)) {
          # make x out of z recursively
          sScalar = sScalar + 
            xCentered[replication.index, sample.index, super.replication.index] * 
            xCentered[replication.index, sample.index + 1, super.replication.index]
        }
        gamma1Hat [replication.index, super.replication.index] = 
          sScalar / self$getSampleSize()
      }
    }
    
    gamma1Error = gamma1Hat - gamma1
    rho1Hat = gamma1Hat / gamma0Hat
    zQuantile = - qnorm(self$getAlpha()/2)
    marginOrError = zQuantile * sqrt((1 - 3 * rho1Hat^2 + 4 * rho1Hat^4) / self$getSampleSize())
    
    # compute margin of error
    confintLower  = rho1Hat - marginOrError
    confintUpper  = rho1Hat + marginOrError
    # confint.width  = confint.upper - confint.lower
    isRho1Covered = (rho1 < confintUpper & rho1 > confintLower)
    sumCovered = apply(isRho1Covered, 2, sum)
    coverageRatio = sumCovered / self$getReplicationCount()
    # should be of length = super.replication.count
    # vector minus scalar
    coverageRatioError = coverageRatio - targetCoverageProb      
    # assigning CRE for CRE plot
    self$CRE <- coverageRatioError
    arraysToSave <- cbind(
      as.vector(confintLower),
      as.vector(confintUpper),
      as.vector(isRho1Covered),
      as.vector(coverageRatioError),
      self$getSuperReplicationCount(),
      self$getReplicationCount(),
      self$getSampleSize(),
      self$getMu(),
      self$getSigma(),
      self$getAlpha(),
      self$getTheta())
      colnames(arraysToSave) <- c(
        "confintLower",
        "confintUpper",
        "isRho1Covered",
        "coverageRatioError",
        "superReplicationCount",
        "replicationCount",
        "sampleSize",
        "mu",
        "sigma",
        "alpha",
        "theta")
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
      self$imageInfo[1] <- "Confidence intervals for correlation of MA(1) \n
  based on Gaussian white noise, using Bartlett's formula"
      # Creating main title for CRE image
      self$imageInfo[2] <- "Difference between supposed and actual coverage probability
      in confidence intervals for correlation of MA(1) 
  based on Gaussian white noise, using Bartlett's formula"
      # Creating sub-title for CI & CRE images
      subTitle = paste0("src = ", self$getSuperReplicationCount(), ", ",
                        "rc = ", self$getReplicationCount(), ", ",
                        "ss = ", self$getSampleSize(), ", ",
                        "mu = ", self$getMu(), ", ",
                        "sigma = ", self$getSigma(), ", ",
                        "alpha = ", self$getAlpha(), ", ",
                        "theta = ", self$getTheta())
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
      paramsInfo <-
        paste(
          "src",
          as.character(self$getSuperReplicationCount()),
          "rc",
          as.character(self$getReplicationCount()),
          "ss",
          as.character(self$getSampleSize()),
          "mu",
          as.character(self$getMu()),
          "sigma",
          as.character(self$getSigma()),
          "a",
          as.character(self$getAlpha()),
          "theta",
          as.character(self$getTheta()),
          sep = "_"
        )
      arraysCsvName = paste0("confintRhoMa1_", paramsInfo, ".csv")
      arraysCsvPath <-
        file.path("./", "csv_output", arraysCsvName)
      # Plotting confints
      dir.create(file.path("./", "plots"), showWarnings = FALSE)
      saveImageCIName <-
        paste0("confintRhoMa1_", paramsInfo, ".jpeg")
      saveImageCIPath <-
        file.path("./", "plots", saveImageCIName)
      # Plotting coverage.ratio.error
      # Exporting the plot to a file stored in a "./plots" folder
      saveImageCREName <-
        paste0("coverageRatioRhoMa1_", paramsInfo, ".jpeg")
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
