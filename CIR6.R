# install.packages("R6")
library(R6)

# Defining CIR6 class:  ---------------------------------------------
CIR6 <- R6Class(
  classname = "CIR6",
  public = list(
    portable = FALSE,
    cloneable = FALSE,
    
    # CIR6 class fields ---------------------------------------------
    superReplicationCount = "numeric",
    replicationCount      = "numeric",
    sampleSize            = "numeric",
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
                          alpha = 0.05,
                          x = array(),
                          arraysToSave = array(),
                          pathsAndNames = list(),
                          imageInfo = list(),
                          CRE = array()) {
      self$superReplicationCount <- superReplicationCount
      self$replicationCount <- replicationCount
      self$sampleSize <- sampleSize
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
    setAlpha = function(value) {
      self$alpha <- value
    },
    getAlpha = function() {
      self$alpha
    },
    
    # x ------------------------------
    createX = function() {
    },
    getX = function() {
      self$x
    },
    
    # CRE ------------------------------
    getCRE = function() {
      self$CRE
      },
    
    # createArraysToSave ------------------------------
    createArraysToSave = function() {
     },
    getArraysToSave = function() {
      self$arraysToSave
    },
    
    # createImageInfo ------------------------------
    createImageInfo = function() {
    },
    getImageInfo = function() {
      self$imageInfo
    },
    
    # createPathsAndNames ------------------------------
    createPathsAndNames = function() {
    },
    getPathsAndNames = function() {
      self$pathsAndNames
    },
    
    # saveCSV ------------------------------
    saveCSV = function() {
      dir.create(file.path("./", "csv_output"), showWarnings = FALSE)
      write.csv(x = self$getArraysToSave(),
                file = as.character(self$getPathsAndNames()[1]))
    },
    
    # saveImageCI ------------------------------
    saveImageCI = function() {
      dir.create(file.path("./", "plots"), showWarnings = FALSE)
      finalInfoLength <- length(self$getArraysToSave())
      jpeg(self$getPathsAndNames()[2])
      plot(self$getArraysToSave()[, 1],
           col = "dark blue",
           type = "l",
           ylab = "intervals",
           ylim = range(min(self$getArraysToSave()[, 1]),
                        max(self$getArraysToSave()[, 2])))
      lines (self$getArraysToSave()[, 2], col = "blue")
      # Drawing a line:
      # - sigma -- for CINormMu, CINormSigma
      abline(h = self$getArraysToSave()[1, 9],  
             col = "dark gray", lty = 5)
      cl = c("red", "green")
      arrows(1:finalInfoLength, self$getArraysToSave()[, 1] [1:finalInfoLength], # confint lower
             1:finalInfoLength, self$getArraysToSave()[, 2] [1:finalInfoLength], # confint upper
             angle = 90, code = 0, lwd = 0.1,
             col = cl[self$getArraysToSave()[, 3][1:finalInfoLength] + 1])
      title(main = as.character(self$getImageInfo()["titleImageCI"]),
            sub  = as.character(self$getImageInfo()["subTitle"]),
            cex.main = 1,
            cex.sub = 0.7)
      graphics.off()
    },
    
    # saveImageCRE ------------------------------
    saveImageCRE = function() {
      dir.create(file.path("./", "plots"), showWarnings = FALSE)
      jpeg(self$getPathsAndNames()[3])
      plot(self$getCRE(), ylab = "difference",
           ylim = range(min(self$getCRE()),
                        max(self$getCRE())))
      title(main = as.character(self$getImageInfo()["titleImageCRE"]),
            sub  = as.character(self$getImageInfo()["subTitle"]),
            cex.main = 1,
            cex.sub = 1)
      graphics.off()
    },
    
    # doAll ------------------------------
    doAll = function() {
      self$createX()
      self$createArraysToSave()
      self$createImageInfo()
      self$createPathsAndNames()
      self$saveCSV()
      self$saveImageCI()
      self$saveImageCRE()
    }
  )
)
