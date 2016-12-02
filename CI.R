CI <- setClass(
  # Set the name for the class
  Class = "CI",
  
  # Define the slots
  slots = c(superReplicationCount = "numeric",
            replicationCount      = "numeric",
            sampleSize            = "numeric",
            alpha = "numeric",
            x     = "array",
            upper = "numeric",
            lower = "numeric",
            arraysToSave  = "array",
            pathsAndNames = "list"),
  # Set the default values for the slots. (optional)
  prototype = list (superReplicationCount = 10,
                    replicationCount      = 10,
                    sampleSize            = 10,
                    alpha = 0.05,
                    x     = array(0, dim = c(0, 0, 0)), 
                    upper =    0,
                    lower =    0,
                    arraysToSave = array(0, dim = c(rep(1, 10))),
                    pathsAndNames = list()),

  # Make a function that can test to see if the data is consistent.
  validity = function(object)  {
    if(object@superReplicationCount < 2 | object@replicationCount < 2 | 
       object@sampleSize < 2 | object@alpha < 0 | object@alpha > 1) {
        return("Some of parameter(s) are out of bounds.")
    }
    return(TRUE)
  }
)


# superReplicationCount ---------------------------------------------------


# create a method to assign and get the super replication count (superReplicationCount)
setGeneric(name <- "setSuperReplicationCount", 
           def <- function(object, superReplicationCount) { 
             standardGeneric("setSuperReplicationCount") 
             } 
           )

setMethod(f <- "setSuperReplicationCount",
          signature <- "CI",
          definition <- function(object, superReplicationCount) {
            object@superReplicationCount <- superReplicationCount
            validObject(object)
            return(object)
            }
          )

setGeneric(name <- "getSuperReplicationCount", 
           def <- function(object) { 
             standardGeneric("getSuperReplicationCount") 
             } 
           )

setMethod(f <- "getSuperReplicationCount",
          signature <- "CI",
          definition <- function(object) {
            return(object@superReplicationCount)
            }
          )

# create a method to set and get the value of the replication count (replicationCount)
setGeneric(name="setReplicationCount",
           def <- function(object, replicationCount) { 
             standardGeneric("setReplicationCount") 
             } 
           )


# replicationCount --------------------------------------------------------


setMethod(f <- "setReplicationCount",
          signature <- "CI",
          definition <- function(object, replicationCount) {
            object@replicationCount <- replicationCount
            validObject(object)
            return(object)
            }
          )

setGeneric(name="getReplicationCount",
           def <- function(object) { 
             standardGeneric("getReplicationCount") 
           } 
)

setMethod(f <- "getReplicationCount",
          signature <- "CI",
          definition <- function(object) {
            return(object@replicationCount)
          }
)


# sampleSize --------------------------------------------------------------


# create a method to assign and get the value of the sample size (sampleSize)
setGeneric(name="setSampleSize",
           def <- function(object, sampleSize) { 
             standardGeneric("setSampleSize") 
             } 
           )

setMethod(f <- "setSampleSize",
          signature <- "CI",
          definition <- function(object, sampleSize) {
            object@sampleSize <- sampleSize
            validObject(object)
            return(object)
            }
          )

setGeneric(name="getSampleSize",
           def <- function(object) { 
             standardGeneric("getSampleSize") 
             } 
           )

setMethod(f <- "getSampleSize",
          signature <- "CI",
          definition <- function(object) {
            return(object@sampleSize)
            }
          )


# alpha -------------------------------------------------------------------


# create a method to assign and get the super replication count (superReplicationCount)
setGeneric(name <- "setAlpha", 
           def <- function(object, alpha) { 
             standardGeneric("setAlpha") 
             } 
           )

setMethod(f <- "setAlpha",
          signature <- "CI",
          definition <- function(object, alpha) {
            object@alpha <- alpha
            validObject(object)
            return(object)
          }
)

setGeneric(name <- "getAlpha", 
           def <- function(object) { 
             standardGeneric("getAlpha") 
             } 
           )

setMethod(f <- "getAlpha",
          signature <- "CI",
          definition <- function(object) {
            return(object@alpha)
            }
          )


# createX -----------------------------------------------------------------------


# create a method to assign and get the x (x)
setGeneric(name <- "createX",
           def <- function(object) {
             standardGeneric("createX")
             }
           )

setMethod(f <- "createX",
          signature <- "CI",
          definition <- function(object) {
            }
          )

setGeneric(name <- "getX",
           def <- function(object) {
             standardGeneric("getX")
             }
           )

setMethod(f <- "getX",
          signature <- "CI",
          definition <- function(object) {
            return(object@x)
          }
)


# lowerBound --------------------------------------------------------------


# create a method to assign and get the lower confint
setGeneric(name <- "setLower", 
           def <- function(object, lower) { 
             standardGeneric("setLower") 
             } 
           )

setMethod(f <- "setLower",
          signature <- "CI",
          definition <- function(object, lower) {
            object@lower <- lower
            validObject(object)
            return(object)
            }
          )

setGeneric(name <- "getLower", 
           def <- function(object) { 
             standardGeneric("getLower") 
             } 
           )

setMethod(f <- "getLower",
          signature <- "CI",
          definition <- function(object) {
            return(object@lower)
            }
          )


# upperBound --------------------------------------------------------------


# create a method to assign and get the super replication count (superReplicationCount)
setGeneric(name <- "setUpper", 
           def <- function(object, upper) { 
             standardGeneric("setUpper") 
           } 
)

setMethod(f <- "setUpper",
          signature <- "CI",
          definition <- function(object, upper) {
            object@upper <- upper
            validObject(object)
            return(object)
          }
)

setGeneric(name <- "getUpper", 
           def <- function(object) { 
             standardGeneric("getUpper") 
           } 
)

setMethod(f <- "getUpper",
          signature <- "CI",
          definition <- function(object) {
            return(object@superReplicationCount)
          }
)


# arraysToSave ------------------------------------------------------------

setGeneric(name <- "createArraysToSave", 
           def <- function(object, arraysToSave) { 
             standardGeneric("createArraysToSave") 
           } 
)

setMethod(f <- "createArraysToSave",
          signature <- "CI",
          definition <- function(object, arraysToSave) {
            return(object)
            }
)

setGeneric(name <- "getArraysToSave", 
           def <- function(object) { 
             standardGeneric("getArraysToSave") 
           } 
)

setMethod(f <- "getArraysToSave",
          signature <- "CI",
          definition <- function(object) {
            return(getArraysToSave(object))
          }
)

# pathsAndNames  --------------------------------------------------------------------

setGeneric(name <- "createPathsAndNames",
           def  <- function(object) {
             standardGeneric("createPathsAndNames")
           } 
)


setMethod(f <- "createPathsAndNames",
          signature <- signature("CI"),
          definition <- function(object) {
            return(object)
            }
)


setGeneric(name <- "getPathsAndNames", 
           def <- function(object) { 
             standardGeneric("getPathsAndNames") 
           } 
)

setMethod(f <- "getPathsAndNames",
          signature <- "CI",
          definition <- function(object) {
            return(getPathsAndNames(object))
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
          signature <- signature("CI", 
                                 "logical", 
                                 "logical", 
                                 "logical"),
          definition <- function(object, 
                                 is_csv_output, 
                                 is_confint_image,
                                 is_cre_image) {
            
            if (is_csv_output == T) { 
              
              
            }
            
            if (is_confint_image == T) { 
              
            }
            
            if (is_cre_image == T) { 
              
              }
            }
          )


# saveCSV ----------------------------------------------------------------

setGeneric(name <- "saveCSV",
           def  <- function(object) {
             standardGeneric("saveCSV")
           } 
)

setMethod(f <- "saveCSV",
          signature <- signature("CI"),
          definition <- function(object) {
            dir.create(file.path("./", "csv_output"), showWarnings = FALSE)
            write.csv(x = getArraysToSave(object), 
                      file = as.character(getPathsAndNames(object)[1]))
          }
)


# saveImageCI -------------------------------------------------------------

setGeneric(name <- "saveImageCI",
           def  <- function(object) {
             standardGeneric("saveImageCI")
           } 
)

setMethod(f <- "saveImageCI",
          signature <- signature("CI"),
          definition <- function(object) {
            
          }
)


# saveImageCRE -------------------------------------------------------------

setGeneric(name <- "saveImageCRE",
           def  <- function(object) {
             standardGeneric("saveImageCRE")
           } 
)

setMethod(f <- "saveImageCRE",
          signature <- signature("CI"),
          definition <- function(object) {
            
          }
)

