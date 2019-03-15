#' Splitting your dataset in training and testing
#'
#' @name sampler
#' @description A training/test partition are created by \code{sampler} function.
#'
#' @param data the data frame that contains the variables to be separeted.
#' @param p the percentage of the training dataset to be obtained randomly. It can be expressed in either decimal fraction (such as 0.7) or percent (such as 72.12).
#' @param seed a single value, interpreted as an integer, or \code{NULL}. The default value is \code{NULL}, but for future checks of the model or models generated it is advisable to set a random seed to be able to reproduce it.

# @param output A character string. Possible values are \code{list}, \code{environment} and \code{invisible}. See details for more information.
# @param trainingname The name of training object (only with select \code{output = environment} or {invisible} )
# @param testingname The name of testing object (only with select \code{output = environment} or {invisible} )
#
#@details This function was created as an auxiliary function of the optimizing functions contained in this package.
#But the use of auxiliary functions are allowed. For this reason, there are different output options, you can use the next options:
# \code{list}: witch this option, this function returns a list,
# \code{environment}: witch this option, this function returns a list, and write two objects (training/testing datasets) in Global Environment
#and \code{invisible}: witch this option, this function returns a NULL, and write two objects (training/testing datasets) in Global Environment.

#' @examples
#' # The best way to demostrate the functionality is test the function
#' Sampling <- sampler(EGATUR,p=0.7)
#'
#'
#'
#' @importFrom stats as.formula  model.matrix
#' @export
sampler <- function(data, p,seed=NULL){

  if(!is.null(seed)){
    set.seed(seed)
  }
  if(p>0.99){
    if(p<99.99){
      p <- p/100
    } else{
      stop(crayon::bold("p"), " must be a value between 0 and 1 or 0 and 100")
    }
  }
   n<- ifelse(is.vector(data),length(data),nrow(data))
  sample <- sample.int(n = n, size = floor(p*n), replace = FALSE)
  if(is.vector(data)){
    training <- data[sample]
    testing  <- data[-sample]
  } else {
  training <- data[sample, ]
  testing  <- data[-sample, ]
  }
  output2 <- list(Data=list(training=training,testing=testing), RowsTraining=sample)
  return(output2)

}
