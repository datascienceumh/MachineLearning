##########################################################################################################
##########################################################################################################
##################                                                                      ##################
##################                       FUNCTIONS FOR MLA CLASS                        ##################
##################                                                                      ##################
##################                                                                      ##################
##########################################################################################################
##########################################################################################################

#' Print an MLA Object
#'
#' @name print.MLA
#' @description This function prints an MLA object. It is a method for the generic function print.
#'
#' @param x object of class "MLA"
#' @param first When the print command shows a set of rules limits the number of rules.
#' @param digits minimal number of significant digits.
#' @param ... further arguments passed to or from other methods.
#'
#' @importFrom "utils" "capture.output"
#' @importFrom "stats" "predict"
#' @importFrom "graphics" "plot"
#' @importFrom crayon bold black
#' @import ggplot2 rpart.plot


#' @export
#### General Print of MLA ####
### MassiveDataAnalysis, MachineLearning, DataMining
print.MLA <- function(x, first=100 ,digits = getOption("digits"), ...) {

  if (!inherits(x, "MLA")) stop("Not a legitimate \"MLA\" object")

  switch(x[[1]],
         "CART" = printCART(x[[2]], digits),

         "Var-Rank"={
           printSelector(x[[2]])
         },
         "CREARBS"={
           printCREARBS(x[[2]])
         },
         "Association"={
           printAsociationRules(x[[2]],first=first, digits=digits)
         },
         "Cluster"={
           printCluster(x[[2]])
         }


  )
}

# Print CART
printCART <- function(x, decimals=getOption("digits")) {

      cat(
        nrow(x),
        format("successful models have been tested \n"),
        format("\n")
      )
      print(x,digits=decimals)

}

# Print Asociation Rules
printAsociationRules  <- function(x, first=100, digits = getOption("digits")) {

  if(length(arules::size(x))>first){
    printable_rules <- x[1:first,]
  } else {
    printable_rules <- x
  }

  arules::inspect(printable_rules, ruleSep="=>",itemSep="+",setStart="",setEnd="",linebreak=FALSE,digits=digits)
}

# Print Cluster
printCluster <- function(x){
  print(x)
}

# Print GainRatio
printGainRatio <- function(x){
 print(x)
}
printCREARBS <- function(x){
  print(x)
}
printSelector <- function(x){
  print(x)
}


#### General Summary of MLA ####
summary.MLA <- function(x, first=100, digits = getOption("digits"), ...) {
  print(x)
}

#### General Plot of MLA ####
plot.MLA <- function(object,...){
  if (!inherits(object, "MLA")) stop("Not a legitimate \"MLA\" object")

  switch(object[[1]],
         "CART" = {
           plotCART(object[[2]])
         },
         "GainRatio"={
           plotSelector(object[[2]])
         },
         {
           warning(paste0('So sorry, this method is not yet implemented for ', object[[1]]))
         }
  )}


#### Helpers to plot for MLA class ####
plotCART <- function(x){
rpart.plot(x)
  }

utils::globalVariables("importance")
plotSelector <- function(x){
  Rank_graph <- x
  Rank_graph$attributes <- factor(Rank_graph$attributes, levels =  rev(Rank_graph$attributes))

  out <- ggplot2::ggplot(Rank_graph, ggplot2::aes(x=attributes, y=importance))
  out <- out + ggplot2::geom_segment( ggplot2::aes(x=attributes, xend=attributes, y=0, yend=importance), color="skyblue")
  out <- out + ggplot2::geom_point( color="blue", size=4, alpha=0.6)
  out <- out + ggplot2::coord_flip()
  out
  }
