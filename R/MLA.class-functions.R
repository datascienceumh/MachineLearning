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
print.MLA <- function(x, first=100 ,digits = getOption("digits"), ...) {

  if (!inherits(x, "MLA")) stop("Not a legitimate \"MLA\" object")

  switch(x[[1]],
         "CART" = printCART(x, digits),

         "Var-Rank"={
           printSelector(x[[2]])
         },
         "CREARBS"={
           printCREARBS(x[[2]])
         },
         "Association"={
           printAssociationRules(x[[2]],first=first, digits=digits)
         },
         "Clustering"={
           printCluster(x)
         }


  )
}

# Print CART
printCART <- function(x, decimals=getOption("digits")) {

      cat(
        nrow(x[[3]]),
        format("successful models have been tested \n"),
        format("\n")
      )
      print(x[[2]],digits=decimals)

}

# Print Asociation Rules
printAssociationRules  <- function(x, first=100, digits = getOption("digits")) {

  if(length(arules::size(x))>first){
    printable_rules <- x[1:first,]
  } else {
    printable_rules <- x
  }

  arules::inspect(printable_rules, ruleSep="=>",itemSep="+",setStart="",setEnd="",linebreak=FALSE,digits=digits)
}

# Print Cluster
printCluster <- function(x){
  if(length(x[[4]])!=0){
  cat(length(x[[4]][[1]]),"successful techniques are used to obtain the best number of clusters. \n")
  }
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
#' @export
summary.MLA <- function(object, ...) {
  print(object)
}

#### General Plot of MLA ####
#' Plot MLA object
#'
#' @name plot.MLA
#' @description This function plot an MLA object. It is a method for the generic function plot.
#'
#' @param x object of class "MLA"
#' @param simply Allow to simplify the view of nodes, in case of MLA object is a CART. Default value is FALSE.

#' @param ... further arguments passed to or from other methods.
#'
#' @export
plot.MLA <- function(x,simply=FALSE,...){
  if (!inherits(x, "MLA")) stop("Not a legitimate \"MLA\" object")

  switch(x[[1]],
         "CART" = {
           plotCART(x[[2]],ownlabs=simply)
         },
         "GainRatio"={
           plotSelector(x[[2]])
         },
         {
           warning(paste0('So sorry, this method is not yet implemented for ', x[[1]]))
         }
  )}


#### Helpers to plot for MLA class ####
#' Plot rpart or MLA object
#'
#' @name plotCART
#' @description This function plot an MLA object or a rpart.
#'
#' @param x object of class "MLA" or "rpart".
#' @param ownlabs Allow to simplify the view of nodes. Default value is TRUE.

#' @param ... further arguments passed to or from other methods.
#'


#' @export
plotCART <- function(x,ownlabs=TRUE){
  if (!inherits(x, c("MLA", "rpart"))) stop("Not a legitimate \"MLA\" or \"rpart\" object")
  if(inherits(x, "MLA")){
    x <- x[[2]]
  }
  if(ownlabs==FALSE){
    rpart.plot(x)
  } else{
    nodeCLASS <- function(x, labs, digits, varlen) {
      data <- as.data.frame(do.call("rbind", strsplit(labs,"\n")),stringsAsFactors = FALSE)
      Matriz <- do.call("rbind",strsplit(data$V2,"  "))
      Matriz <- apply(Matriz, 1,as.numeric)*100
      Matriz <- apply(Matriz, 2,function(x) paste0(x, "%",collapse="  "))
      data[,2] <- Matriz
      apply(data,1,function(x) paste0(x, collapse="\n"))
    }

    nodeBIN <- function(x, labs, digits, varlen) {
      data <- as.data.frame(do.call("rbind", strsplit(labs,"\n")),stringsAsFactors = FALSE)
      Matriz <- do.call("rbind",strsplit(data$V2,"  "))
      Matriz <- apply(Matriz, 1,as.numeric)*100
      Matriz <- apply(Matriz, 2,function(x) paste0(x, "%",collapse="  "))
      data[,2] <- Matriz
      apply(data,1,function(x) paste0(x, collapse="\n"))
    }
    nodeREG <- function(x, labs, digits, varlen) {
      # save(x, labs,digits,varlen,file="Test.Rda")
      #
      # ValorEstimado <- x$frame$yval
      # Confianza <- x$frame$n/x$frame$n[1]
      # paste0(ValorEstimado,"\n",round(Confianza*100,2),"%")
paste(labs)
}
    class <- x$method
    nodefun <- switch(class,
                        "anova"=nodeREG,
                        "poisson"=nodeBIN,
                        "class"=nodeCLASS,
                          nodeREG)
    rpart.plot(x,node.fun=nodefun)
  }
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
