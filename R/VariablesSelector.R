#' Ranks of importance variables
#'
#' @name VariableSelector
#' @description A Ranker of variables
#'
#' @param formula A character object that contain the name of response variable about which a researcher is asking a question. Default value: "Y"
#' @param data the data frame that contains the variables specified in \code{formula}.
#' @param based Methodology used to rank variables. The options available are informationgain, gainratio and symmetrical.uncertainty.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A MLA object of subclass Var-Rank

#' @examples
#' ## Load a Dataset
#' data(EGATUR)
#' VariableRanker(formula=GastoTotalD~pais+aloja+motivo,EGATUR)
#'
#'@import FSelectorRcpp
#'@importFrom stats as.formula


#'@export
VariableRanker <- function(formula,data,based="gainratio",...) {
  Methods <- c("informationgain", "gainratio", "symmetrical.uncertainty")
  Method <- c("infogain", "gainratio","symuncert")[which(Methods==tolower(based))]


  Rankeo <- FSelectorRcpp::information_gain(formula,
                                            data=data,
                                            type=Method)

  Rankeo <- Rankeo[order(Rankeo$importance,decreasing = TRUE),]
  rownames(Rankeo) <- 1:length(Rankeo$importance)

  output <- list(Type="Var-Rank",
                 Ranking=Rankeo)

  class(output) <- "MLA"

  return(output)

}
