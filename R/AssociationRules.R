
#' Simple way to obtain data mining rules
#'
#' @name AssociationRules
#' @description This is a rule-based machine learning method to discover interesting relationships between a consequent and an antecedent (or group of antecedents) in large databases.
#'
#' @param data a data frame with discrete variables.
#' @param support a numeric value for the minimun support of the antecedents (default: 0.2).
#' @param confidence a numeric value for the minimun confidence of confidence in rule/association method (default: 0.8)
#' @param minlength an integer value for the minimal number of items per item set (default: 2 item)
#'
#' @return A MLA object of subclass Association
#'
#' @examples
#' ## Load a Dataset
#' data(EGATUR)
#' ## Generate an asociation rules with apriori, remmember only support discretized variables,
#' ##  in this remove numerical variables.
#' Rules <- AssociationRules(EGATUR[,c(2,4,5,8)])
#'


#' @importFrom arules apriori

#' @export
AssociationRules <- function(data, support = 0.2, confidence = 0.1 , minlength = 2){

  rules <- arules::apriori(data, parameter = list(support = support,
                                         confidence = confidence,
                                         minlen=minlength))
  Info <- data.frame(NTransactions=rules@info$ntransactions,
                       Support=rules@info$support,
                       Confidence=rules@info$confidence)

out <- list(Subclass = "Association",
            Model    =  rules,
            Summary  = summary(rules),
            Info
            )
class(out) <- "MLA"
return(out)
}

