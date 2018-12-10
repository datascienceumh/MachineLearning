#' An expert rule based system using Reduction Based on Significance
#'
#' @name CREA.RBS
#' @description CREA-RBS is a rule reduction method for allocating a significance value to each rule in the system so that experts
#' may select the rules that should be considered as preferable and understand the exact degree of correlation between the different
#' rule attributes.
#'
#' @param formula a formula of the form y ~ x1 + x2 + ...
#' @param data the data frame that contains the variables specified in \code{formula}.
#'
#'@details Significance is calculated from the antecedent frequency and rule frequency parameters for each rule; if the first
#' one is above the minimal level and rule frequency is in a critical interval, its significance ratio is computed by the algorithm.
#' These critical boundaries are calculated by an incremental method and the rule space is divided according to them.
#' The significance function is defined for these intervals.
#'
#' @return A MLA object of subclass CREA-RBS
#'
#'@references Almiñana, M., Escudero, L. F., Pérez-Martín, A., Rabasa, A., & Santamaría, L. (2014). A classification rule reduction algorithm based on significance domains. Top, 22(1), 397-418.
#'
#' @examples
#' ## Load a Dataset
#' data(EGATUR)
#' ## Generate a CREA-RBS model, remmember only support discretized variables
#' CREA.RBS(GastoTotalD~pais+aloja+motivo,data=EGATUR)
#'
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom utils globalVariables
#' @import formula.tools
#' @import dplyr magrittr
#'

utils::globalVariables(c("totalRegla","total.casos"))

#' @export
CREA.RBS <- function(formula, data) {

  # mf <- match.call(expand.dots = FALSE)
  # m <- match(c("formula", "data"), names(mf), 0L)
  # mf <- mf[c(1L, m)]
  # mf$drop.unused.levels <- TRUE
  # mf[[1L]] <- quote(stats::model.frame)
  # mf <- eval(mf, parent.frame())
  # Is discrete??
  # apply(data,2,FUN=class)

  ## Process the formula
  responsevariable <- as.character(formula.tools::lhs(formula))
  dependentvariable <-  as.character(formula.tools::rhs.vars(formula, data = data))

  ## Detect the Response Variable and Save
  Y <- data[, responsevariable]

  Ec <- 1 / length(unique(data[, responsevariable]))
  Es <- 1 / prod(sapply(lapply(data[, dependentvariable], levels), length))

  ## Length of Dataset
  length_data <- nrow(data)
  data <- data[, c(dependentvariable, responsevariable)]

  Rules <- data %>%
    dplyr::group_by_at(dplyr::vars(c(dependentvariable, responsevariable))) %>%
    dplyr::summarise(
     "totalRegla" = n()
      # ,
      # Confianza=ElementosIguales/total.casos
    )
  Rules2 <- data %>%
    dplyr::group_by_at(dplyr::vars(c(dependentvariable))) %>%
    dplyr::summarise(
     "total.casos" = n(),
      Support = n() / length_data
    )

  RulesOut <- merge(x = Rules, y = Rules2, by = dependentvariable) %>%
    dplyr::mutate(Confidence = totalRegla / total.casos)

  Eci <- mean(RulesOut$Confidence[RulesOut$Confidence <= Ec])
  Ecs <- mean(RulesOut$Confidence[RulesOut$Confidence > Ec])
  Esi <- mean(RulesOut$Support[RulesOut$Support <= Es])
  Ess <- mean(RulesOut$Support[RulesOut$Support > Es])

  RulesOut$Region <- 0
  RulesOut$Region[which(RulesOut$Confidence <= Eci & RulesOut$Support >= Ess)] <- 1
  RulesOut$Region[which(RulesOut$Confidence >= Ecs & RulesOut$Support >= Ess)] <- 2
  RulesOut$Region[which(RulesOut$Support <= Esi)] <- 3

  rules_reg1 <- which(RulesOut$Region == 1)
  rules_reg2 <- which(RulesOut$Region == 2)
  rules_reg3 <- which(RulesOut$Region == 3)
  rules_reg0 <- which(RulesOut$Region == 0)

  ### Reglas en cada región
  rules_reg1_size <- length(rules_reg1)
  rules_reg2_size <- length(rules_reg2)
  rules_reg3_size <- length(rules_reg3)
  rules_reg0_size <- length(rules_reg0)


  ### Reajuste de limites de Regiones
  ### Usar TRY error y si es == Inf cancelar o mejor no reajustar si el length en la region ==0

  if(!is.infinite(max(RulesOut$Support[rules_reg3]))) Esi <- max(RulesOut$Support[rules_reg3])

  if(!is.infinite(min(RulesOut$Support[c(rules_reg1, rules_reg2)]))) Ess <- min(RulesOut$Support[c(rules_reg1, rules_reg2)])

  if(!is.infinite(max(RulesOut$Confidence[rules_reg1]))) Eci <- max(RulesOut$Confidence[rules_reg1])

  if(!is.infinite(min(RulesOut$Confidence[rules_reg2]))) Ecs <- min(RulesOut$Confidence[rules_reg2])


  ## Rule Significance
  RulesOut$Importance <- 0

  RulesOut$Importance[rules_reg1] <- -1+RulesOut$Support[rules_reg1]* RulesOut$Confidence[rules_reg1]
  ## RulesOut$Importance[rules_reg1] <- - 1 - RulesOut$Support[rules_reg1] * RulesOut$Confidence[rules_reg1]
  RulesOut$Importance[rules_reg2] <- RulesOut$Support[rules_reg2]* RulesOut$Confidence[rules_reg2]
  ## RulesOut$Importance[rules_reg2] <- 1 + RulesOut$Support[rules_reg2] * RulesOut$Confidence[rules_reg2]
  RulesOut$Importance[rules_reg3] <- -2 + RulesOut$Support[rules_reg3]* RulesOut$Confidence[rules_reg3]
  ## RulesOut$Importance[rules_reg3] <- RulesOut$Support[rules_reg3] * RulesOut$Confidence[rules_reg3]

  ACI_Num <- (rules_reg1_size +
                rules_reg2_size +
                rules_reg3_size) / nrow(RulesOut)
  #############     Region 1  + Region 3  +  Region 2
  ACI_Denom <- (1 - Ess) * Eci + Esi*1 + (1 - Ecs) * (1 - Ess)


  aci <- ACI_Num / ACI_Denom
  # Como gráficarlo
  # plot(RulesOut$Confidence,RulesOut$Support,xlab = "Confidence",ylab = "Support")
  # abline(v=Ecs,lty=2)
  # abline(v=Eci,lty=2)
  # abline(h=Esi,lty=2)
  # abline(h=Ess,lty=2)

  ## Falta ordenar y eliminar las reglas de la Selección 0 o cargarselas del PRINT
  RulesOutput <- RulesOut[-rules_reg0,]
  RulesOutput <- RulesOutput[order(RulesOutput$Importance, decreasing = TRUE),]
  rownames(RulesOutput) <- 1:nrow(RulesOutput)
  out <- list(Subclass="CREARBS",
    Model = RulesOutput,
    ACI = aci,
    Axis <- list(
      Ess = Ess,
      Esi = Esi,
      Eci = Eci,
      Ecs = Ecs
    )
  )
  class(out) <- "MLA"

  return(out)
}

#' @export
print.CREA_RBS <- function(x, first=100, digits = getOption("digits"), ...) {

  if (!inherits(x, "CREA_RBS")) stop("Not a legitimate \"VarRank\" object")
  if(nrow(x[[1]])>100){
    printable_rules <- x[[1]][1:first,]
  } else {
    printable_rules <- x[[1]]
  }
  print(printable_rules, digits = digits)

}
