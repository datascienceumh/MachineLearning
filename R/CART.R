#' Fit and graph a cart model
#'
#' @name CART
#' @description Classification And Regression Tree is a simple technique to fit a relationship between numerical variables partitioning the target variable by a range of values of the explanatory variables. This function fits and graphs a cart model with a previous separation of training a testing datasets.
#'
#'
#' @param formula a formula of the form y ~ x1 + x2 + ...
#' @param data the data frame that contains the variables specified in \code{formula}.
#' @param p the percentage of the training dataset to be obtained randomly.
#' @param nodes_min Number of minimum nodes.
#' @param nodes_max Number of maximum nodes.
#' @param includedata logicals. If TRUE the training and testing datasets are returned.
#' @param seed a single value, interpreted as an integer, or NULL. The default value is NULL, but for future checks of the model or models generated it is advisable to set a random seed to be able to reproduce it.
#' @param ... further arguments passed to or from other methods.


#' @return A MLA object of subclass CART
#'
#' @examples
#' ## Load a Dataset
#'  \dontrun{
#' data(EGATUR)
#' CART(GastoTotalD~pais+aloja+motivo,data=EGATUR)
#'}
#'
#' @import rpart

#' @export
CART <- function(formula, data, p = 0.7, nodes_min = 2, nodes_max = 18, includedata = FALSE, seed = NULL, ...) {

  # Protect if it doesn't install Rpart
  if (!requireNamespace("rpart", quietly = TRUE)) {
    stop(crayon::bold(crayon::red("rpart package needed for this function to work. Please install it.")),
      call. = FALSE
    )
  }

  response_variable <- as.character(formula.tools::lhs(formula))

  test_type <- is.numeric(data[, response_variable])

  if(!is.null(p)){
  # Using a Sample module to part data
  data <- sampler(data, p, seed = seed)
  # Rename Training
  training <- data$Data$training
  # Rename Testing
  testing <- data$Data$testing
  # Remove list of content
  remove(data)
  } else {
    training <- data
    testing <- data
    remove(data)
  }

  tree <- rpart::rpart(
    formula = formula, data = training, na.action = rpart::na.rpart,
    model = FALSE, x = FALSE, y = FALSE, cp = 0.01, ...
  )

  orig_predict <- predict(tree, testing, type = ifelse(test_type,
    "vector", "class"
  ))

  if (nrow(tree$cptable) < nodes_min) {
    newtree <- rpart::rpart(
      formula = formula, data = training, na.action = rpart::na.rpart,
      model = FALSE, x = FALSE, y = FALSE, cp = 0, ...
    )
    cptest <- unique(newtree$frame$complexity[newtree$frame$complexity < 0.01 & newtree$frame$complexity > 0])
    cptest <- cptest[order(cptest, decreasing = TRUE)] # df[order(df[,1],df[,2],decreasing=TRUE),]
    K <- length(cptest)
    k <- 0
    newtree2 <- newpredict <- rmse <- cp <- mc <- Success_rate <- error_tII <- error_tI <- nodes <- list()
    while (k < K) {
      k <- k + 1
      cp[[k]] <- cptest
      newtree2[[k]] <- rpart::prune.rpart(newtree, cp = cptest[[k]])
      newpredict[[k]] <- predict(newtree2[[k]],
        testing,
        type = ifelse(test_type,
          "vector",
          "class"
        )
      )
      rmse[[k]] <- RMSE(y = testing[, response_variable], yhat = newpredict[[k]])
      mc[[k]] <- MC(y = testing[, response_variable], yhat = newpredict[[k]])
      Success_rate[[k]] <- (sum(diag(mc[[k]]))) / sum(mc[[k]])
      error_tI[[k]] <- sum(mc[[k]][upper.tri(mc[[k]], diag = FALSE)]) / sum(mc[[k]])
      error_tII[[k]] <- sum(mc[[k]][lower.tri(mc[[k]], diag = FALSE)]) / sum(mc[[k]])
      nodes[[k]] <- nrow(newtree2[[k]]$frame)
    }
    SummaryTrees <- data.frame(
      CP = unlist(cp),
      rmse = unlist(rmse),
      success_rate = unlist(Success_rate),
      error = 1 - unlist(Success_rate),
      ti_error = unlist(error_tI),
      tii_error = unlist(error_tII),
      Nnodes = unlist(nodes)
    )
    SummaryTrees <- SummaryTrees[which(SummaryTrees$Nnodes < nodes_max), ]
    SummaryTrees <- SummaryTrees[order(SummaryTrees[, 2],
      SummaryTrees[, 4],
      SummaryTrees[, 7],
      decreasing = TRUE
    ), ]
    cp <- SummaryTrees$CP[1]
  }

  if (nrow(tree$cptable) > nodes_max) {
    cptest <- unique(tree$frame$complexity)
    cptest <- cptest[order(cptest, decreasing = FALSE)] # df[order(df[,1],df[,2],decreasing=TRUE),]
    K <- length(cptest)
    k <- 0
    newtree2 <- newpredict <- rmse <- cp <- mc <- Success_rate <- error_tII <- error_tI <- nodes <- list()
    while (k < K) {
      k <- k + 1
      cp[[k]] <- cptest
      newtree2[[k]] <- rpart::prune.rpart(newtree, cp = cptest[[k]])
      newpredict[[k]] <- predict(newtree2[[k]],
        testing,
        type = ifelse(test_type,
          "vector",
          "class"
        )
      )
      rmse[[k]] <- RMSE(y = testing[, response_variable], yhat = newpredict[[k]])
      mc[[k]] <- MC(y = testing[, response_variable], yhat = newpredict[[k]])
      Success_rate[[k]] <- (sum(diag(mc[[k]]))) / sum(mc[[k]])
      error_tI[[k]] <- sum(mc[[k]][upper.tri(mc[[k]], diag = FALSE)]) / sum(mc[[k]])
      error_tII[[k]] <- sum(mc[[k]][lower.tri(mc[[k]], diag = FALSE)]) / sum(mc[[k]])
      nodes[[k]] <- nrow(newtree2[[k]]$frame)
    }
    SummaryTrees <- data.frame(
      CP = unlist(cp),
      rmse = unlist(rmse),
      success_rate = unlist(Success_rate),
      error = 1 - unlist(Success_rate),
      ti_error = unlist(error_tI),
      tii_error = unlist(error_tII),
      Nnodes = unlist(nodes)
    )
    SummaryTrees <- SummaryTrees[which(SummaryTrees$Nnodes < nodes_max), ]
    SummaryTrees <- SummaryTrees[order(SummaryTrees[, 2],
      SummaryTrees[, 4],
      SummaryTrees[, 7],
      decreasing = TRUE
    ), ]
    cp <- SummaryTrees$CP[1]
  }

  if (nrow(tree$cptable) > nodes_min | nrow(tree$cptable) < nodes_max) {
    cptest <- unique(tree$frame$complexity)
    cptest <- cptest[order(cptest, decreasing = TRUE)] # df[order(df[,1],df[,2],decreasing=TRUE),]
    K <- length(cptest)
    k <- 0
    newtree2 <- newpredict <- rmse <- cp <- mc <- Success_rate <- error_tII <- error_tI <- nodes <- list()
    while (k < K) {
      k <- k + 1
      cp[[k]] <- cptest
      newtree2[[k]] <- rpart::prune.rpart(tree, cp = cptest[[k]])
      newpredict[[k]] <- predict(newtree2[[k]],
        testing,
        type = ifelse(test_type,
          "vector",
          "class"
        )
      )
      rmse[[k]] <- RMSE(y = testing[, response_variable], yhat = newpredict[[k]])
      mc[[k]] <- MC(y = testing[, response_variable], yhat = newpredict[[k]])
      Success_rate[[k]] <- (sum(diag(mc[[k]]))) / sum(mc[[k]])
      error_tI[[k]] <- sum(mc[[k]][upper.tri(mc[[k]], diag = FALSE)]) / sum(mc[[k]])
      error_tII[[k]] <- sum(mc[[k]][lower.tri(mc[[k]], diag = FALSE)]) / sum(mc[[k]])
      nodes[[k]] <- nrow(newtree2[[k]]$frame)
    }
    SummaryTrees <- data.frame(
      CP = unlist(cp),
      rmse = unlist(rmse),
      success_rate = unlist(Success_rate),
      error = 1 - unlist(Success_rate),
      ti_error = unlist(error_tI),
      tii_error = unlist(error_tII),
      Nnodes = unlist(nodes)
    )
    SummaryTrees <- SummaryTrees[which(SummaryTrees$Nnodes < nodes_max & SummaryTrees$Nnodes > nodes_min), ]
    SummaryTrees <- SummaryTrees[order(SummaryTrees[, 2],
      SummaryTrees[, 4],
      SummaryTrees[, 7],
      decreasing = TRUE
    ), ]
    cp <- SummaryTrees$CP[1]
  }
  tree <- rpart::rpart(
    formula = formula, data = training, na.action = rpart::na.rpart,
    model = FALSE, x = FALSE, y = FALSE, cp = cp, ...
  )

  predict_final <- predict(tree, testing, type = ifelse(test_type,
    "vector", "class"
  ))
  mc <- MC(y = testing[, response_variable], yhat = predict_final)



  ans <- list(
    Subclass = "CART",
    Model = tree,
    Summary = SummaryTrees,
    Confussion_Matrixs = mc,
    Data = ifelse(includedata, list(training, testing), list(NULL))
  )



  class(ans) <- "MLA"

  ans
}
RMSE <- function(yhat, y, type.of = c("numeric", "text", "scalable")) {
  ### Agregar funciones de cierre de edición
  ## Opción gráfica
  ## Conversiones, etc.
  if (is.factor(y) == TRUE && is.factor(yhat) == FALSE) {
    y <- as.numeric(y) - 1
  }
  if (is.factor(y) == FALSE && is.factor(yhat) == TRUE) {
    yhat <- as.numeric(yhat) - 1
  }
  Real <- y
  Estimated <- yhat
  rmse <- sqrt(sum((as.numeric(Estimated) - as.numeric(Real))^2) / length(Real))

  return(rmse)
}
MC <- function(yhat, y) {
  if (class(yhat) != class(y)) {
    yhat <- as.numeric(yhat)
    y <- as.numeric(y)
  }
  Real <- y
  Estimated <- yhat
  MC <- table(Estimated, Real)
  Success_rate <- (sum(diag(MC))) / sum(MC)
  tI_error <- sum(MC[upper.tri(MC, diag = FALSE)]) / sum(MC)
  tII_error <- sum(MC[lower.tri(MC, diag = FALSE)]) / sum(MC)
  General_metrics <- data.frame(
    Success_rate = Success_rate,
    tI_error = tI_error,
    tII_error = tII_error
  )

  output <- MC

  return(output)
}
