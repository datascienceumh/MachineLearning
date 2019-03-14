#' A simple and powerful function to create clusters with KMeans
#'
#' @name Clustering
#' @description This is a modified kmeans clustering technique to automatize the number of groups or clusters that can be partitioned the sample. Several techniques are used to obtain the best number of clusters.
#'
#' @param data Data frame which numeric variables.
#' @param n Data frame which numeric variables.
#' @param iter.max the maximum number of iterations allowed.
#' @param n_max maximal number of clusters, between 2 and (number of objects - 1), greater or equal to n_min. By default, n_max=10.
#' @param auto_criterion the available criterions are: "explainwss",
#'  "db", "ratkowsky", "ball" and "friedman".
#' @param confidenceWSS a confidence interval for criterion WSS.
#' @param agregate_method a function to agregate results of different methods. Default value=median


#' @details Several methods are available in order to obtain the best number of clusters:
#' explainwss = Within-cluster Sum of Square
#' db = Davies–Bouldin index (DBI). Davies and Bouldin (1979)
# is an internal evaluation scheme, where the validation of how well the clustering has been done is made using quantities and features inherent to the dataset. doi:10.1109/TPAMI.1979.4766909
#' ratkowsky = Ratkowsky and Lance (1978)
#' ball = Ball and Hall (1965)
#' friedman = Friedman and Rubin (1967)
#'
#'  @return A MLA object of subclass Clustering
#' @importFrom stats kmeans median var
#' @importFrom NbClust NbClust
#' @import formula.tools
#' @import dplyr magrittr
#'
#' @examples
#' ## Load a Dataset
#'  \dontrun{
#' data(EGATUR)
#' modelFit <- Clustering(data=EGATUR[,c("A13","gastototal")])
#'}
#'
#' @export
Clustering <- function(data, n="auto", n_max=10, iter.max=10, auto_criterion=c("explainwss","db", "ratkowsky", "ball","friedman"),confidenceWSS=0.9, agregate_method=median){
    Iters <- list()
    if(class(n)=="character"){
      if(n!="auto"){

        warning(paste0("I do not understand what you mean by '",n,"', so I decide to apply the automatic method." ))

         } # End If different of auto
       recommended <- NULL
      #Se inicia a buscar el optimo
      if(length(intersect(auto_criterion,"explainwss"))==1){
        WSS <- WSSOptimizer(dataPassed = data,maxCluster = n_max,confidence = confidenceWSS)
        recommended <- rbind(recommended,WSS[[2]])
        names(recommended) <- "WSS"
      }
       ## Ahora vamos con los que usan NbClust
       Metodos <- intersect(auto_criterion,c("db", "ratkowsky", "ball", "friedman"))

       if(length(Metodos)!=0){
         tempNC <- sapply(Metodos,FUN = NbClustOptimizer, dataPassed=data, maxCluster=n_max)
         recommended <- c(recommended,tempNC)
       }

       n_recommended <- round(agregate_method(recommended, na.rm = TRUE),0)
       n <- n_recommended
       Iters[[1]] <- recommended
       names(Iters[[1]]) <- gsub(".Number_clusters","",names(Iters[[1]]))
       Iters[[2]] <- agregate_method
    }

  model <- kmeans(data, centers = n, iter.max = iter.max)

  out <- list(Subclass="Clustering",
                Model=model,
                Summary=NULL,
                Iters=Iters
              )

  class(out) <- "MLA"
  return(out)

} # End function

### Función auxiliar para modo automático
NbClustOptimizer <- function(inds,dataPassed,maxCluster){
TempClust <- NbClust::NbClust(dataPassed, distance = "euclidean", min.nc = 2,
                              max.nc = maxCluster, method = "kmean", index  = inds)
tempNC<-TempClust$Best.nc[1]
return(tempNC)
}

### Función auxiliar para KMeans con WSS
WSSOptimizer <-function(dataPassed, maxCluster, confidence=0.90){

  wss <- (nrow(dataPassed)-1)*sum(apply(dataPassed, 2, var))

  for (i in 2:maxCluster) wss[i] <- sum(kmeans(dataPassed, nstart = 3,
                                             centers = i)$withinss)
    wss <- wss/max(wss)

   ProposedNClust <- min(c(seq(1, length.out = length(wss))[wss < (1-confidence)], length(wss)))

  return(list(wss, ProposedNClust))

}
