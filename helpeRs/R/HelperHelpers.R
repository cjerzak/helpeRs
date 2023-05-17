#' Implements...
#'
#' @usage
#'
#' getTableEntry(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `getTableEntry` implements...
#'
#' @examples
#'
#' # Perform analysis
#' TableEntry <- getTableEntry()
#'
#' print( TableEntry )
#'
#' @export
#'
#' @md

vcovCluster <- function(fm, clvar){
  # R-codes (www.r-project.org) for computing
  # clustered-standard errors. Mahmood Arai, Jan 26, 2008.
  # The arguments of the function are:
  # fitted model, cluster1 and cluster2
  # You need to install libraries `sandwich' and `lmtest'
  x <- eval(fm$call$data, envir = parent.frame())
  if ("polr" %in% class(fm)) {
    require(MASS)
    cluster <- x[rownames(predict(fm, type = "probs")), clvar]
  } else {
    cluster <- x[names(predict(fm)), clvar]
  }
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- dim(vcov(fm))[1]
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
}

#' Implements...
#'
#' @usage
#'
#' getTableEntry(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `getTableEntry` implements...
#'
#' @examples
#'
#' # Perform analysis
#' TableEntry <- getTableEntry()
#'
#' print( TableEntry )
#'
#' @export
#'
#' @md
f2n<-function(.){as.numeric(as.character(.))}

#' Implements...
#'
#' @usage
#'
#' getTableEntry(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `getTableEntry` implements...
#'
#' @examples
#'
#' # Perform analysis
#' TableEntry <- getTableEntry()
#'
#' print( TableEntry )
#'
#' @export
#'
#' @md
WidenMargins <- function(x){
  x = gsub(x,  pattern="\\\\begin\\{table\\}\\[htbp\\]",
           replace="\\\\begin\\{table\\}[htbp]\\\\begin\\{adjustwidth\\}\\{-.5in\\}\\{-.5in\\}")
  x = gsub(x,  pattern="\\\\end\\{table\\}",
           replace="\\\\end\\{adjustwidth\\}\\\\end\\{table\\}")
}
