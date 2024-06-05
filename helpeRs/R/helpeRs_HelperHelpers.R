#' Implements...
#'
#' @usage
#'
#' vcovCluster(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `vcovCluster` implements...
#'
#' @examples
#'
#' # Perform analysis
#' TableEntry <- vcovCluster()
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

#' Implements...
#'
#' @usage
#'
#' vcovCluster(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `vcovCluster` implements...
#'
#' @examples
#'
#' # Perform analysis
#' TableEntry <- vcovCluster()
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
#' fixZeroEndings(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `fixZeroEndings` implements...
#'
#' @examples
#'
#' # Perform analysis
#' fixZeroEndings <- getTableEntry()
#'
#' print( fixZeroEndings )
#'
#' @export
#'
#' @md
fixZeroEndings <- function(zr,roundAt=2){
  unlist( lapply(strsplit(as.character(zr),split="\\."),function(l_){
    if(length(l_) == 1){ retl <- paste(l_, paste(rep("0",times=roundAt),collapse=""),sep=".") }
    if(length(l_) == 2){
      retl <- paste(l_[1], paste(l_[2], paste(rep("0",times=roundAt-nchar(l_[2])),collapse=""),sep=""),
                    sep = ".") }
    return( retl  )
  }) ) }

##
cleanStars <- function(zer){
  zer = sapply(zer,function(sa)gsub(sa,pattern="\\} c", replace="\\} l"))
  zer = sapply(zer,function(sa)gsub(sa,pattern="!htbp",replace="htbp"))
  zer = sapply(zer,function(sa)gsub(sa,pattern="checkmark",replace="\\\\checkmark"))
  zer = sapply(zer,function(sa)gsub(sa,pattern="\\\\textasteriskcentered",replace="$^\\*$"))
  zer <- gsub(zer,pattern="Continuous covariates",replace="\\\\emph{Continuous covariates}")
  zer <- gsub(zer,pattern="Instruments",replace="\\\\emph{Instruments}")
  zer <- gsub(zer,pattern="Factor covariates",replace="\\\\emph{Factor covariates}")
  zer <- gsub(zer,pattern="Control variables",replace="\\\\emph{Control variables}")
  zer <- gsub(zer,pattern="Other statistics",replace="\\\\emph{Other statistics}")
  zer <- gsub(zer,pattern="Body indicators",replace="\\\\emph{Body indicators}")
  zer <- gsub(zer,pattern="xxx",replace=" ")
  zer <- gsub(zer,pattern="space0",replace=" ")
  zer <- gsub(zer,pattern="space1",replace=" ")
  zer <- gsub(zer,pattern="space2",replace=" ")
  zer <- gsub(zer,pattern="space3",replace=" ")
  zer <- gsub(zer,pattern="space4",replace=" ")
  zer <- gsub(zer,pattern="space5",replace=" ")
  zer <- gsub(zer,pattern="spaceX",replace=" ")
  names(zer) <- NULL
  captionIndices <- grep(zer,pattern="caption\\{"):(grep(zer,pattern="label\\{"))
  insertCaptionAfter <- grep(zer,pattern="end\\{tabular")

  # re-order so captions are at bottom of table
  zer <- c(zer[1:(min(captionIndices)-1)],
           zer[(max(captionIndices)+1):(insertCaptionAfter)],
           zer[captionIndices],
           zer[(insertCaptionAfter+1):length(zer)])
  return( zer )
}
