#' Cluster-robust covariance matrix estimator
#'
#' Computes a clustered sandwich covariance matrix for a fitted model using the
#' method of Arellano (1987). This provides standard errors that are robust to
#' arbitrary within-cluster correlation.
#'
#' The function applies the degrees-of-freedom correction
#' \eqn{(M/(M-1)) \times ((N-1)/(N-K))}{(M/(M-1)) * ((N-1)/(N-K))} where M is
#' the number of clusters, N is the number of observations, and K is the number
#' of parameters.
#'
#' @param fm A fitted model object (typically from \code{lm()} or \code{glm()}).
#'   For \code{polr} models from \pkg{MASS}, predictions are handled specially.
#' @param clvar Character string giving the name of the clustering variable.
#'   This variable must exist in the data used to fit \code{fm}.
#'
#' @return A K x K covariance matrix where K is the number of model coefficients.
#'
#' @references
#' Arellano, M. (1987). Computing Robust Standard Errors for Within-Groups
#' Estimators. \emph{Oxford Bulletin of Economics and Statistics}, 49(4), 431-434.
#'
#' @seealso \code{\link{GetTableEntry}} which uses this function for clustered
#'   standard errors, \code{\link[sandwich]{vcovHC}} for heteroskedasticity-robust
#'   covariance without clustering
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' # Cluster by number of cylinders
#' V_clust <- vcovCluster(fit, "cyl")
#'
#' # Use with coeftest for clustered standard errors
#' library(lmtest)
#' coeftest(fit, vcov. = V_clust)
#' }
#'
#' @export
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
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)
}

#' Convert factors to numeric values
#'
#' Converts a vector to numeric by first coercing to character. This is the
#' correct way to extract numeric values from factors, as direct coercion with
#' \code{as.numeric()} returns factor level indices rather than the underlying
#' values.
#'
#' @param x A vector to convert, typically a factor whose levels are numeric
#'   strings (e.g., \code{factor(c("1.5", "2.3", "3.1"))}).
#'
#' @return A numeric vector. Values that cannot be coerced to numeric become
#'   \code{NA} with a warning.
#'
#' @seealso \code{\link{cols2numeric}} for converting multiple columns of a
#'   data frame
#'
#' @examples
#' # Factor with numeric levels
#' x <- factor(c("1.5", "2.3", "3.1"))
#'
#' # Wrong way - returns level indices (1, 2, 3)
#' as.numeric(x)
#'
#' # Correct way - returns actual values
#' f2n(x)  # Returns c(1.5, 2.3, 3.1)
#'
#' @export
f2n <- function(x){as.numeric(as.character(x))}

#' Widen margins in a LaTeX table string
#'
#' Adds horizontal margin adjustments to a LaTeX table by wrapping the table
#' content in an \code{adjustwidth} environment from the \pkg{ragged2e} package.
#' This is useful when tables are too wide to fit within standard margins.
#'
#' The function extends margins by 0.5 inches on each side, effectively allowing
#' the table to be 1 inch wider than the text width.
#'
#' @param x Character vector containing the LaTeX table code, typically the
#'   output from \code{\link[stargazer]{stargazer}} or \code{\link{Tables2Tex}}.
#'
#' @return A character vector with the modified LaTeX code. The \code{adjustwidth}
#'   environment is inserted inside the \code{table} environment.
#'
#' @section LaTeX Requirements:
#' The output requires the \pkg{ragged2e} package in your LaTeX preamble:
#' \preformatted{\\usepackage{ragged2e}}
#'
#' @seealso \code{\link{Tables2Tex}} for generating LaTeX tables
#'
#' @examples
#' \dontrun{
#' # Read an existing table and widen its margins
#' tex_lines <- readLines("my_table.tex")
#' tex_wide <- WidenMargins(tex_lines)
#' writeLines(tex_wide, "my_table_wide.tex")
#' }
#'
#' @export
WidenMargins <- function(x){
  x = gsub(x,  pattern="\\\\begin\\{table\\}\\[htbp\\]",
           replace="\\\\begin\\{table\\}[htbp]\\\\begin\\{adjustwidth\\}\\{-.5in\\}\\{-.5in\\}")
  x = gsub(x,  pattern="\\\\end\\{table\\}",
           replace="\\\\end\\{adjustwidth\\}\\\\end\\{table\\}")
  return(x)
}

#' Ensure numbers have a fixed number of decimal places
#'
#' Pads numeric strings with trailing zeros so that all values have exactly
#' \code{roundAt} digits after the decimal point. This ensures consistent
#' formatting in regression tables where column alignment matters.
#'
#' @param zr A character or numeric vector to process. Numeric values are first
#'   converted to character.
#' @param roundAt Integer specifying the desired number of decimal places.
#'   Default is 2.
#'
#' @return A character vector with values padded to have exactly \code{roundAt}
#'   decimal places. Values without a decimal point receive one followed by
#'   the appropriate number of zeros.
#'
#' @seealso \code{\link{GetTableEntry}} which uses this function to format
#'   coefficient estimates
#'
#' @examples
#' # Pad to 2 decimal places (default)
#' fixZeroEndings(c("1.5", "2", "3.14"))
#' # Returns: c("1.50", "2.00", "3.14")
#'
#' # Pad to 3 decimal places
#' fixZeroEndings(c(1.5, 2.33, 3), roundAt = 3)
#' # Returns: c("1.500", "2.330", "3.000")
#'
#' @export
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
  
  zer <- gsub(zer,pattern="StartEmph",replace="\\\\emph{")
  zer <- gsub(zer,pattern="EndEmph",replace="}")
  
  zer <- gsub(zer,pattern="StartMakeCell",replace="\\\\makecell{")
  zer <- gsub(zer,pattern="EndMakeCell",replace="}")
  
  zer <- gsub(zer,pattern="LINEBREAK",replace=" \\\\\\\\ ")
              
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

  # Find required pattern indices
  captionIdx <- grep(zer, pattern = "caption\\{")
  labelIdx <- grep(zer, pattern = "label\\{")
  insertCaptionAfter <- grep(zer, pattern = "end\\{tabular")


  # Only re-order if all required patterns are found
  if (length(captionIdx) > 0 && length(labelIdx) > 0 && length(insertCaptionAfter) > 0) {
    captionIndices <- captionIdx[1]:labelIdx[1]
    insertCaptionAfter <- insertCaptionAfter[1]

    # re-order so captions are at bottom of table
    zer <- c(zer[1:(min(captionIndices)-1)],
             zer[(max(captionIndices)+1):(insertCaptionAfter)],
             zer[captionIndices],
             zer[(insertCaptionAfter+1):length(zer)])
  }
  return( zer )
}

print2 <- function(text, quiet = F){
  if(!quiet){ print( sprintf("[%s] %s" ,format(Sys.time(), "%Y-%m-%d %H:%M:%S"),text) ) }
}