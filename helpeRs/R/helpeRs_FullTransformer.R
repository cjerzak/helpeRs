#' Implements...
#'
#' @usage
#'
#' FullTransformer(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `FullTransformer` implements...
#'
#' @examples
#'
#' # Perform analysis
#' TableEntry <- FullTransformer()
#'
#' print( TableEntry )
#'
#' @export
#'
#' @md

FullTransformer <- function(t_FULL,
                            COLNAMES_VEC){
  upperFirst <- function(string) {
    sub("^(.)", "\\U\\1", string, perl = TRUE)
  }

  # old name:" TRANSFORM_TABLES_FULL
  row.names(t_FULL) <- sapply(row.names(t_FULL),function(zer){
    match_ <- sapply(name_conversion_matrix[,1],function(zerr){ grepl(zer,pattern=zerr) })
    new_name <- name_conversion_matrix[which(match_),2]
    if(length(new_name) == 0){new_name <- zer}; return(new_name) } )
  last_ <- which(row.names(t_FULL) %in%
                   c("Countries","Observations",
                     "Adjusted R-squared",
                     "AIC", "A.I.C.",
                     "Weak instruments","Wu-Hausman"))
  t_FULL <- rbind(t_FULL[-last_,],t_FULL[last_,])
  add_factor <- upperFirst( gsub(gsub(stringr::str_extract(
                                              row.names(t_FULL),
                                               pattern="as.factor\\(.*\\)"),
                          pattern="as.factor\\(",replace=""),pattern="\\)",replace=" - ") )
  add_factor[is.na(add_factor)] <- ""
  row.names(t_FULL) <- ( stringr::str_replace(row.names(t_FULL),
                                            pattern="as.factor\\(.*\\)",
                                            replacement = add_factor) )
  FullNameConverter <- function(text_){
    #text_ <- stringr::str_to_title(text_)
    splitOnUpper <- function(x){ gsub('([[:upper:]])', ' \\1', x)}
    text_ <- splitOnUpper(text_)
    text_ <- gsub(text_,pattern="G D P",replace="GDP")
    text_ <- gsub(text_,pattern="M M D",replace="MMD")
    text_ <- gsub(text_,pattern="A I C",replace="AIC")
    text_ <- gsub(text_,pattern="S M D",replace="SMD")
    text_ <- gsub(text_,pattern="\\( ", replace="\\(")
    text_ <- gsub(text_,pattern="S D\\(",replace="SD\\(")
    text_ <- gsub(text_,pattern=" \\( ",replace=" - ")
    text_ <- gsub(text_,pattern="-$",replace="")# replace hanging -'s at end of string
    text_ <- gsub(text_,pattern="- $",replace="")# replace hanging -'s at end of string
    text_ <- gsub(text_,pattern="\\.\\.",replace="")
    return(  text_  )  }
  row.names( t_FULL ) <- FullNameConverter(row.names(t_FULL))
  colnames( t_FULL ) <- COLNAMES_VEC
  return( t_FULL  )
}

