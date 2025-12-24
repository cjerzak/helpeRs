#' Clean and reorder regression tables for publication
#'
#' Takes a matrix of regression output and performs string replacements,
#' row name cleaning, and reordering so that the resulting table is suitable
#' for inclusion in LaTeX documents. This function is typically called internally
#' by \code{\link{Tables2Tex}} but can be used directly for custom table processing.
#'
#' The function performs several transformations:
#' \itemize{
#'   \item Renames rows using the optional \code{name_conversion_matrix}
#'   \item Moves summary statistics (R-squared, observations, etc.) to the bottom
#'   \item Expands camelCase variable names into readable format
#'   \item Handles \code{as.factor()} variable names by extracting factor levels
#'   \item Preserves common acronyms (GDP, AIC, MMD, SMD)
#' }
#'
#' @param t_FULL Matrix or data frame containing the raw regression table output,
#'   typically from \code{\link{GetTableEntry}}.
#' @param COLNAMES_VEC Character vector of column names to apply to the final table.
#'   Length must match the number of columns in \code{t_FULL}.
#' @param name_conversion_matrix Optional two-column matrix for renaming row labels.
#'   Column 1 contains regex patterns to match against row names,
#'   column 2 contains the replacement names. If \code{NULL} (default), no
#'   renaming is performed.
#'
#' @return A transformed data frame with cleaned row names, reordered rows
#'   (summary statistics at bottom), and the specified column names.
#'
#' @seealso \code{\link{Tables2Tex}} which calls this function to process full tables,
#'   \code{\link{GetTableEntry}} for extracting table entries from models
#'
#' @examples
#' \dontrun{
#' # Create a simple table
#' t_raw <- data.frame(Model1 = c("0.5 (2.1)*", "0.95", "100"))
#' row.names(t_raw) <- c("gdpPerCapita", "Adjusted R-squared", "Observations")
#'
#' # Clean and rename
#' name_mat <- matrix(c("gdpPerCapita", "GDP per Capita"), ncol = 2)
#' t_clean <- FullTransformer(t_raw, COLNAMES_VEC = "Model 1",
#'                            name_conversion_matrix = name_mat)
#' }
#'
#' @export

FullTransformer <- function(t_FULL,
                            COLNAMES_VEC,
                            name_conversion_matrix = NULL){
  upperFirst <- function(string) {
    sub("^(.)", "\\U\\1", string, perl = TRUE)
  }

  # old name:" TRANSFORM_TABLES_FULL
  if (!is.null(name_conversion_matrix)) {
    row.names(t_FULL) <- sapply(row.names(t_FULL),function(zer){
      match_ <- sapply(name_conversion_matrix[,1],function(zerr){ grepl(zer,pattern=zerr) })
      new_name <- name_conversion_matrix[which(match_),2]
      if(length(new_name) == 0){new_name <- zer}; return(new_name) } )
  }
  last_ <- which(row.names(t_FULL) %in%
                   c("Countries","Observations",
                     "Adjusted R-squared",
                     "AIC", "A.I.C.",
                     "Weak instruments","Wu-Hausman"))
  t_FULL <- rbind(t_FULL[-last_,],t_FULL[last_,])
  add_factor <- upperFirst( gsub(gsub(stringr::str_extract(
                                              row.names(t_FULL),
                                               pattern="as.factor\\(.*\\)"),
                          pattern="as.factor\\(",replacement=""),pattern="\\)",replacement=" - ") )
  add_factor[is.na(add_factor)] <- ""
  row.names(t_FULL) <- ( stringr::str_replace(row.names(t_FULL),
                                            pattern="as.factor\\(.*\\)",
                                            replacement = add_factor) )
  FullNameConverter <- function(text_){
    #text_ <- stringr::str_to_title(text_)
    splitOnUpper <- function(x){ gsub('([[:upper:]])', ' \\1', x)}
    text_ <- splitOnUpper(text_)
    text_ <- gsub(text_,pattern="G D P",replacement="GDP")
    text_ <- gsub(text_,pattern="M M D",replacement="MMD")
    text_ <- gsub(text_,pattern="A I C",replacement="AIC")
    text_ <- gsub(text_,pattern="S M D",replacement="SMD")
    text_ <- gsub(text_,pattern="\\( ", replacement="\\(")
    text_ <- gsub(text_,pattern="S D\\(",replacement="SD\\(")
    text_ <- gsub(text_,pattern=" \\( ",replacement=" - ")
    text_ <- gsub(text_,pattern="-$",replacement="")# replace hanging -'s at end of string
    text_ <- gsub(text_,pattern="- $",replacement="")# replace hanging -'s at end of string
    text_ <- gsub(text_,pattern="\\.\\.",replacement="")
    return(  text_  )  }
  row.names( t_FULL ) <- FullNameConverter(row.names(t_FULL))
  colnames( t_FULL ) <- COLNAMES_VEC
  return( t_FULL  )
}

