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

FullTransformer <- function(t_FULL,COLNAMES_VEC){
  # old name:" TRANSFORM_TABLES_FULL
  row.names(t_FULL) <- gsub(row.names(t_FULL) ,
                            pattern= "groupTypePresentb_gender",replace = "Gender")
  row.names(t_FULL) <- gsub(row.names(t_FULL) ,
                            pattern= "groupTypePresentc_language",replace = "Language")
  row.names(t_FULL) <- gsub(row.names(t_FULL) ,
                            pattern= "groupTypePresentd_religion",replace = "Religion")
  row.names(t_FULL) <- gsub(row.names(t_FULL) ,
                            pattern= "groupType_gender",replace = "Group Type - Gender")
  row.names(t_FULL) <- sapply(row.names(t_FULL),function(zer){
    match_ <- sapply(name_conversion_matrix[,1],function(zerr){ grepl(zer,pattern=zerr) })
    new_name <- name_conversion_matrix[which(match_),2]
    if(length(new_name) == 0){new_name <- zer}; return(new_name) } )
  last_ <- which(row.names(t_FULL) %in%
                   c("Countries","Observations","Adjusted R-squared",
                     "Weak instruments","Wu-Hausman"))
  t_FULL <- rbind(t_FULL[-last_,],t_FULL[last_,])
  add_factor <- gsub(gsub(stringr::str_extract(row.names(t_FULL),
                                               pattern="as.factor\\(.*\\)"),
                          pattern="as.factor\\(",replace=""),pattern="\\)",replace=" - ")
  add_factor[is.na(add_factor)] <- ""
  row.names(t_FULL) <- stringr::str_replace(row.names(t_FULL),
                                            pattern="as.factor\\(.*\\)",
                                            replacement = add_factor)
  FullNameConverter <- function(text_){
    text_ <- gsub(text_,pattern="quotas_quotaType",replace="Gender Quotas")
    text_ <- gsub(text_,pattern="Ruedin_ethnicity_quota_master",replace="Ethnic Quotas")
    text_ <- gsub(text_,pattern="bodyType",replace="Body Type ")
    text_ <- gsub(text_,pattern="f2n\\(round\\)",replace="Round 2 Indicator")
    text_ <- gsub(text_,pattern="glp_country",replace="Country")
    text_ <- gsub(text_,pattern="groupType",replace="Group Type ")
    text_ <- gsub(text_,pattern="executive",replace="Executive")
    text_ <- gsub(text_,pattern="lowerHouse",replace="Lower House")
    text_ <- gsub(text_,pattern="party",replace="Party")
    text_ <- gsub(text_,pattern="supremeCourt",replace="Supreme Court")
    text_ <- gsub(text_,pattern="upperHouse",replace="Upper House")
    #text_ <- stringr::str_to_title(text_)
    splitOnUpper <- function(x){ gsub('([[:upper:]])', ' \\1', x)}
    text_ <- splitOnUpper(text_)
    text_ <- gsub(text_,pattern="G D P",replace="GDP")
    text_ <- gsub(text_,pattern="S D\\(",replace="SD\\(")
    text_ <- gsub(text_,pattern=" \\( ",replace=" - ")
    text_ <- gsub(text_,pattern="gender",replace="Gender")
    text_ <- gsub(text_,pattern="language",replace="Language")
    text_ <- gsub(text_,pattern="selection Rule",replace="Selection Rule")
    text_ <- gsub(text_,pattern="religion",replace="Religion")
    text_ <- gsub(text_,pattern="selectionRule",replace="Selection Rule")
    text_ <- gsub(text_,pattern="-$",replace="")# replace hanging -'s at end of string
    text_ <- gsub(text_,pattern="- $",replace="")# replace hanging -'s at end of string
    return(  text_  )  }
  row.names(t_FULL) <- FullNameConverter(row.names(t_FULL))
  colnames(t_FULL) <- COLNAMES_VEC
  return( t_FULL  )
}


#' Implements...
#'
#' @usage
#'
#' Tables2Tex(...)
#'
#' @param x Description
#'
#' @return `z` Description
#' @export
#'
#' @details `Tables2Tex` implements...
#'
#' @examples
#'
#' # Perform analysis
#' Tables2Tex()
#'
#' print( TableEntries2Tex )
#'
#' @export
#'
#' @md

Stargazer2FullTable <- function(stargazer_text){
  stargazer_text_orig <- stargazer_text
  stargazer_text = sapply(stargazer_text,function(sa)gsub(sa,pattern="\\[!htbp\\]",replace=""))
  stargazer_text = sapply(stargazer_text,function(sa)gsub(sa,pattern="\\\\centering",replace=""))
  col_arrange <- stargazer_text[grepl(stargazer_text,pattern = "\\\\begin\\{tabular\\}")]
  pattern <- "\\\\begin\\{tabular\\}\\{.*?\\\\extracolsep\\{5pt\\}\\}\\s+(\\w+)"
  col_arrange <- sub(pattern, "\\1", col_arrange)
  col_arrange <- gsub(gsub(col_arrange,pattern="\\}",replace=""),pattern=" ", replace="")
  col_arrange <- paste("l",substr(col_arrange,start = 2, stop = nchar(col_arrange)),sep="")
  stargazer_text = sapply(stargazer_text,function(sa){gsub(sa, pattern="\\\\begin\\{table\\}",
                                                           replace=
                                                             sprintf("\\\\begin\\{footnotesize\\}
                                                            \\\\begin\\{longtable\\}\\{%s\\}",
                                                                     col_arrange))})
  stargazer_text = sapply(stargazer_text,function(sa)gsub(sa,
                                                          pattern="\\\\end\\{table\\}",
                                                          replace="\\\\end\\{longtable\\}
                                                            \\\\end\\{footnotesize\\}
                                                            "))
  # see:
  # https://tex.stackexchange.com/questions/71549/how-can-i-repeat-the-header-but-not-the-caption-with-longtable
  stargazer_text  <- stargazer_text[!grepl(stargazer_text,pattern = "\\\\begin\\{tabular\\}")]
  endhead_index <- grep(stargazer_text,pattern="\\&")[1]+1
  stargazer_text <- c(stargazer_text[1:endhead_index],
                      "\\endfirsthead",
                      sprintf("\\multicolumn{%s}{c}",nchar(col_arrange)),
                      "{\\it (Continued from previous page)} \\\\",
                      stargazer_text[(endhead_index-3):endhead_index],
                      "\\endhead",
                      stargazer_text[(endhead_index+1):length(stargazer_text)])
  stargazer_text <- sapply(stargazer_text,function(sa)gsub(sa,pattern="\\\\end\\{tabular\\}", replace=""))
  return(  stargazer_text )
}
