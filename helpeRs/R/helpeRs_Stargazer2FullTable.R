
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

Stargazer2FullTable <- function(stargazer_text, fontsize = "footnotesize"){
  stargazer_text_orig <- stargazer_text
  stargazer_text = sapply(stargazer_text,function(sa)gsub(sa,pattern="\\[!htbp\\]",replace=""))
  stargazer_text = sapply(stargazer_text,function(sa)gsub(sa,pattern="\\\\centering",replace=""))
  col_arrange <- stargazer_text[grepl(stargazer_text,pattern = "\\\\begin\\{tabular\\}")]
  pattern <- "\\\\begin\\{tabular\\}\\{.*?\\\\extracolsep\\{5pt\\}\\}\\s+(\\w+)"
  col_arrange <- sub(pattern, "\\1", col_arrange)
  col_arrange <- gsub(gsub(col_arrange,pattern="\\}",replace=""),pattern=" ", replace="")
  col_arrange <- paste("l",substr(col_arrange,start = 2, stop = nchar(col_arrange)),sep="")
  stargazer_text = sapply(stargazer_text,function(sa){gsub(sa, pattern="\\\\begin\\{table\\}",
                                                           replace= sprintf("\\\\begin\\{%s\\}
                                                            \\\\begin\\{longtable\\}\\{%s\\}",
                                                                       fontsize, col_arrange))})
  stargazer_text = sapply(stargazer_text,function(sa){ gsub(sa,
                                                          pattern="\\\\end\\{table\\}",
                                                          replace=sprintf("\\\\end\\{longtable\\}
                                                            \\\\end\\{%s\\}", fontsize)
                                                          ) } )
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
