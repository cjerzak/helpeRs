#' Convert a stargazer table to a self-contained longtable
#'
#' Transforms the LaTeX output from \code{\link[stargazer]{stargazer}} into a
#' longtable environment suitable for multi-page regression tables. This function
#' handles the conversion automatically, including header repetition on continued
#' pages.
#'
#' The function performs several transformations:
#' \itemize{
#'   \item Replaces \code{table} with \code{longtable} environment
#'   \item Wraps content in the specified font size environment
#'   \item Adds \code{\\endfirsthead} and \code{\\endhead} markers for header
#'     repetition
#'   \item Includes "(Continued from previous page)" note on subsequent pages
#'   \item Removes incompatible stargazer formatting directives
#' }
#'
#' @param stargazer_text Character vector containing the raw lines produced by
#'   \code{stargazer()} (typically captured with \code{capture.output()}).
#' @param fontsize Character string specifying the LaTeX font size environment
#'   to wrap the table in. Common values include \code{"footnotesize"},
#'   \code{"scriptsize"}, or \code{"tiny"}. Default is \code{"footnotesize"}.
#'
#' @return A character vector of LaTeX code ready to be written to a .tex file
#'   using \code{write()} or \code{cat()}.
#'
#' @section LaTeX Requirements:
#' The output requires the \code{longtable} package in your LaTeX preamble:
#' \preformatted{\\usepackage{longtable}}
#'
#' @seealso \code{\link{Tables2Tex}} which calls this function internally,
#'   \code{\link{FullTransformer}} for table cleaning before conversion
#'
#' @examples
#' \dontrun{
#' # Capture stargazer output and convert to longtable
#' library(stargazer)
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' sg_output <- capture.output(stargazer(fit))
#'
#' # Convert to longtable format
#' lt_output <- Stargazer2FullTable(sg_output, fontsize = "scriptsize")
#'
#' # Write to file
#' write(lt_output, file = "full_table.tex")
#' }
#'
#' @export
#' 
Stargazer2FullTable <- function(stargazer_text, fontsize = "footnotesize"){
  stargazer_text_orig <- stargazer_text
  stargazer_text = sapply(stargazer_text,function(sa) gsub(sa,pattern="\\[!htbp\\]",replacement=""), 
                          USE.NAMES = FALSE)
  stargazer_text = sapply(stargazer_text,function(sa) gsub(sa,pattern="\\\\centering",replacement=""), 
                          USE.NAMES = FALSE)
  col_arrange <- stargazer_text[grepl(stargazer_text,pattern = "\\\\begin\\{tabular\\}")]
  pattern <- "\\\\begin\\{tabular\\}\\{.*?\\\\extracolsep\\{5pt\\}\\}\\s+(\\w+)"
  col_arrange <- sub(pattern, "\\1", col_arrange)
  col_arrange <- gsub(gsub(col_arrange,pattern="\\}",replacement=""),pattern=" ", replacement="")
  col_arrange <- paste("l",substr(col_arrange,start = 2, stop = nchar(col_arrange)),sep="")
  stargazer_text = sapply(stargazer_text,function(sa){gsub(sa, pattern="\\\\begin\\{table\\}",
                                                           replacement= sprintf("\\\\begin\\{%s\\}
                                                            \\\\begin\\{longtable\\}\\{%s\\}",
                                                                       fontsize, col_arrange))},
                          USE.NAMES = FALSE)
  stargazer_text = sapply(stargazer_text,function(sa){ gsub(sa,
                                                          pattern="\\\\end\\{table\\}",
                                                          replacement=sprintf("\\\\end\\{longtable\\}
                                                            \\\\end\\{%s\\}", fontsize)
                                                          ) } ,
                          USE.NAMES = FALSE)
  # see:
  # https://tex.stackexchange.com/questions/71549/how-can-i-repeat-the-header-but-not-the-caption-with-longtable
  stargazer_text  <- stargazer_text[!grepl(stargazer_text,pattern = "\\\\begin\\{tabular\\}")]
  if(length(stargazer_text) > 40){ 
    endhead_index <- grep(stargazer_text,pattern="\\&")[1]+1
      stargazer_text <- c(stargazer_text[1:endhead_index],
                          "\\endfirsthead",
                          sprintf("\\multicolumn{%s}{c}",nchar(col_arrange)),
                          "{\\it (Continued from previous page)} \\\\",
                          stargazer_text[(endhead_index-3):endhead_index],
                          "\\endhead",
                          stargazer_text[(endhead_index+1):length(stargazer_text)])
  }
  stargazer_text <- gsub(stargazer_text, pattern = '"', replacement = "")
  stargazer_text <- gsub(stargazer_text, pattern = "C L I P- R S I C D", replacement = "CLIP-RSICD")
  stargazer_text <- gsub(stargazer_text, pattern = "Vi T", replacement = "ViT")
  stargazer_text <- sapply(stargazer_text,function(sa){ gsub(sa,pattern="\\\\end\\{tabular\\}", replacement="") })
  return(  stargazer_text )
}