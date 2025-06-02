#' Convert a stargazer table to a self-contained longtable.
#'
#' @param stargazer_text A character vector – the raw lines produced by
#'   `stargazer()` (as captured with `capture.output()` or the browser() trick).
#' @param fontsize       LaTeX font-size environment to wrap the table in
#'                       (e.g. "footnotesize", "scriptsize").
#' @param continued_note Logical.  If TRUE (default) add the
#'                       "(Continued from previous page)" banner that longtable
#'                       prints *only* on page ≥ 2.  Set to FALSE if you do not
#'                       want that banner at all.
#'
#' @return A character vector ready to be `cat()`-ed into a .tex file.
#' @export
#'
#' @examples
#' tex_lines <- Stargazer2FullTable(stargazer_text)
#' cat(tex_lines, file = "table.tex", sep = "\n")
#' 
Stargazer2FullTable <- function(stargazer_text, fontsize = "footnotesize"){
  stargazer_text_orig <- stargazer_text
  stargazer_text = sapply(stargazer_text,function(sa)gsub(sa,pattern="\\[!htbp\\]",replace=""), USE.NAMES = FALSE)
  stargazer_text = sapply(stargazer_text,function(sa)gsub(sa,pattern="\\\\centering",replace=""), USE.NAMES = FALSE)
  col_arrange <- stargazer_text[grepl(stargazer_text,pattern = "\\\\begin\\{tabular\\}")]
  pattern <- "\\\\begin\\{tabular\\}\\{.*?\\\\extracolsep\\{5pt\\}\\}\\s+(\\w+)"
  col_arrange <- sub(pattern, "\\1", col_arrange)
  col_arrange <- gsub(gsub(col_arrange,pattern="\\}",replace=""),pattern=" ", replace="")
  col_arrange <- paste("l",substr(col_arrange,start = 2, stop = nchar(col_arrange)),sep="")
  stargazer_text = sapply(stargazer_text,function(sa){gsub(sa, pattern="\\\\begin\\{table\\}",
                                                           replace= sprintf("\\\\begin\\{%s\\}
                                                            \\\\begin\\{longtable\\}\\{%s\\}",
                                                                       fontsize, col_arrange))}, USE.NAMES = FALSE)
  stargazer_text = sapply(stargazer_text,function(sa){ gsub(sa,
                                                          pattern="\\\\end\\{table\\}",
                                                          replace=sprintf("\\\\end\\{longtable\\}
                                                            \\\\end\\{%s\\}", fontsize)
                                                          ) } , USE.NAMES = FALSE)
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
  stargazer_text <- gsub(stargazer_text, pattern = '"', replace = "")
  stargazer_text <- gsub(stargazer_text, pattern = "C L I P- R S I C D", replace = "CLIP-RSICD")
  stargazer_text <- gsub(stargazer_text, pattern = "Vi T", replace = "ViT")
  stargazer_text <- sapply(stargazer_text,function(sa){ gsub(sa,pattern="\\\\end\\{tabular\\}", replace="") })
  return(  stargazer_text )
}