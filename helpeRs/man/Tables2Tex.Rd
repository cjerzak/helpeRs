% Generated by manual editing
% Please edit documentation in R/helpeRs_Tables2Tex.R
\name{Tables2Tex}
\alias{Tables2Tex}
\title{Generate publication-ready regression tables}
\usage{
Tables2Tex(reg_list, clust_id, seType = "analytical",
           checkmark_list = NULL, addrow_list = NULL,
           saveFolder = "./", nameTag = "Table", saveFull = TRUE,
           tabCaption = "", model.names = NULL, NameConversionMat = NULL,
           DoFullTableKey = TRUE, superunit_covariateName = "country",
           superunit_label = "Countries", font.size = "footnotesize",
           inParens = "tstat", font.size.full = "footnotesize")
}
\arguments{
  \item{reg_list}{List of fitted models or character strings.}
  \item{clust_id}{Clustering variable name.}
  \item{seType}{Type of standard errors.}
  \item{checkmark_list}{Optional list of binary indicators.}
  \item{addrow_list}{Optional named list of additional rows.}
  \item{saveFolder}{Directory for output files.}
  \item{nameTag}{Base name for files.}
  \item{saveFull}{Produce the full table as well.}
  \item{tabCaption}{Caption for the condensed table.}
  \item{model.names}{Column headings for models.}
  \item{NameConversionMat}{Two-column matrix for renaming rows.}
  \item{DoFullTableKey}{Mention the full table in the caption.}
  \item{superunit_covariateName}{Variable used to count higher level units.}
  \item{superunit_label}{Label for that count.}
  \item{font.size}{Font size for the short table.}
  \item{inParens}{"tstat" or "se" for entries in parentheses.}
  \item{font.size.full}{Font size for the full table.}
}
\value{Invisibly returns \code{NULL}. Files are written to disk.}
\description{Collates regression results and writes LaTeX tables using \code{stargazer}.}
\examples{\dontrun{Tables2Tex(list(lm(y~x,data)), NULL)}}
