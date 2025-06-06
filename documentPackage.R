{
setwd("~/Documents/helpeRs-software")

package_path <- "~/Documents/helpeRs-software/helpeRs"

devtools::document(package_path)
try(file.remove(sprintf("./helpeRs.pdf")),T)
system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(package_path)))

# Check package to ensure it meets CRAN standards.
# devtools::check( package_path )

#install.packages(package_path)

#install.packages( "~/Documents/helpeRs-software/helpeRs",repos = NULL, type = "source")

#install.packages("~/Library/gurobi911/mac64/R/gurobi_9.1-1_R_4.0.2.tgz",repos=NULL)
}
