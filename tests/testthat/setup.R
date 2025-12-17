# Setup file for testthat
# This file is run before tests to set up the testing environment

# Suppress messages from library loading
suppressPackageStartupMessages({
  library(helpeRs)
  # Load sandwich for estfun and sandwich functions used by vcovCluster
  if (requireNamespace("sandwich", quietly = TRUE)) {
    library(sandwich)
  }
  # Load lmtest for coeftest used in tests
  if (requireNamespace("lmtest", quietly = TRUE)) {
    library(lmtest)
  }
})

# Set a seed for reproducibility in tests that use random data
set.seed(90120)
