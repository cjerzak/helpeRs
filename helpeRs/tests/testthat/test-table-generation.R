# Tests for table generation functions: GetTableEntry, Tables2Tex, FullTransformer, Stargazer2FullTable

write_bootstrap_files <- function(data, directory, prefix, reps = 8) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, file.path(directory, sprintf("%s_0.csv", prefix)), row.names = FALSE)
  for (rep_idx in seq_len(reps)) {
    sample_index <- sample.int(nrow(data), size = nrow(data), replace = TRUE)
    utils::write.csv(
      data[sample_index, , drop = FALSE],
      file.path(directory, sprintf("%s_%d.csv", prefix, rep_idx)),
      row.names = FALSE
    )
  }
}

read_tex_body <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^% Date and time:", lines)]
  if (length(lines) <= 2) {
    return(lines)
  }
  lines[-c(1, 2)]
}

# GetTableEntry tests

test_that("GetTableEntry returns a data frame", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  result <- GetTableEntry(fit, clust_id = NULL, NAME = "Model 1")

  expect_s3_class(result, "data.frame")
})

test_that("GetTableEntry has correct structure for simple model", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  result <- GetTableEntry(fit, clust_id = NULL, NAME = "Model 1")

  # Should have rows for wt, hp (intercept excluded by default), plus statistics
  expect_true("wt" %in% colnames(result))
  expect_true("hp" %in% colnames(result))
  expect_true("Observations" %in% colnames(result))
  expect_true("Adjusted R-squared" %in% colnames(result))
})

test_that("GetTableEntry includes intercept when keepCoef1 = TRUE", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  result <- GetTableEntry(fit, clust_id = NULL, NAME = "Model 1", keepCoef1 = TRUE)

  expect_true("(Intercept)" %in% colnames(result))
})

test_that("GetTableEntry excludes intercept by default", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  result <- GetTableEntry(fit, clust_id = NULL, NAME = "Model 1")

  expect_false("(Intercept)" %in% colnames(result))
})

test_that("GetTableEntry works with clustered standard errors", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  result_clust <- GetTableEntry(fit, clust_id = "cyl", NAME = "Clustered")
  result_robust <- GetTableEntry(fit, clust_id = NULL, NAME = "Robust")

  # Both should work and return data frames
  expect_s3_class(result_clust, "data.frame")
  expect_s3_class(result_robust, "data.frame")
  expect_false(identical(result_clust$wt, result_robust$wt))
})

test_that("GetTableEntry respects iv_round parameter", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  fit <- lm(mpg ~ wt, data = mtcars)

  # Test with iv_round = 2 (default)
  result_2 <- GetTableEntry(fit, clust_id = NULL, iv_round = 2)

  # Should work and return data frame
  expect_s3_class(result_2, "data.frame")
  # Check that values are formatted to 2 decimal places
  expect_true(any(grepl("\\.", result_2$wt)))
})

test_that("GetTableEntry handles inParens = 'se'", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  fit <- lm(mpg ~ wt, data = mtcars)

  result_tstat <- GetTableEntry(fit, clust_id = NULL, inParens = "tstat")
  result_se <- GetTableEntry(fit, clust_id = NULL, inParens = "se")

  expect_equal(result_tstat$wt, "-5.34 (-8.17)*")
  expect_equal(result_se$wt, "-5.34 (0.65)*")
})

test_that("GetTableEntry handles single covariate model", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  fit <- lm(mpg ~ wt, data = mtcars)

  result <- GetTableEntry(fit, clust_id = NULL, NAME = "Single Var")

  expect_s3_class(result, "data.frame")
  expect_true("wt" %in% colnames(result))
})

test_that("GetTableEntry handles GLM models", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  mtcars$am_binary <- mtcars$am
  fit <- glm(am_binary ~ wt + hp, data = mtcars, family = binomial)

  result <- GetTableEntry(fit, clust_id = NULL, NAME = "Logit Model")

  expect_s3_class(result, "data.frame")
  expect_true("AIC" %in% colnames(result))  # GLMs use AIC instead of R-squared
})

test_that("GetTableEntry counts observations correctly", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  result <- GetTableEntry(fit, clust_id = NULL)

  # mtcars has 32 observations
  expect_equal(result$Observations, "32")
})

test_that("GetTableEntry includes IV diagnostics for ivreg models", {
  skip_if_not_installed("AER")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  data("CigarettesSW", package = "AER")
  c1995 <- subset(CigarettesSW, year == 1995)
  c1995$rprice <- c1995$price / c1995$cpi
  c1995$rincome <- c1995$income / c1995$population / c1995$cpi
  c1995$tax <- c1995$tax / c1995$cpi

  fit <- AER::ivreg(
    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tax,
    data = c1995
  )
  result <- GetTableEntry(fit, clust_id = NULL, iv = TRUE, NAME = "IV")

  expect_true("Weak instruments" %in% colnames(result))
  expect_true("Wu-Hausman" %in% colnames(result))
  expect_match(result[["Weak instruments"]], "^[0-9]+(\\.[0-9]+)?\\*?$")
  expect_match(result[["Wu-Hausman"]], "^[0-9]+(\\.[0-9]+)?\\*?$")
})

test_that("GetTableEntry handles clustered single-coefficient models", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  fit <- lm(mpg ~ 1, data = mtcars)
  result <- GetTableEntry(
    fit,
    clust_id = "cyl",
    keepCoef1 = TRUE,
    NAME = "Intercept Only"
  )

  expect_true("(Intercept)" %in% colnames(result))
  expect_equal(result[["Adjusted R-squared"]], "0.00")
  expect_equal(result$Observations, "32")
})

test_that("GetTableEntry supports in-memory bootstrap standard errors", {
  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  result <- GetTableEntry(
    fit,
    clust_id = NULL,
    seType = "boot",
    inParens = "se",
    bootstrap_reps = 25,
    bootstrap_seed = 123
  )

  expect_s3_class(result, "data.frame")
  expect_true("wt" %in% colnames(result))
  expect_match(result$wt, "\\(")
})

test_that("GetTableEntry bootstrap results are reproducible with a fixed seed", {
  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  result_1 <- GetTableEntry(
    fit,
    clust_id = NULL,
    seType = "boot",
    bootstrap_reps = 20,
    bootstrap_seed = 99
  )
  result_2 <- GetTableEntry(
    fit,
    clust_id = NULL,
    seType = "boot",
    bootstrap_reps = 20,
    bootstrap_seed = 99
  )

  expect_identical(result_1, result_2)
})

test_that("GetTableEntry supports cluster bootstrap when clust_id is outside the formula", {
  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  result <- GetTableEntry(
    fit,
    clust_id = "cyl",
    seType = "boot",
    bootstrap_reps = 20,
    bootstrap_seed = 321
  )

  expect_s3_class(result, "data.frame")
  expect_true("wt" %in% colnames(result))
})

test_that("GetTableEntry supports bootstrap inference for GLM models", {
  data(mtcars)
  mtcars$am_binary <- mtcars$am
  fit <- glm(am_binary ~ wt + hp, data = mtcars, family = binomial)

  result <- GetTableEntry(
    fit,
    clust_id = NULL,
    seType = "boot",
    bootstrap_reps = 20,
    bootstrap_seed = 456
  )

  expect_s3_class(result, "data.frame")
  expect_true("AIC" %in% colnames(result))
})

test_that("GetTableEntry errors for unsupported bootstrap model classes", {
  fit <- stats::arima(AirPassengers, order = c(1, 0, 0))

  expect_error(
    GetTableEntry(fit, clust_id = NULL, seType = "boot", bootstrap_reps = 10),
    "supports only `lm` and `glm`"
  )
})

test_that("GetTableEntry rejects IV diagnostics with bootstrap inference", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  expect_error(
    GetTableEntry(
      fit,
      clust_id = NULL,
      seType = "boot",
      iv = TRUE,
      bootstrap_reps = 10
    ),
    "does not support IV diagnostics"
  )
})

test_that("GetTableEntry errors when the bootstrap cluster variable is missing", {
  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  expect_error(
    GetTableEntry(
      fit,
      clust_id = "missing_cluster",
      seType = "boot",
      bootstrap_reps = 10,
      bootstrap_seed = 1
    ),
    "Cluster variable"
  )
})

test_that("GetTableEntry supports file-based bootstrap replications", {
  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  bootstrap_dir <- tempfile("bootstrap-files-")
  write_bootstrap_files(
    data = mtcars[, c("mpg", "wt", "hp")],
    directory = bootstrap_dir,
    prefix = "mtcars_boot",
    reps = 8
  )

  result <- GetTableEntry(
    fit,
    clust_id = NULL,
    seType = "boot",
    bootstrap_source = "files",
    bootstrap_dir = bootstrap_dir,
    bootstrap_prefix = "mtcars_boot",
    bootstrap_reps = 8
  )

  expect_s3_class(result, "data.frame")
  expect_true("hp" %in% colnames(result))
})

test_that("bootstrap argument normalization validates inputs and legacy aliases", {
  bootstrap_dir <- tempfile("bootstrap-args-")
  dir.create(bootstrap_dir)

  expect_error(
    helpeRs:::helpeRsNormalizeBootstrapArgs(bootstrap_reps = 0),
    "positive integer"
  )
  expect_error(
    helpeRs:::helpeRsNormalizeBootstrapArgs(
      bootstrap_source = "files",
      bootstrap_dir = tempfile("missing-dir-"),
      bootstrap_prefix = "boot"
    ),
    "`bootstrap_dir`"
  )
  expect_error(
    helpeRs:::helpeRsNormalizeBootstrapArgs(
      bootstrap_source = "files",
      bootstrap_dir = bootstrap_dir,
      bootstrap_prefix = ""
    ),
    "`bootstrap_prefix`"
  )

  expect_warning(
    legacy_args <- helpeRs:::helpeRsNormalizeBootstrapArgs(
      bootDataLocation = bootstrap_dir,
      bootDataNameTag = "legacy_boot"
    ),
    "deprecated"
  )
  expect_equal(legacy_args$source, "files")
  expect_equal(legacy_args$dir, bootstrap_dir)
  expect_equal(legacy_args$prefix, "legacy_boot")

  expect_warning(
    processing_args <- helpeRs:::helpeRsNormalizeBootstrapArgs(
      bootFactorVars = "gear",
      bootExcludeCovars = "hp"
    ),
    "deprecated and ignored"
  )
  expect_equal(processing_args$source, "memory")
})

test_that("bootstrap file discovery validates and orders replicate files", {
  bootstrap_dir <- tempfile("bootstrap-fileset-")
  dir.create(bootstrap_dir)

  expect_error(
    helpeRs:::helpeRsBootstrapFileSet(bootstrap_dir, "mtcars_boot", 5),
    "No bootstrap replicate files"
  )

  utils::write.csv(
    mtcars[1:2, c("mpg", "wt", "hp")],
    file.path(bootstrap_dir, "mtcars_boot_0.csv"),
    row.names = FALSE
  )
  expect_error(
    helpeRs:::helpeRsBootstrapFileSet(bootstrap_dir, "mtcars_boot", 5),
    "were not found"
  )

  utils::write.csv(mtcars[1:2, c("mpg", "wt", "hp")],
                   file.path(bootstrap_dir, "mtcars_boot_3.csv"),
                   row.names = FALSE)
  utils::write.csv(mtcars[3:4, c("mpg", "wt", "hp")],
                   file.path(bootstrap_dir, "mtcars_boot_1.csv"),
                   row.names = FALSE)
  utils::write.csv(mtcars[5:6, c("mpg", "wt", "hp")],
                   file.path(bootstrap_dir, "mtcars_boot_2.csv"),
                   row.names = FALSE)

  file_set <- helpeRs:::helpeRsBootstrapFileSet(
    bootstrap_dir,
    "mtcars_boot",
    bootstrap_reps = 2
  )

  expect_equal(basename(file_set$validation_file), "mtcars_boot_0.csv")
  expect_equal(
    basename(file_set$replicate_files),
    c("mtcars_boot_1.csv", "mtcars_boot_2.csv")
  )
})

test_that("bootstrap data alignment preserves template column types", {
  template <- data.frame(
    fac = factor(c("low", "high"), levels = c("low", "high"), ordered = TRUE),
    flag = c(TRUE, FALSE),
    count = 1:2,
    value = c(1.5, 2.5),
    label = c("a", "b"),
    stringsAsFactors = FALSE
  )
  incoming <- data.frame(
    X = c("row1", "row2"),
    fac = c("high", "low"),
    flag = c("TRUE", "FALSE"),
    count = c("3", "4"),
    value = c("5.5", "6.5"),
    label = c(1, 2),
    stringsAsFactors = FALSE
  )

  aligned <- helpeRs:::helpeRsAlignBootstrapData(incoming, template)

  expect_false("X" %in% names(aligned))
  expect_s3_class(aligned$fac, "ordered")
  expect_equal(levels(aligned$fac), c("low", "high"))
  expect_type(aligned$flag, "logical")
  expect_type(aligned$count, "integer")
  expect_type(aligned$value, "double")
  expect_type(aligned$label, "character")
  expect_error(
    helpeRs:::helpeRsAlignBootstrapData(data.frame(fac = "low"), template),
    "missing required columns"
  )
  expect_identical(
    helpeRs:::helpeRsMaybeStripRowNamesColumn(data.frame(), names(template)),
    data.frame()
  )
})

test_that("bootstrap helpers report unalignable data and invalid coefficients", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  no_data_fit <- lm(mtcars$mpg ~ mtcars$wt)
  expect_null(helpeRs:::helpeRsGetOriginalData(no_data_fit))

  missing_data_fit <- fit
  missing_data_fit$call$data <- quote(missing_mtcars_data)
  expect_null(helpeRs:::helpeRsGetOriginalData(missing_data_fit))

  unaligned_fit <- fit
  row.names(unaligned_fit$model) <- paste0("row", seq_len(nrow(unaligned_fit$model)))
  expect_error(
    helpeRs:::helpeRsBootstrapTemplateData(unaligned_fit, clust_id = "cyl"),
    "Unable to align"
  )

  expect_error(
    helpeRs:::helpeRsReferenceCoefficients(
      structure(list(coefficients = c(1, 2)), class = "lm")
    ),
    "named coefficients"
  )
  expect_error(
    helpeRs:::helpeRsBootstrapSample(model.frame(fit), clust_id = "missing_cluster"),
    "not available"
  )
})

test_that("bootstrap refitting handles weights, offsets, and failed file replicates", {
  dat <- data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    x = c(0, 1, 2, 3, 4, 5),
    w = c(1, 2, 1, 2, 1, 2),
    off = rep(0.25, 6)
  )
  fit_weighted <- lm(y ~ x, data = dat, weights = w, offset = off)
  template_weighted <- model.frame(fit_weighted)

  refit <- helpeRs:::helpeRsRefitBootstrapModel(
    fit_weighted,
    template_weighted,
    template_weighted
  )
  expect_s3_class(refit, "lm")

  fit <- lm(mpg ~ wt + hp, data = mtcars)
  bootstrap_dir <- tempfile("bad-bootstrap-files-")
  dir.create(bootstrap_dir)
  bad_rep <- mtcars[, c("mpg", "wt")]
  utils::write.csv(bad_rep, file.path(bootstrap_dir, "bad_boot_1.csv"), row.names = FALSE)
  utils::write.csv(bad_rep, file.path(bootstrap_dir, "bad_boot_2.csv"), row.names = FALSE)

  expect_error(
    helpeRs:::helpeRsBootstrapSummary(
      fit,
      clust_id = NULL,
      bootstrap_args = list(
        source = "files",
        reps = 2L,
        dir = bootstrap_dir,
        prefix = "bad_boot"
      )
    ),
    "at least two valid replicate estimates"
  )
})

test_that("bootstrap memory seed cleanup restores the prior random state", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  template <- model.frame(fit)
  coef_names <- names(coef(fit))[-1]

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    rm(".Random.seed", envir = .GlobalEnv)
  }
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  coef_mat <- helpeRs:::helpeRsBootstrapReplicatesMemory(
    fit,
    template_data = template,
    clust_id = NULL,
    coef_names = coef_names,
    reps = 2L,
    seed = 42
  )

  expect_equal(dim(coef_mat), c(2L, length(coef_names)))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

# FullTransformer tests

test_that("FullTransformer preserves single non-stat rows and applies column names", {
  t_raw <- matrix(
    c("0.5 (2.1)*", "0.95", "100",
      "0.6 (1.8)*", "0.96", "100"),
    ncol = 2, byrow = FALSE
  )
  row.names(t_raw) <- c("wt", "Adjusted R-squared", "Observations")
  result <- FullTransformer(t_raw, COLNAMES_VEC = c("Model 1", "Model 2"))

  expect_equal(row.names(result), c("wt", "Adjusted R-squared", "Observations"))
  expect_equal(colnames(result), c("Model 1", "Model 2"))
})

test_that("FullTransformer applies name_conversion_matrix", {
  t_raw <- matrix(
    c("0.5 (2.1)*", "0.95", "100",
      "0.6 (1.8)*", "0.96", "100"),
    ncol = 2, byrow = FALSE
  )
  row.names(t_raw) <- c("gdpPerCapita", "Adjusted R-squared", "Observations")
  name_mat <- matrix(c("gdpPerCapita", "GDP per Capita"), ncol = 2)

  result <- FullTransformer(
    t_raw,
    COLNAMES_VEC = c("Model 1", "Model 2"),
    name_conversion_matrix = name_mat
  )

  expect_equal(row.names(result), c("GDP per Capita", "Adjusted R-squared", "Observations"))
})

test_that("FullTransformer moves statistics to bottom", {
  t_raw <- matrix(
    c("0.5*", "100", "0.95", "0.3*",
      "0.6*", "100", "0.96", "0.4*"),
    ncol = 2, byrow = FALSE
  )
  row.names(t_raw) <- c("wt", "Observations", "Adjusted R-squared", "hp")
  colnames(t_raw) <- c("Model1", "Model2")

  result <- FullTransformer(t_raw, COLNAMES_VEC = c("Model 1", "Model 2"))

  expect_equal(
    row.names(result),
    c("wt", "hp", "Observations", "Adjusted R-squared")
  )
})

test_that("FullTransformer handles camelCase splitting", {
  t_raw <- matrix(
    c("0.5*", "0.7*", "0.95", "100",
      "0.6*", "0.8*", "0.96", "100"),
    ncol = 2, byrow = FALSE
  )
  row.names(t_raw) <- c("someVariableName", "anotherVariableName", "Adjusted R-squared", "Observations")
  colnames(t_raw) <- c("Model1", "Model2")

  result <- FullTransformer(t_raw, COLNAMES_VEC = c("Model 1", "Model 2"))

  expect_equal(
    row.names(result)[1:2],
    c("some Variable Name", "another Variable Name")
  )
})

test_that("FullTransformer preserves GDP acronym", {
  t_raw <- matrix(
    c("0.5*", "0.95", "100",
      "0.6*", "0.96", "100"),
    ncol = 2, byrow = FALSE
  )
  row.names(t_raw) <- c("GDPPerCapita", "Adjusted R-squared", "Observations")
  colnames(t_raw) <- c("Model1", "Model2")

  result <- FullTransformer(t_raw, COLNAMES_VEC = c("Model 1", "Model 2"))

  expect_equal(row.names(result)[1], "GDP Per Capita")
})

test_that("FullTransformer handles as.factor() variable names", {
  t_raw <- matrix(
    c("0.5*", "0.95", "100",
      "0.6*", "0.96", "100"),
    ncol = 2, byrow = FALSE
  )
  row.names(t_raw) <- c("as.factor(cyl)6", "Adjusted R-squared", "Observations")
  colnames(t_raw) <- c("Model1", "Model2")

  result <- FullTransformer(t_raw, COLNAMES_VEC = c("Model 1", "Model 2"))

  expect_equal(row.names(result)[1], "Cyl - 6")
})

# Stargazer2FullTable tests

test_that("Stargazer2FullTable converts to longtable environment", {
  # Simulate stargazer output
  stargazer_text <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\begin{tabular}{@{\\extracolsep{5pt}} lcc}",
    "\\hline",
    "Variable & Model 1 & Model 2 \\\\",
    "\\hline",
    "wt & 0.5* & 0.6* \\\\",
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  result <- Stargazer2FullTable(stargazer_text)

  expect_true(any(grepl("longtable", result)))
})

test_that("Stargazer2FullTable removes [!htbp] from table", {
  stargazer_text <- c(
    "\\begin{table}[!htbp]",
    "\\end{table}"
  )

  result <- Stargazer2FullTable(stargazer_text)

  expect_false(any(grepl("\\[!htbp\\]", result)))
})

test_that("Stargazer2FullTable removes \\centering", {
  stargazer_text <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\end{table}"
  )

  result <- Stargazer2FullTable(stargazer_text)

  expect_false(any(grepl("\\\\centering", result)))
})

test_that("Stargazer2FullTable wraps in fontsize environment", {
  stargazer_text <- c(
    "\\begin{table}[!htbp]",
    "\\begin{tabular}{@{\\extracolsep{5pt}} lc}",
    "content",
    "\\end{tabular}",
    "\\end{table}"
  )

  result <- Stargazer2FullTable(stargazer_text, fontsize = "scriptsize")

  expect_true(any(grepl("scriptsize", result)))
})

test_that("Stargazer2FullTable removes tabular begin tag", {
  stargazer_text <- c(
    "\\begin{table}[!htbp]",
    "\\begin{tabular}{@{\\extracolsep{5pt}} lc}",
    "\\end{tabular}",
    "\\end{table}"
  )

  result <- Stargazer2FullTable(stargazer_text)

  expect_false(any(grepl("\\\\begin\\{tabular\\}", result)))
})

test_that("Stargazer2FullTable removes tabular end tag", {
  stargazer_text <- c(
    "\\begin{table}[!htbp]",
    "\\begin{tabular}{@{\\extracolsep{5pt}} lc}",
    "\\end{tabular}",
    "\\end{table}"
  )

  result <- Stargazer2FullTable(stargazer_text)

  expect_false(any(grepl("\\\\end\\{tabular\\}", result)))
})

test_that("Stargazer2FullTable adds continued headers for long tables", {
  stargazer_text <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\begin{tabular}{@{\\extracolsep{5pt}} lcc}",
    "\\hline",
    "Variable & Model 1 & Model 2 \\\\",
    "\\hline",
    rep("row & 1 & 2 \\\\", 45),
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  result <- Stargazer2FullTable(stargazer_text)

  expect_true(any(grepl("\\\\endfirsthead", result)))
  expect_true(any(grepl("\\\\endhead", result)))
  expect_true(any(grepl("Continued from previous page", result, fixed = TRUE)))
  expect_true(any(grepl("\\\\multicolumn\\{3\\}\\{c\\}", result)))
})

# Tables2Tex integration tests

test_that("Tables2Tex creates output files", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit1 <- lm(mpg ~ wt, data = mtcars)
  fit2 <- lm(mpg ~ wt + hp, data = mtcars)

  temp_dir <- tempdir()

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit1, fit2),
      clust_id = NULL,
      saveFolder = temp_dir,
      nameTag = "TestTable",
      tabCaption = "Test Caption",
      saveFull = TRUE
    )
  )

  # Check files were created
  main_file <- file.path(temp_dir, "tabTestTable_SEanalytical.tex")
  full_file <- file.path(temp_dir, "FULL_tabTestTable_SEanalytical.tex")

  expect_true(file.exists(main_file))
  expect_true(file.exists(full_file))
  expect_true(any(grepl("longtable", readLines(full_file, warn = FALSE), fixed = TRUE)))

  # Clean up
  unlink(main_file)
  unlink(full_file)
})

test_that("Tables2Tex handles custom model names", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit1 <- lm(mpg ~ wt, data = mtcars)
  fit2 <- lm(mpg ~ wt + hp, data = mtcars)

  temp_dir <- tempdir()

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit1, fit2),
      clust_id = NULL,
      saveFolder = temp_dir,
      nameTag = "CustomNames",
      model.names = c("Base Model", "Full Model"),
      saveFull = FALSE
    )
  )

  # Check file was created
  main_file <- file.path(temp_dir, "tabCustomNames_SEanalytical.tex")
  expect_true(file.exists(main_file))

  # Read content and check model names are included
  content <- paste(readLines(main_file, warn = FALSE), collapse = "\n")
  expect_match(content, "Base Model", fixed = TRUE)
  expect_match(content, "Full Model", fixed = TRUE)

  # Clean up
  unlink(main_file)
})

test_that("Tables2Tex handles NameConversionMat", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  temp_dir <- tempdir()

  name_mat <- matrix(
    c("wt", "Weight (1000 lbs)",
      "hp", "Horsepower",
      "Observations", "Observations",
      "Adjusted R-squared", "Adjusted R-squared"),
    ncol = 2, byrow = TRUE
  )

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit),
      clust_id = NULL,
      saveFolder = temp_dir,
      nameTag = "NameConvert",
      NameConversionMat = name_mat,
      saveFull = FALSE
    )
  )

  content <- paste(
    readLines(file.path(temp_dir, "tabNameConvert_SEanalytical.tex"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(content, "Weight (1000 lbs)", fixed = TRUE)
  expect_match(content, "Horsepower", fixed = TRUE)

  # Clean up
  unlink(file.path(temp_dir, "tabNameConvert_SEanalytical.tex"))
})

test_that("Tables2Tex handles saveFull = FALSE", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit <- lm(mpg ~ wt, data = mtcars)

  temp_dir <- tempdir()

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit),
      clust_id = NULL,
      saveFolder = temp_dir,
      nameTag = "NoFull",
      saveFull = FALSE
    )
  )

  # Main file should exist
  main_file <- file.path(temp_dir, "tabNoFull_SEanalytical.tex")
  expect_true(file.exists(main_file))

  # Full file should NOT exist
  full_file <- file.path(temp_dir, "FULL_tabNoFull_SEanalytical.tex")
  expect_false(file.exists(full_file))

  # Clean up
  unlink(main_file)
})

test_that("Tables2Tex handles checkmark_list", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit1 <- lm(mpg ~ wt, data = mtcars)
  fit2 <- lm(mpg ~ wt + hp, data = mtcars)

  temp_dir <- tempdir()

  checkmarks <- list(
    "Year FE" = c(1, 1),
    "State FE" = c(0, 1)
  )

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit1, fit2),
      clust_id = NULL,
      saveFolder = temp_dir,
      nameTag = "Checkmarks",
      checkmark_list = checkmarks,
      saveFull = FALSE
    )
  )

  content <- paste(
    readLines(file.path(temp_dir, "tabCheckmarks_SEanalytical.tex"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(content, "Year.FE", fixed = TRUE)
  expect_match(content, "State.FE", fixed = TRUE)
  expect_match(content, "\\checkmark", fixed = TRUE)

  # Clean up
  unlink(file.path(temp_dir, "tabCheckmarks_SEanalytical.tex"))
})

test_that("Tables2Tex handles addrow_list", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  fit1 <- lm(mpg ~ wt, data = mtcars)
  fit2 <- lm(mpg ~ wt + hp, data = mtcars)
  temp_dir <- tempdir()

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit1, fit2),
      clust_id = NULL,
      saveFolder = temp_dir,
      nameTag = "AddRows",
      addrow_list = list("Sample" = c("All", "Restricted")),
      saveFull = FALSE
    )
  )

  content <- paste(
    readLines(file.path(temp_dir, "tabAddRows_SEanalytical.tex"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(content, "Sample", fixed = TRUE)
  expect_match(content, "All", fixed = TRUE)
  expect_match(content, "Restricted", fixed = TRUE)

  unlink(file.path(temp_dir, "tabAddRows_SEanalytical.tex"))
})

test_that("Tables2Tex supports character reg_list inputs", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  fit1 <- lm(mpg ~ wt, data = mtcars)
  fit2 <- lm(mpg ~ wt + hp, data = mtcars)
  object_dir <- tempfile("tables2tex-objects-")
  char_dir <- tempfile("tables2tex-characters-")
  dir.create(object_dir)
  dir.create(char_dir)
  assign("fit1", fit1, envir = .GlobalEnv)
  assign("fit2", fit2, envir = .GlobalEnv)
  on.exit({
    rm("fit1", envir = .GlobalEnv)
    rm("fit2", envir = .GlobalEnv)
  }, add = TRUE)

  Tables2Tex(
    reg_list = list(fit1, fit2),
    clust_id = NULL,
    saveFolder = object_dir,
    nameTag = "Same",
    saveFull = FALSE
  )
  Tables2Tex(
    reg_list = c("fit1", "fit2"),
    clust_id = NULL,
    saveFolder = char_dir,
    nameTag = "Same",
    saveFull = FALSE
  )

  expect_identical(
    read_tex_body(file.path(object_dir, "tabSame_SEanalytical.tex")),
    read_tex_body(file.path(char_dir, "tabSame_SEanalytical.tex"))
  )

  unlink(file.path(object_dir, "tabSame_SEanalytical.tex"))
  unlink(file.path(char_dir, "tabSame_SEanalytical.tex"))
})

test_that("Tables2Tex handles single model", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  temp_dir <- tempdir()

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit),
      clust_id = NULL,
      saveFolder = temp_dir,
      nameTag = "SingleModel",
      saveFull = FALSE
    )
  )

  # Clean up
  unlink(file.path(temp_dir, "tabSingleModel_SEanalytical.tex"))
})

test_that("Tables2Tex works with clustered standard errors", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  temp_dir <- tempdir()

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit),
      clust_id = "cyl",
      saveFolder = temp_dir,
      nameTag = "Clustered",
      saveFull = FALSE
    )
  )

  # Clean up
  unlink(file.path(temp_dir, "tabClustered_SEanalytical.tex"))
})

test_that("Tables2Tex handles font.size parameter", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit <- lm(mpg ~ wt, data = mtcars)

  temp_dir <- tempdir()

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit),
      clust_id = NULL,
      saveFolder = temp_dir,
      nameTag = "FontSize",
      font.size = "scriptsize",
      saveFull = FALSE
    )
  )

  expect_match(
    paste(readLines(file.path(temp_dir, "tabFontSize_SEanalytical.tex"), warn = FALSE), collapse = "\n"),
    "\\scriptsize",
    fixed = TRUE
  )

  # Clean up
  unlink(file.path(temp_dir, "tabFontSize_SEanalytical.tex"))
})

test_that("Tables2Tex works with in-memory bootstrap inference", {
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit1 <- lm(mpg ~ wt, data = mtcars)
  fit2 <- lm(mpg ~ wt + hp, data = mtcars)

  temp_dir <- tempdir()

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit1, fit2),
      clust_id = NULL,
      seType = "boot",
      bootstrap_reps = 20,
      bootstrap_seed = 42,
      saveFolder = temp_dir,
      nameTag = "BootstrapMemory",
      saveFull = FALSE
    )
  )

  expect_true(file.exists(file.path(temp_dir, "tabBootstrapMemory_SEboot.tex")))
  unlink(file.path(temp_dir, "tabBootstrapMemory_SEboot.tex"))
})

test_that("Tables2Tex works with file-based bootstrap inference", {
  skip_if_not_installed("stargazer")
  skip_if_not_installed("plyr")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  temp_dir <- tempdir()
  bootstrap_dir <- tempfile("tables2tex-bootstrap-")
  write_bootstrap_files(
    data = mtcars[, c("mpg", "wt", "hp")],
    directory = bootstrap_dir,
    prefix = "table_boot",
    reps = 8
  )

  expect_no_error(
    Tables2Tex(
      reg_list = list(fit),
      clust_id = NULL,
      seType = "boot",
      bootstrap_source = "files",
      bootstrap_dir = bootstrap_dir,
      bootstrap_prefix = "table_boot",
      bootstrap_reps = 8,
      saveFolder = temp_dir,
      nameTag = "BootstrapFiles",
      saveFull = FALSE
    )
  )

  expect_true(file.exists(file.path(temp_dir, "tabBootstrapFiles_SEboot.tex")))
  unlink(file.path(temp_dir, "tabBootstrapFiles_SEboot.tex"))
})
