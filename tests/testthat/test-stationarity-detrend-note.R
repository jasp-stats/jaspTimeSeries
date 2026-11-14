.stationarityNoteRun <- function(y, ...) {
  options <- jaspTools::analysisOptions("StationarityTimeSeries")
  options$dependent <- "y"
  options$dependent.types <- "scale"
  options$adfTest <- TRUE
  options$kpssLevel <- TRUE
  options$timeSeriesPlot <- FALSE
  options <- utils::modifyList(options, list(...))
  result <- jaspTools::runAnalysis("StationarityTimeSeries", data.frame(y = y), options, view = FALSE, quiet = TRUE)
  expect_identical(result$status, "complete")
  table <- result$results$stationaryTable
  expect_identical(table$status, "complete")
  vapply(table$footnotes, function(note) note$text, character(1L))
}

.stationarityNote <- "These p-values assume at most a linear trend."

test_that("stationarity p-values are flagged after polynomial detrending of degree 2 or higher", {
  set.seed(20261112)
  y <- cumsum(rnorm(120))

  expect_false(any(grepl(.stationarityNote, .stationarityNoteRun(y, detrend = FALSE), fixed = TRUE)))
  expect_false(any(grepl(.stationarityNote, .stationarityNoteRun(y, detrend = TRUE,
    polynomialSpecification = "custom", detrendPoly = 1L), fixed = TRUE)))

  notes <- .stationarityNoteRun(y, detrend = TRUE, polynomialSpecification = "custom", detrendPoly = 3L)
  expect_true(any(grepl("After removing a polynomial trend of degree 3 they are not valid.", notes, fixed = TRUE)))

  # Automatic selection reports the degree it chose.
  t <- seq_along(y)
  aic <- vapply(0:5, function(d) AIC(if (d == 0) lm(y ~ 1) else lm(y ~ poly(t, d))), numeric(1L))
  selected <- which.min(aic) - 1L
  notes <- .stationarityNoteRun(y, detrend = TRUE, polynomialSpecification = "auto",
                                polynomialSpecificationAutoMax = 5L, polynomialSpecificationAutoIc = "aic")
  expect_identical(any(grepl(.stationarityNote, notes, fixed = TRUE)), selected >= 2L)
  if (selected >= 2L)
    expect_true(any(grepl(paste0("degree ", selected, " they are not valid"), notes, fixed = TRUE)))
})
