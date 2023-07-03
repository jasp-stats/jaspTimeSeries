context("Spectral Time Series")

options <- jaspTools::analysisOptions("SpectralTimeSeries")
options$dependent <- "visits"
options$kernel <- TRUE
options$taper <- 0.2
options$pinkNoise <- TRUE
options$kernelTerm <- list(list(kernelDimension = 2))
set.seed(1)
results <- jaspTools::runAnalysis("SpectralTimeSeries", "JASP Webpage Visits.csv", options)


test_that("Spectral Density table results match", {
  table <- results[["results"]][["bandWidthTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0050117210867155))
})

test_that("Power Spectral Density Plot matches", {
  plotName <- results[["results"]][["powerSpectralDensity"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "power-spectral-density-plot")
})