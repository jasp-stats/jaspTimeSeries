context("Example: JASP Webpage Visits")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("StationarityTimeSeries results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "JASP Webpage Visits.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("StationarityTimeSeries", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["acfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_autocorrelation-function")

  plotName <- results[["results"]][["timeSeriesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_time-series-plot")

})

