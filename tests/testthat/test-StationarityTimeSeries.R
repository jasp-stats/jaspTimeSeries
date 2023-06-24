context("Stationarity Time Series")

options <- jaspTools::analysisOptions("StationarityTimeSeries")
options$dependent <- "visits"
options$time <- "date"
options$adfTest <- TRUE
options$ppTestRegressionCoefficient <- TRUE
options$ppTestStudentized <- TRUE
options$kpssLevel <- TRUE
options$kpssTrend <- TRUE
options$detrend <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("StationarityTimeSeries", "JASP Webpage Visits.csv", options)


test_that("Detrend using linear regression table results match", {
  table <- results[["results"]][["polyTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4444.89603738658, 4452.14407239895, 0, 4405.95422965815, 4416.82628217671,
                                      1, 4406.10303631081, 4420.59910633556, 2, 4293.3838315003, 4311.50391903124,
                                      3, 4286.71235708712, 4308.45646212425, 4, 4262.85663398085,
                                      4288.22475652416, 5))
})

test_that("Stationarity Tests table results match", {
  table <- results[["results"]][["stationaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6, "Non-stationary", 0.361526409282873, -2.50921476228524, "Augmented Dickey-Fuller t",
                                      0, 5, "Non-stationary", 0.01, -79.3680079369253, "Phillips-Perron regression coefficient <unicode>",
                                      0, 5, "Non-stationary", 0.01, -8.2850708060335, "Phillips-Perron studentized <unicode>",
                                      1, 5, "Level stationary", 0.1, 0.049596225480524, "Kwiatkowski-Phillips-Schmidt-Shin Level <unicode>",
                                      1, 5, "Trend stationary", 0.1, 0.049596225480524, "Kwiatkowski-Phillips-Schmidt-Shin Trend <unicode>"
                                 ))
})

test_that("Time Series Plot matches", {
  plotName <- results[["results"]][["timeSeriesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "time-series-plot-stat")
})