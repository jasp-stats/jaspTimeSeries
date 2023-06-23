context("ARIMA Time Series")

# test with covariate
options <- jaspTools::analysisOptions("ARIMATimeSeries")
options$dependent <- "visits"
options$time <- "date"
options$covariates <- "actions"
options$seasonal <- TRUE
options$residualTimeSeries <- TRUE
options$residualQQ <- TRUE
options$residualLjungBox <- TRUE
options$periodSpecification <- "dominant"
options$forecastSave <- ""
set.seed(1)
results <- jaspTools::runAnalysis("ARIMATimeSeries", "JASP Webpage Visits.csv", options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["coefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 0.054896748258671, "MA(1)", -0.384412851213784, -0.492491170956408,
                                      1.98230321046822e-11, -7.00247033581021, -0.276334531471161,
                                      "FALSE", 0.0559876165193427, "MA(2)", -0.370081542800987, -0.480307516319357,
                                      2.03275840604533e-10, -6.61006068499326, -0.259855569282616,
                                      "TRUE", 0.0615596106422369, "seasonal AR(1)", 0.143927082313645,
                                      0.0227312101852408, 0.0201143892489153, 2.33801157629309, 0.26512295444205,
                                      "FALSE", 0.0694953442661468, "seasonal AR(2)", -0.170651287605148,
                                      -0.307470685410445, 0.014694019427854, -2.45557870684982, -0.0338318897998512,
                                      "TRUE", 0.00410873623069308, "actions", 0.331704239051192, 0.323615138655181,
                                      0, 80.7314513336961, 0.339793339447204))
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["modelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3278.28528217595, 3278.59754983394, 3300.00768737026, -1633.14264108798,
                                      8187.159216112))
})

test_that("Ljung-Box Plot matches", {
  plotName <- results[["results"]][["residContainer"]][["collection"]][["residContainer_ljungPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "ljung-box-plot")
})

test_that("Q-Q Plot matches", {
  plotName <- results[["results"]][["residContainer"]][["collection"]][["residContainer_residualQQPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "q-q-plot")
})

test_that("Time Series Plot matches", {
  plotName <- results[["results"]][["residContainer"]][["collection"]][["residContainer_residualTimeSeriesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "time-series-plot")
})

test_that("Time Series Plot matches", {
  plotName <- results[["results"]][["timeSeriesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "time-series-plot2")
})

# test with forecasting
options <- jaspTools::analysisOptions("ARIMATimeSeries")
options$dependent <- "visits"
options$seasonal <- TRUE
options$m <- 7
options$p <- 2
options$d <- 1
options$q <- 1
options$P <- 1
options$forecastLength <- 20
options$forecastTimeSeries <- TRUE
options$forecastTable <- TRUE
options$modelSpecification <- "custom"
options$forecastSave <- ""
set.seed(1)
results <- jaspTools::runAnalysis("ARIMATimeSeries", "JASP Webpage Visits.csv", options)


test_that("Coefficients table results match", {
  table <- results[["results"]][["coefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 3.39005881784864, "Drift", 7.68963795029409, 1.01543824842242,
                                      0.0240985149162039, 2.26829042310659, 14.3638376521658, "TRUE",
                                      0.062733927891018, "AR(1)", 0.533767245984023, 0.410259429357649,
                                      1.33226762955019e-15, 8.5084301896621, 0.657275062610397, "FALSE",
                                      0.0635407309683744, "AR(2)", 0.137979761572092, 0.0128835462931604,
                                      0.0307596921342448, 2.17151674948101, 0.263075976851023, "TRUE",
                                      0.00900225186425044, "MA(1)", -0.999999101143194, -1.01772234133956,
                                      0, -111.083217424117, -0.982275860946824, "TRUE", 0.0290590940028099,
                                      "seasonal AR(1)", 0.901918333537911, 0.844708058745896, 0, 31.0373865561913,
                                      0.959128608329927))
})

test_that("Forecast Time Series Plot matches", {
  plotName <- results[["results"]][["forecastPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forecast-time-series-plot")
})

test_that("Forecasts table results match", {
  table <- results[["results"]][["forecastTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2623.5974678915, 2450.52090896614, 278, 3277.49627292311, 3450.57283184846,
                                      2950.5468704073, 1590.67611261519, 1394.28945479282, 279, 2332.64256764266,
                                      2529.02922546504, 1961.65934012893, 1586.29133445426, 1376.48389475706,
                                      280, 2378.96271038512, 2588.77015008232, 1982.62702241969, 3643.47082954636,
                                      3427.16286366333, 281, 4460.7017780235, 4677.00974390653, 4052.08630378493,
                                      3982.06994392108, 3762.32859986804, 282, 4812.27250390087, 5032.01384795391,
                                      4397.17122391098, 3047.33661597295, 2825.77744873908, 283, 3884.40707541346,
                                      4105.96624264734, 3465.87184569321, 3181.5259705088, 2958.98700219402,
                                      284, 4022.29820655006, 4244.83717486484, 3601.91208852943, 2485.61875552301,
                                      2204.61589281667, 285, 3547.27284244867, 3828.27570515501, 3016.44579898584,
                                      1560.5507509318, 1263.84232806521, 286, 2681.54185660168, 2978.25027946827,
                                      2121.04630376674, 1559.36259584244, 1253.24747302908, 287, 2715.89305989036,
                                      3022.00818270372, 2137.6278278664, 3415.40054180663, 3104.53911489116,
                                      288, 4589.86296960489, 4900.72439652035, 4002.63175570576, 3720.92398174579,
                                      3407.49433287197, 289, 4905.08938307234, 5218.51903194616, 4313.00668240907,
                                      2877.91746632314, 2563.09394720073, 290, 4067.34903480477, 4382.17255392718,
                                      3472.63325056395, 2999.06265821102, 2683.46752946392, 291, 4191.40943724841,
                                      4507.00456599551, 3595.23604772971, 2401.04076302184, 2048.32703417078,
                                      292, 3733.62495146425, 4086.33868031531, 3067.33285724304, 1573.23922868509,
                                      1209.65579007447, 293, 2946.89015815207, 3310.47359676269, 2260.06469341858,
                                      1576.03541699628, 1205.78681692371, 294, 2974.86792588902, 3345.1165259616,
                                      2275.45167144265, 3252.13806259347, 2878.44894915304, 295, 4663.96914040798,
                                      5037.6582538484, 3958.05360150072, 3529.0688874877, 3153.48088131973,
                                      296, 4948.07415280129, 5323.66215896926, 4238.5715201445, 2769.76213499614,
                                      2393.12054732769, 297, 4192.74792597099, 4569.38951363944, 3481.25503048356
                                 ))
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["modelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3860.19374839746, 3860.50601605545, 3881.91615359176, -1924.09687419873,
                                      64928.1574797458))
})

test_that("Time Series Plot matches", {
  plotName <- results[["results"]][["timeSeriesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "time-series-plot3")
})