test_that("BSTS coefficient estimates use JASP's standard numeric formatting", {
  options <- list(modelTerms = list(list(components = "predictor")),
                  posteriorSummaryTable = TRUE, showCoefMeanInc = TRUE,
                  posteriorSummaryCiLevel = 0.95)
  for (suffix in c("CoefficientTable", "ConditionalCoefficientTable")) {
    builder <- getFromNamespace(paste0(".bstsCreate", suffix), "jaspTimeSeries")
    scope <- new.env(parent = environment(builder))
    # Inspect declarations without fitting or constructing native result objects.
    table <- new.env(parent = emptyenv())
    table$columns <- list()
    table$dependOn <- function(...) NULL
    table$addFootnote <- function(...) NULL
    table$addColumnInfo <- function(name, ...) table$columns[[name]] <- list(...)
    scope$createJaspTable <- function(...) table
    scope[[paste0(".bstsFill", suffix)]] <- function(...) NULL
    environment(builder) <- scope
    result <- list(bstsMainContainer = new.env(parent = emptyenv()))
    builder(result, options, TRUE)
    for (column in c("mean", "sd", "lowerCri", "upperCri", "postIncP")) {
      expect_identical(table$columns[[column]]$type, "number")
      expect_null(table$columns[[column]]$format)
    }
    expect_false(any(c("meanInc", "sdInc") %in% names(table$columns)))
    if (suffix == "ConditionalCoefficientTable")
      expect_identical(table$columns$includedDraws$type, "integer")
  }
})
