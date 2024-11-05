
analysisOptionsFromJaspDescriptives <- function(functionName) {

  filename <- switch(
    functionName,
    "DescriptivesTimeSeries" = "DescriptivesTimeSeriesForm.qml"

  )

  path <- system.file("qml", "common", filename, package = "jaspDescriptives")

  if (!file.exists(path))
    stop("Could not find the QML file for \"", functionName, "\" at path:", path)

  return(jaspTools:::readQML(path))
}
