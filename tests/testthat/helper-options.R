# Native QML option parsing must find the shared Descriptives form, just as
# JASP does. R installs its qmldir beside the package's qml directory.
# Register it before the bridge creates its QML engine, preserving other imports.
local({
  dependencyLibrary <- dirname(find.package("jaspDescriptives"))
  for (name in c("QML_IMPORT_PATH", "QML2_IMPORT_PATH")) {
    paths <- strsplit(Sys.getenv(name), .Platform$path.sep, fixed = TRUE)[[1L]]
    paths <- unique(c(dependencyLibrary, paths[nzchar(paths)]))
    do.call(Sys.setenv, setNames(list(paste(paths, collapse = .Platform$path.sep)), name))
  }
})
