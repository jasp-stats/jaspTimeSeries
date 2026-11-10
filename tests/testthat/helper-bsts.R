# jaspTools::runAnalysis leaves its quiet-mode sink tempfile behind. BSTS
# reseeds the native RNG used for temporary names, so repeated runs can exhaust
# the available names. Track only this runner's direct tempfile calls and clean
# them up after its exit handlers finish, without changing jaspTools itself.
# Remove this workaround when the upstream runner cleans up its output file.
runBsts <- function(...) {
  outputFiles <- character()
  trackedTempfile <- function(...) {
    path <- base::tempfile(...)
    outputFiles <<- c(outputFiles, path)
    path
  }
  runner <- jaspTools::runAnalysis
  environment(runner) <- list2env(list(tempfile = trackedTempfile), parent = environment(runner))
  on.exit(unlink(outputFiles), add = TRUE)
  runner(...)
}
