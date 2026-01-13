setAnalysisThreads <- function(n = 1L, verbose = TRUE) {
  n <- as.integer(n)
  if (!is.finite(n) || n < 1L) {
    n <- 1L
  }

  vars <- c(
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "BLIS_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "NUMEXPR_NUM_THREADS"
  )
  vals <- as.list(setNames(rep(as.character(n), length(vars)), vars))
  do.call(Sys.setenv, vals)

  # Also cap data.table if present
  if (requireNamespace("data.table", quietly = TRUE)) {
    try(data.table::setDTthreads(n), silent = TRUE)
  }
  # Optionally cap BLAS/OpenMP via RhpcBLASctl if present
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    try(RhpcBLASctl::blas_set_num_threads(n), silent = TRUE)
    try(RhpcBLASctl::omp_set_num_threads(n), silent = TRUE)
  }

  if (isTRUE(verbose)) {
    msg <- paste0(
      "Threads set: OMP=",
      Sys.getenv("OMP_NUM_THREADS"),
      " OPENBLAS=",
      Sys.getenv("OPENBLAS_NUM_THREADS"),
      " MKL=",
      Sys.getenv("MKL_NUM_THREADS"),
      " BLIS=",
      Sys.getenv("BLIS_NUM_THREADS"),
      " VECLIB=",
      Sys.getenv("VECLIB_MAXIMUM_THREADS"),
      " DT=",
      if (requireNamespace("data.table", quietly = TRUE)) {
        data.table::getDTthreads()
      } else {
        NA
      }
    )
    message(msg)
  }
  invisible(n)
}
