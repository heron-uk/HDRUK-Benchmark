source("R/functions.R")

iterations <- 1 # number of times to run the same query

general_benchmark <- generalBenchmark(cdm = cdm, iterations = iterations)
incidencePrevalence_benchmark <- incidencePrevalenceBenchmark(cdm = cdm, iterations = iterations)
cdmConnector_benchmark <- cdmConnectorBenchmark(cdm = cdm, iterations = iterations)

result <- omopgenerics::bind(general_benchmark, incidencePrevalence_benchmark, cdmConnector_benchmark)

omopgenerics::exportSummarisedResult(result, path = here::here("Results"), fileName = paste0(
  "result_timings_", db_name, ".csv"
))

# Close connection
cdm_disconnect(cdm)

# Zip the results
results_dir <- here::here("Results")
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}
files_to_zip <- list.files(results_dir)
files_to_zip <- files_to_zip[stringr::str_detect(files_to_zip, db_name)]

zip::zip(
  zipfile = file.path(paste0(
    results_dir, "/results_", db_name, ".zip"
  )),
  files = files_to_zip,
  root = results_dir
)
