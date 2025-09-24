source("R/functions.R")

iterations <- 1
pkg_name <- "HDRUK-benchmark"
pkg_version <- "0.2.0"

# create log file
outputFolder <- here::here("Results")
options(
  omopgenerics.log_sql_path = paste0(outputFolder, "/sql_logs"),
  omopgenerics.log_sql_explain_path = paste0(outputFolder, "/sql_explain")
)
log_file <- file.path(outputFolder, paste0(
  "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))
logger <- log4r::create.logger(logfile = log_file, level = "INFO")

# reading tables in write schema
log4r::info(logger = logger, "reading tables in write schema (initial)")
initialTables <- omopgenerics::listSourceTables(cdm = cdm)

# general benchmark
log4r::info(logger = logger, "general benchmark")
result <- generalBenchmark(cdm = cdm, iterations = iterations, logger = logger)
general_benchmark <- result$general_benchmark
cdm <- result$cdm

# CDMConnector benchmark
log4r::info(logger = logger, "CDMConnector benchmark")
cdmConnector_benchmark <- cdmConnectorBenchmark(cdm = cdm, iterations = iterations, logger = logger)

# IncidencePrevalence benchmark
log4r::info(logger = logger, "IncidencePrevalence benchmark")
incidencePrevalence_benchmark <- incidencePrevalenceBenchmark(cdm = cdm, iterations = iterations, logger = logger)

# CohortCharacteristics benchmark
log4r::info(logger = logger, "CohortCharacteristics benchmark")
cohortCharacteristics_benchmark <- cohortCharacteristicsBenchmark(cdm = cdm, iterations = iterations, logger = logger)

log4r::info(logger = logger, "CohortConstructor benchmark")
cohortConstructor_benchmark <- cohortConstructorBenchmark(cdm = cdm, iterations = iterations, logger = logger)

log4r::info(logger = logger, "DrugUtilisation benchmark")
drugUtilisation_benchmark <- drugUtilisationBenchmark(cdm = cdm, iterations = iterations, logger = logger)

# export results
log4r::info(logger = logger, "Export results")

omopgenerics::exportSummarisedResult(
  general_benchmark,
  incidencePrevalence_benchmark,
  cdmConnector_benchmark,
  cohortCharacteristics_benchmark,
  cohortConstructor_benchmark,
  drugUtilisation_benchmark,
  minCellCount = minCellCount,
  path = outputFolder,
  fileName = "result_benchmark_{cdm_name}_{date}.csv"
)

# reading tables in write schema
log4r::info(logger = logger, "reading tables in write schema (final)")
finalTables <- omopgenerics::listSourceTables(cdm = cdm)
createdTables <- finalTables[!finalTables %in% initialTables]
if (length(createdTables) > 0) {
  createdTables <- paste0(createdTables, collapse = ", ")
  mes <- "the following tables where created in the write schema: {createdTables}" |>
    glue::glue()
  log4r::info(logger = logger, mes)
}

# Close connection
log4r::info(logger = logger, "closing connection")
CDMConnector::cdmDisconnect(cdm)

# Zip the results
log4r::info(logger = logger, "ziping results")
root_files <- list.files(
  outputFolder,
  pattern     = "\\.(csv|txt)$",
  recursive   = FALSE,
  full.names  = FALSE
)

# 2) All SQL files anywhere under outputFolder (keeps subfolder paths)
sql_files <- list.files(
  outputFolder,
  pattern     = "\\.sql$",
  recursive   = TRUE,
  full.names  = FALSE
)

# 3) Zip while preserving structure (paths like "sub/dir/file.sql" are kept)
zip::zip(
  zipfile = file.path(outputFolder, paste0("results_", dbName, ".zip")),
  files   = c(root_files, sql_files),
  root    = outputFolder
)
