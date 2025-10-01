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
log_file <- file.path(outputFolder, paste0("/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"))

omopgenerics::createLogFile(logFile = log_file)

# reading tables in write schema
omopgenerics::logMessage("reading tables in write schema (initial)")

initialTables <- omopgenerics::listSourceTables(cdm = cdm)

# general benchmark
omopgenerics::logMessage("general benchmark")
result <- generalBenchmark(cdm = cdm, iterations = iterations)
general_benchmark <- result$general_benchmark
cdm <- result$cdm

# CDMConnector benchmark
omopgenerics::logMessage("CDMConnector benchmark")
cdmConnector_benchmark <- cdmConnectorBenchmark(cdm = cdm, iterations = iterations)

# IncidencePrevalence benchmark
omopgenerics::logMessage("IncidencePrevalence benchmark")
incidencePrevalence_benchmark <- incidencePrevalenceBenchmark(cdm = cdm, iterations = iterations)

# CohortCharacteristics benchmark
omopgenerics::logMessage("CohortCharacteristics benchmark")
cohortCharacteristics_benchmark <- cohortCharacteristicsBenchmark(cdm = cdm, iterations = iterations)

omopgenerics::logMessage("CohortConstructor benchmark")
cohortConstructor_benchmark <- cohortConstructorBenchmark(cdm = cdm, iterations = iterations)

omopgenerics::logMessage("DrugUtilisation benchmark")
drugUtilisation_benchmark <- drugUtilisationBenchmark(cdm = cdm, iterations = iterations)

omopgenerics::logMessage("OmopConstructor benchmark")
omopConstructor_benchmark <- omopConstructorBenchmark(cdm = cdm, iterations = iterations)


# export results
omopgenerics::logMessage("Export results")

omopgenerics::exportSummarisedResult(
  general_benchmark,
  incidencePrevalence_benchmark,
  cdmConnector_benchmark,
  cohortCharacteristics_benchmark,
  cohortConstructor_benchmark,
  drugUtilisation_benchmark,
  omopConstructor_benchmark,
  minCellCount = minCellCount,
  path = outputFolder,
  fileName = "result_benchmark_{cdm_name}_{date}.csv"
)

# reading tables in write schema
omopgenerics::logMessage("reading tables in write schema (final)")
finalTables <- omopgenerics::listSourceTables(cdm = cdm)
createdTables <- finalTables[!finalTables %in% initialTables]
if (length(createdTables) > 0) {
  createdTables <- paste0(createdTables, collapse = ", ")
  mes <- "the following tables where created in the write schema: {createdTables}" |>
    glue::glue()
  omopgenerics::logMessage(mes)
}

# Close connection
omopgenerics::logMessage("closing connection")
CDMConnector::cdmDisconnect(cdm)

# Zip the results
omopgenerics::logMessage("ziping results")
root_files <- list.files(
  outputFolder,
  pattern     = "\\.(csv|txt)$",
  recursive   = FALSE,
  full.names  = FALSE
)

resultLog <- omopgenerics::summariseLogFile(cdmName = dbName)
omopgenerics::exportSummarisedResult(resultLog,
                                     minCellCount = minCellCount,
                                     path = outputFolder,
                                     fileName = "log_benchmark_{cdm_name}_{date}.csv")

# 2) All SQL files anywhere under outputFolder (keeps subfolder paths)
sql_files <- list.files(
  outputFolder,
  pattern     = "\\.(sql|txt)$",
  recursive   = TRUE,
  full.names  = FALSE
)

# 3) Zip while preserving structure (paths like "sub/dir/file.sql" are kept)
zip::zip(
  zipfile = file.path(outputFolder, paste0("results_", dbName, ".zip")),
  files   = c(root_files, sql_files),
  root    = outputFolder
)
