source("R/functions.R")
iterations <- 1

# create log file
outputFolder <- here::here("Results")
logfile <- file.path(outputFolder, paste0(
  "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))
logger <- log4r::create.logger(logfile = logfile, level = "INFO")

# general benchmark
log4r::info(logger = logger, "general benchmark")
general_benchmark <- generalBenchmark(cdm = cdm, iterations = iterations)

# CDMConnector benchmark
log4r::info(logger = logger, "CDMConnector benchmark")
cdmConnector_benchmark <- cdmConnectorBenchmark(cdm = cdm, iterations = iterations)

# IncidencePrevalence benchmark
log4r::info(logger = logger, "IncidencePrevalence benchmark")
incidencePrevalence_benchmark <- incidencePrevalenceBenchmark(cdm = cdm, iterations = iterations)

# export results
log4r::info(logger = logger, "Export results")
omopgenerics::exportSummarisedResult(
  general_benchmark,
  incidencePrevalence_benchmark,
  cdmConnector_benchmark,
  path = outputFolder,
  fileName = "result_benchmark_{cdmName}.csv"
)

# Close connection
CDMConnector::cdmDisconnect(cdm)

# Zip the results
files_to_zip <- list.files(outputFolder)
files_to_zip <- files_to_zip[stringr::str_detect(files_to_zip, dbName)]

zip::zip(
  zipfile = file.path(paste0(
    outputFolder, "/results_", db_name, ".zip"
  )),
  files = files_to_zip,
  root = outputFolder
)
