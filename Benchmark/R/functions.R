new_rows <- function(res, task_name, time, iteration) {
  res <- dplyr::bind_rows(res, tibble::tibble(
    group_level = task_name,
    estimate_value = as.character(time$toc - time$tic),
    estimate_name = "time_seconds",
    strata_level = as.character(iteration)
  ) |>
    tibble::add_row(
      group_level = task_name,
      estimate_value = as.character(round((time$toc - time$tic) / 60, 1)),
      estimate_name = "time_minutes",
      strata_level = as.character(iteration)
    ))
  return(res)
}
check_int64 <- function(druglist){
if (any(purrr::map_lgl(druglist, inherits, "integer64"))) {
  druglist <- purrr::map(druglist, as.integer)
}
  return(druglist)
}
generalBenchmark <- function(cdm, iterations, logger) {

  res <- tibble::tibble()

  for (i in 1:iterations) {
    mes <- glue::glue("general benchmark interation {i}/{iterations}")
    log4r::info(logger = logger, mes)

    # 1) Count condition occurrence rows
    tictoc::tic()
    cdm$condition_occurrence |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    task_name <- "Count condition occurrence rows"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 2) Count individuals in condition occurrence table
    tictoc::tic()
    cdm$condition_occurrence |>
      dplyr::group_by(person_id) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    task_name <- "Count individuals in condition occurrence table"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 3) Count individuals in person but not in condition occurrence table
    tictoc::tic()
    cdm$person |>
      dplyr::left_join(
        cdm$condition_occurrence |>
          dplyr::group_by(person_id) |>
          dplyr::tally() |>
          dplyr::ungroup(),
        by = "person_id"
      ) |>
      dplyr::filter(is.na(n)) |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    task_name <- "Count individuals in person but not in condition occurrence table"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 4) Compute person table to write schema
    tictoc::tic()
    cdm$person_ws <- cdm$person |>
      dplyr::compute(
        name = "person_ws",
        temporary = FALSE
      )
    cdm$person_ws |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    task_name <- "Compute person table to write schema"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 5) Count individuals in person (in write schema) but not in condition occurrence table
    tictoc::tic()
    cdm$person_ws |>
      dplyr::left_join(
        cdm$condition_occurrence |>
          dplyr::group_by(person_id) |>
          dplyr::tally() |>
          dplyr::ungroup(),
        by = "person_id"
      ) |>
      dplyr::filter(is.na(n)) |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    task_name <- "Count individuals in person (in write schema) but not in condition occurrence table"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 6) Create IncidencePrevalence cohorts
    tictoc::tic()
    cdm <- IncidencePrevalence::generateDenominatorCohortSet(
      cdm = cdm,
      name = "denominator",
      ageGroup = list(c(18, 150), c(0, 17), c(18, 64), c(65, 150)),
      sex = c("Both", "Male", "Female"),
      daysPriorObservation = c(365)
    )
    t <- tictoc::toc()
    task_name <- "Create IncidencePrevalence cohorts"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 7) Create demographics cohorts
    tictoc::tic()
    cdm$denominator_cc <- CohortConstructor::demographicsCohort(
      cdm = cdm,
      name = "denominator_cc",
      ageRange = list(c(18, 150), c(0, 17), c(18, 64), c(65, 150)),
      sex = c("Both", "Male", "Female"),
      minPriorObservation = c(365)
    )
    t <- tictoc::toc()
    task_name <- "Create demographics cohorts"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 8) Get ingredient codes with CodelistGenerator
    tictoc::tic()
    druglist <- CodelistGenerator::getDrugIngredientCodes(
      cdm = cdm, name = NULL, nameStyle = "{concept_code}_{concept_name}")
    t <- tictoc::toc()
    task_name <- "Get ingredient codes with CodelistGenerator"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    druglist <- check_int64(druglist)

    # 9) Instantiate acetaminophen and metformin cohorts
    tictoc::tic()
    cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
      cdm = cdm,
      name = "drug_cohorts",
      conceptSet = druglist[c("161_acetaminophen", "11289_warfarin")],
      gapEra = 30
    )
    t <- tictoc::toc()
    task_name <- "Instantiate acetaminophen and warfarin cohorts"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 10) Require 365 days of prior washout to drug_cohorts
    tictoc::tic()
    cdm$drug_cohorts <- cdm$drug_cohorts |>
      DrugUtilisation::requirePriorDrugWashout(days = 365)
    t <- tictoc::toc()
    task_name <- "Require 365 days of prior washout to drug_cohorts"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 11) Get conditions codes with CodelistGenerator
    tictoc::tic()
    codes_sin <- CodelistGenerator::getCandidateCodes(cdm, c("sinusitis"))$concept_id
    codes_ph <- CodelistGenerator::getCandidateCodes(cdm, c("pharyngitis"))$concept_id
    codes_bro <- CodelistGenerator::getCandidateCodes(cdm, c("bronchitis"))$concept_id
    t <- tictoc::toc()
    task_name <- "Get conditions codes with CodelistGenerator"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    codes <- omopgenerics::newCodelist(list("sinusitis" = codes_sin, "bronchitis" = codes_bro, "pharyngitis" = codes_ph)|>check_int64())

    # 12) Create condtions cohorts with CohortConstructor
    tictoc::tic()
    cdm$conditions_cohort <- CohortConstructor::conceptCohort(
      cdm = cdm,
      conceptSet = codes,
      name = "conditions_cohort"
    )
    t <- tictoc::toc()
    task_name <- "Create condtions cohorts with CohortConstructor"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 13) Summary cdm
    tictoc::tic()
    snap <- summary(cdm)
    t <- tictoc::toc()
    task_name <- "Summary cdm"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 14) Drop tables created
    tictoc::tic()
    cdm <- CDMConnector::dropSourceTable(
      cdm = cdm,
      name = dplyr::starts_with(c(
        "conditions_cohort", "drug_cohorts", "denominator_cc", "denominator",
        "person_ws"
      ))
    )
    t <- tictoc::toc()
    task_name <- "Drop tables created"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 15) Create cohort for diabetes, asthma and hypertension
    tictoc::tic()
    cdm <- CDMConnector::generateConceptCohortSet(
      cdm = cdm,
      conceptSet = list("diabetes" = CodelistGenerator::getDescendants(cdm, conceptId = 201820)$concept_id,
                      "hypertension_disorder" = CodelistGenerator::getDescendants(cdm, conceptId = 316866)$concept_id,
                      "asthma" = CodelistGenerator::getDescendants(cdm, conceptId = 317009)$concept_id) |>
        check_int64(),
      name = "my_cohort"
      )
    t <- tictoc::toc()
    task_name <- "Cohort created using codes descendant from diabetes, hypertension disorder and asthma"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)
  }

  log4r::info(logger = logger, "Compile results for general benchmark")

  res <- res |>
    dplyr::mutate(
      dbms = attr(attr(cdm, "cdm_source"), "source_type"),
      person_n = cdm$person |> dplyr::distinct(person_id) |> dplyr::tally() |> dplyr::pull()
    ) |>
    dplyr::mutate(
      result_id = 1L,
      cdm_name = omopgenerics::cdmName(cdm),
      group_name = "task",
      strata_name = "iteration",
      variable_name = "overall",
      variable_level = "overall",
      estimate_type = "numeric"
    ) |>
    visOmopResults::uniteAdditional(cols = c("dbms", "person_n"))

  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_general_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)

  return(list(general_benchmark = res, cdm = cdm))
}

incidencePrevalenceBenchmark <- function(cdm, iterations, logger) {
  res <- omopgenerics::emptySummarisedResult()

  for (i in 1:iterations) {
    mes <- glue::glue("IncidencePrevalence benchmark interation {i}/{iterations}")
    log4r::info(logger = logger, mes)

    x <- IncidencePrevalence::benchmarkIncidencePrevalence(cdm, analysisType = "all")
    x <- x |>
      dplyr::mutate(
        strata_name = "iteration",
        strata_level = as.character(i),
        estimate_name = "time_minutes"
      )
    x <- dplyr::bind_rows(
      x,
      x |>
        dplyr::filter(estimate_name == "time_minutes") |>
        dplyr::mutate(
          estimate_name = "time_seconds",
          estimate_value = as.character(as.numeric(estimate_value) * 60)
        )
    )
    res <- dplyr::bind_rows(res, x)
  }

  log4r::info(logger = logger, "Compile results for IncidencePrevalence benchmark")

  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_incidence_prevalence_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(res)
}

cdmConnectorBenchmark <- function(cdm, iterations, logger) {

  source("R/benchmarkCDMConnector.R")

  res <- list()

  for (i in 1:iterations) {
    mes <- glue::glue("CDMConnector benchmark interation {i}/{iterations}")
    log4r::info(logger = logger, mes)
    res <- dplyr::bind_rows(res, benchmarkCDMConnector(cdm) |>
      dplyr::mutate(strata_level = as.character(i)))
  }

  log4r::info(logger = logger, "Compile results for CDMConnector benchmark")
  res <- res |>
    tidyr::pivot_longer(
      cols = c(time_taken_secs, time_taken_mins),
      names_to = "estimate_name",
      values_to = "estimate_value",
      names_transform = list(estimate_name = ~ dplyr::case_when(
        . == "time_taken_secs" ~ "time_seconds",
        . == "time_taken_mins" ~ "time_minutes"
      ))
    ) |>
    visOmopResults::uniteAdditional(cols = c("dbms", "person_n")) |>
    dplyr::mutate(
      result_id = 1L,
      cdm_name = omopgenerics::cdmName(cdm),
      group_name = "task",
      strata_name = "iteration",
      variable_name = "overall",
      variable_level = "overall",
      estimate_type = "numeric",
      estimate_value = as.character(.data$estimate_value)
    ) |>
    dplyr::rename("group_level" = "task")

  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_cdm_connector_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(res)
}

cohortCharacteristicsBenchmark <- function(cdm, iterations, logger) {
  res <- omopgenerics::emptySummarisedResult()

  for (i in 1:iterations) {
    mes <- glue::glue("CohortCharacteristics benchmark interation {i}/{iterations}")
    log4r::info(logger = logger, mes)

    x <- CohortCharacteristics::benchmarkCohortCharacteristics(cdm$my_cohort)
    x <- x |>
    dplyr::mutate(
      strata_name = "iteration",
      strata_level = as.character(i),
      estimate_name = "time_seconds"
    )

  res <- dplyr::bind_rows(res, x)
  }

  log4r::info(logger = logger, "Compile results for CohortCharacteristics benchmark")

  res <- dplyr::bind_rows(
    res,
    res |>
      dplyr::filter(estimate_name == "time_seconds") |>
      dplyr::mutate(
        estimate_name = "time_minutes",
        estimate_value = as.character(as.numeric(estimate_value) / 60)
      )
  ) |>
    dplyr::mutate(
      dbms = omopgenerics::sourceType(cdm),
      person_n = omopgenerics::settings(x)$person_n
    ) |>
    dplyr::select(!c(additional_name, additional_level)) |>
    omopgenerics::uniteAdditional(cols = c("dbms", "person_n"))
  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_cohort_characteristics_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(res)
}



