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
generalBenchmark <- function(cdm, iterations) {

  res <- tibble::tibble()

  for (i in 1:iterations) {
    mes <- glue::glue("general benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)

    # 1) Count condition occurrence rows
    tictoc::tic()
    cdm$condition_occurrence |>
      dplyr::tally() |>
      dplyr::pull("n") |>
      suppressMessages()
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
      dplyr::pull("n") |>
      suppressMessages()
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
      dplyr::pull("n") |>
      suppressMessages()
    t <- tictoc::toc()
    task_name <- "Count individuals in person but not in condition occurrence table"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 4) Compute person table to write schema
    tictoc::tic()
    cdm$person_ws <- cdm$person |>
      dplyr::compute(
        name = "person_ws",
        temporary = FALSE
      )|>
      suppressMessages()
    cdm$person_ws |>
      dplyr::tally() |>
      dplyr::pull("n") |>
      suppressMessages()
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
      dplyr::pull("n") |>
      suppressMessages()
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
    ) |> suppressMessages()
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
    ) |>
      suppressMessages()
    t <- tictoc::toc()
    task_name <- "Create demographics cohorts"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 8) Get ingredient codes with CodelistGenerator
    tictoc::tic()
    druglist <- CodelistGenerator::getDrugIngredientCodes(
      cdm = cdm, name = NULL, nameStyle = "{concept_code}_{concept_name}") |>
      suppressMessages()
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
    ) |>
      suppressMessages()
    t <- tictoc::toc()
    task_name <- "Instantiate acetaminophen and warfarin cohorts"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 10) Require 365 days of prior washout to drug_cohorts
    tictoc::tic()
    cdm$drug_cohorts <- cdm$drug_cohorts |>
      DrugUtilisation::requirePriorDrugWashout(days = 365) |>
      suppressMessages()
    t <- tictoc::toc()
    task_name <- "Require 365 days of prior washout to drug_cohorts"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 11) Get conditions codes with CodelistGenerator
    tictoc::tic()
    codes_sin <- CodelistGenerator::getCandidateCodes(cdm, c("sinusitis")) |>
      suppressMessages() |>
      dplyr::pull("concept_id")
    codes_ph <- CodelistGenerator::getCandidateCodes(cdm, c("pharyngitis")) |>
      suppressMessages() |>
      dplyr::pull("concept_id")
    codes_bro <- CodelistGenerator::getCandidateCodes(cdm, c("bronchitis"))|>
      suppressMessages() |>
      dplyr::pull("concept_id")
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
    ) |>
      suppressMessages()
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
    ) |> suppressMessages()
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
      ) |>
      suppressMessages()
    t <- tictoc::toc()
    task_name <- "Cohort created using codes descendant from diabetes, hypertension disorder and asthma"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)
  }

  omopgenerics::logMessage("Compile results for general benchmark")

  res <- res |>
    dplyr::mutate(
      dbms = omopgenerics::sourceType(cdm),
      person_n = omopgenerics::numberSubjects(cdm$person)
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

incidencePrevalenceBenchmark <- function(cdm, iterations) {
  res <- omopgenerics::emptySummarisedResult()

  for (i in 1:iterations) {
    mes <- glue::glue("IncidencePrevalence benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)

    x <- IncidencePrevalence::benchmarkIncidencePrevalence(cdm, analysisType = "all") |>
      suppressMessages()
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

  omopgenerics::logMessage("Compile results for IncidencePrevalence benchmark")

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

cdmConnectorBenchmark <- function(cdm, iterations) {

  res <- list()

  for (i in 1:iterations) {
    mes <- glue::glue("CDMConnector benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)
    res <- dplyr::bind_rows(res, CDMConnector::benchmarkCDMConnector(cdm) |>
      dplyr::mutate(strata_level = as.character(i))) |>
      suppressMessages()
  }

  omopgenerics::logMessage("Compile results for CDMConnector benchmark")
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

cohortCharacteristicsBenchmark <- function(cdm, iterations) {
  res <- omopgenerics::emptySummarisedResult()

  for (i in 1:iterations) {
    mes <- glue::glue("CohortCharacteristics benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)

    x <- CohortCharacteristics::benchmarkCohortCharacteristics(cdm$my_cohort) |>
      suppressMessages()
    x <- x |>
    dplyr::mutate(
      strata_name = "iteration",
      strata_level = as.character(i),
      estimate_name = "time_seconds"
    )

  res <- dplyr::bind_rows(res, x)
  }

  omopgenerics::logMessage("Compile results for CohortCharacteristics benchmark")

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



cohortConstructorBenchmark <- function(cdm, iterations) {

  res <- list()

  for (i in 1:iterations) {
    mes <- glue::glue("CohortConstructor benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)
    res <- dplyr::bind_rows(res, CohortConstructor::benchmarkCohortConstructor(cdm, runCIRCE = FALSE)  |>
                              suppressMessages() |>
                              omopgenerics::filterSettings(result_type %in% c("instanciation_time", "cohort_count")) |>
                              dplyr::mutate(iteration = as.character(i)))
  }
  omopgenerics::logMessage("Compile results for CohortConstructor benchmark")

  subj <- res |>
    omopgenerics::addSettings(settingsColumn = "result_type") |>
    dplyr::filter(.data$result_type == "cohort_count") |>
    omopgenerics::tidy() |>
    dplyr::filter(.data$variable_name == "number_subjects") |>
    dplyr::mutate(cohort_name = stringr::str_remove(.data$cohort_name, "^cc_")) |>
    dplyr::select("cdm_name", "iteration", "cohort_name",
                  "person_n" = "count")


  inst <- res |>
    omopgenerics::addSettings(settingsColumn = "result_type") |>
    dplyr::filter(.data$result_type == "instanciation_time") |>
    omopgenerics::tidy() |>
    dplyr::mutate(
      cohort_name = dplyr::case_when(
        .data$variable_name == "Cohort set"                                     ~ paste0("Cohort set: ", .data$variable_level),
        .data$variable_name == "Acquired neutropenia or unspecified leukopenia" ~ "neutropenia_leukopenia",
        .data$variable_name == "Asthma without COPD"                            ~ "asthma_no_copd",
        .data$variable_name == "COVID-19"                                       ~ "covid",
        .data$variable_name == "COVID-19: female"                               ~ "covid_female",
        .data$variable_name == "COVID-19: female, 0 to 50"                      ~ "covid_female_0_to_50",
        .data$variable_name == "COVID-19: female, 51 to 150"                    ~ "covid_female_51_to_150",
        .data$variable_name == "COVID-19: male"                                 ~ "covid_male",
        .data$variable_name == "COVID-19: male, 0 to 50"                        ~ "covid_male_0_to_50",
        .data$variable_name == "COVID-19: male, 51 to 150"                      ~ "covid_male_51_to_150",
        .data$variable_name == "Endometriosis procedure"                        ~ "endometriosis_procedure",
        .data$variable_name == "Inpatient hospitalisation"                      ~ "hospitalisation",
        .data$variable_name == "Major non cardiac surgery"                      ~ "major_non_cardiac_surgery",
        .data$variable_name == "New fluoroquinolone users"                      ~ "new_fluoroquinolone",
        .data$variable_name == "New users of beta blockers nested in essential hypertension"
        ~ "beta_blockers_hypertension",
        .data$variable_name == "Transverse myelitis"                            ~ "transverse_myelitis",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select("cdm_name", "iteration", "cohort_name",
                  "time_minutes" = "time")


  out <- inst |>
    dplyr::left_join(subj, by = c("cdm_name", "iteration", "cohort_name")) |>
    dplyr::select("cdm_name", "iteration", "cohort_name",
                  "person_n", "time_minutes")


  out <- out |>
    dplyr::mutate(time_seconds = as.numeric(.data$time_minutes * 60L),
                  dbms = omopgenerics::sourceType(cdm)
                  ) |>
    tidyr::pivot_longer(
      cols = c(time_seconds, time_minutes),
      names_to = "estimate_name",
      values_to = "estimate_value"
      ) |>
    visOmopResults::uniteAdditional(cols = c("dbms", "person_n")) |>
    visOmopResults::uniteStrata(cols = "iteration") |>
    dplyr::rename("group_level" = "cohort_name") |>
    dplyr::mutate(
      result_id = 1L,
      group_name = "task",
      variable_name = "overall",
      variable_level = "overall",
      estimate_type = "numeric",
      estimate_value = sprintf("%.3f", .data$estimate_value))


  settings <- dplyr::tibble(
    result_id = 1L,
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_cohort_constructor_benchmark"
  )
  out <- out |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(out)
}




drugUtilisationBenchmark <- function(cdm, iterations) {

  res <- list()

  for (i in 1:iterations) {
    mes <- glue::glue("DrugUtilisation benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)
    res <- dplyr::bind_rows(res, DrugUtilisation::benchmarkDrugUtilisation(cdm) |>
                              dplyr::mutate(strata_level = as.character(i)) |>
                              suppressMessages())
  }
  omopgenerics::logMessage("Compile results for DrugUtilisation benchmark")



  res <- res |>
    omopgenerics::pivotEstimates() |>
    dplyr::mutate(time_minutes = as.numeric(.data$time_seconds / 60L),
                  dbms = omopgenerics::sourceType(cdm),
                  person_n = omopgenerics::numberSubjects(cdm$person),
                  "strata_name" = "iteration") |>
    tidyr::pivot_longer(
      cols = c(time_seconds, time_minutes),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    visOmopResults::splitAdditional() |>
    visOmopResults::uniteAdditional(cols = c("dbms", "person_n")) |>
    dplyr::mutate(
      estimate_type = "numeric",
      estimate_value = sprintf("%.2f", .data$estimate_value))


  settings <- dplyr::tibble(
    result_id = 1L,
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_drug_utilisation_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(res)
}


omopConstructorBenchmark <- function(cdm, iterations) {
  res <- tibble::tibble()
  for (i in 1:iterations) {
    mes <- glue::glue("OmopConstructor iteration {i}/{iterations}")

    omopgenerics::logMessage(mes)

    tictoc::tic()

    cdm <- OmopConstructor::buildObservationPeriod(
      cdm,
      collapseDays = Inf,
      persistenceDays = Inf

    )|>
      suppressMessages()

    t <- tictoc::toc()

    task_name <- "OP: first record to data extraction"

    res <- new_rows(res, task_name = task_name, time = t, iteration = i)


    tictoc::tic()

    cdm <- OmopConstructor::buildObservationPeriod(
      cdm = cdm,
      collapseDays = 1,
      persistenceDays = 0,
      recordsFrom = "visit_occurrence"
    ) |>
      suppressMessages()

    t <- tictoc::toc()

    task_name <- "OP: Inpatient"

    res <- new_rows(res, task_name = task_name, time = t, iteration = i)


    tictoc::tic()

    cdm <- OmopConstructor::buildObservationPeriod(
      cdm = cdm,
      collapseDays = 365,
      persistenceDays = 364,
      censorAge = 60
    ) |>
     suppressMessages()
    t <- tictoc::toc()
    task_name <- "OP: Collapse+Persistence 365 - Age<60"
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)
  }

  omopgenerics::logMessage("Compile results for OmopConstructor benchmark")

  res <- res |>
    dplyr::mutate(
      dbms = omopgenerics::sourceType(cdm),
      person_n = omopgenerics::numberSubjects(cdm$person)
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
    result_type = "summarise_omop_constructor_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)

  return(res)
}
