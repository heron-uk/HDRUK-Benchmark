generalBenchmark <- function(cdm, iterations)
{
  res <- tibble::tibble()

  for(i in 1:iterations){

tictoc::tic()
cdm$condition_occurrence |>
  dplyr::tally() |>
  dplyr::pull("n")
t <- tictoc::toc()
task_name <- "Count condition occurrence rows"
res <- new_rows(res, task_name = task_name, time = t, iteration = i)

# 2) grouped count
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



# 3) join
tictoc::tic()
cdm$person |>
  dplyr::left_join(cdm$condition_occurrence |>
                     dplyr::group_by(person_id) |>
                     dplyr::tally() |>
                     dplyr::ungroup()) |>
  dplyr::filter(is.na(n)) |>
  dplyr::tally() |>
  dplyr::pull("n")
t<- tictoc::toc()
task_name <- "Count individuals in person but not in condition occurrence table"
res <- new_rows(res, task_name = task_name, time = t, iteration = i)

# 4) compute to write schema
tictoc::tic()
cdm$person_ws<- cdm$person |>
  dplyr::compute(name = "person_ws",
                 temporary = FALSE)
cdm$person_ws |>
  dplyr::tally() |>
  dplyr::pull("n")

t<- tictoc::toc()
task_name <- "Compute person table to write schema"
res <- new_rows(res, task_name = task_name, time = t, iteration = i)


# 5) join with a table in the write_schema
tictoc::tic()
cdm$person_ws |>
  dplyr::left_join(cdm$condition_occurrence |>
                     dplyr::group_by(person_id) |>
                     dplyr::tally() |>
                     dplyr::ungroup()) |>
  dplyr::filter(is.na(n)) |>
  dplyr::tally() |>
  dplyr::pull("n")
t<- tictoc::toc()
task_name <- "Count individuals in person (in write schema) but not in condition occurrence table"
res <- new_rows(res, task_name = task_name, time = t, iteration = i)

# incprev denominator
tictoc::tic()
cdm <- IncidencePrevalence::generateDenominatorCohortSet(cdm = cdm,
                                                         name = "denominator",
                                                         ageGroup = list(c(18,150),
                                                                         c(18,64),
                                                                         c(65,150)),
                                                         cohortDateRange = as.Date(c("2013-01-01",
                                                                                     "2022-12-31")),
                                                         sex = c("Both", "Male", "Female"),
                                                         daysPriorObservation = c(365))


t<- tictoc::toc()
task_name <- "Create IncidencePrevalence cohorts"
res <- new_rows(res, task_name = task_name, time = t, iteration = i)


# drugutilisation cohort
tictoc::tic()
druglist <- CodelistGenerator::getDrugIngredientCodes(cdm, c("acetaminophen", "metformin"))
tictoc::toc()
task_name <- "Get ingredient codes with CodelistGenerator"
res <- new_rows(res, task_name = task_name, time = t, iteration = i)

tictoc::tic()
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "drug_cohorts",
  conceptSet = druglist
)
cdm$drug_cohorts <- cdm$drug_cohorts |> DrugUtilisation::requireObservationBeforeDrug(days = 365)
t<- tictoc::toc()
task_name <- "Create DrugUtilisation cohorts"
res <- new_rows(res, task_name = task_name, time = t, iteration = i)

tictoc::tic()
codes_sin <- CodelistGenerator::getCandidateCodes(cdm, c("sinusitis"))$concept_id
codes_ph <- CodelistGenerator::getCandidateCodes(cdm, c( "pharyngitis"))$concept_id
codes_bro <- CodelistGenerator::getCandidateCodes(cdm, c( "bronchitis"))$concept_id
tictoc::toc()
task_name <- "Get conditions codes with CodelistGenerator"
res <- new_rows(res, task_name = task_name, time = t, iteration = i)

codes <- omopgenerics::newCodelist(list("sinusitis"=codes_sin, "bronchitis"=codes_bro, "pharyngitis"=codes_ph))

tictoc::tic()
cdm$conditions_cohort <- CohortConstructor::conceptCohort(cdm, conceptSet = codes, name = "conditions_cohort")
tictoc::toc()
task_name <- "Create condtions cohorts with CohortConstructor"
res <- new_rows(res, task_name = task_name, time = t, iteration = i)
}

res <- res |>
  dplyr::mutate(dbms = attr(attr(cdm, "cdm_source"), "source_type"),
                person_n = cdm$person|>dplyr::distinct(person_id)|>dplyr::tally()|>dplyr::pull())|>
  dplyr::mutate(result_id = 1L,
                cdm_name = omopgenerics::cdmName(cdm),
                group_name = "task",
                strata_name = "iteration",
                variable_name = "overall",
                variable_level = "overall",
                estimate_type = "numeric")|>
  visOmopResults::uniteAdditional(cols = c("dbms","person_n" ))

settings <- dplyr::tibble(
  result_id = unique(res$result_id),
  package_name = "",
  package_version = "",
  result_type = "summarise_general_benchmark"
)
res <- res |>
  omopgenerics::newSummarisedResult(settings = settings)

return(res)
}

new_rows <- function(res, task_name, time, iteration) {

  res<- dplyr::bind_rows(res, tibble::tibble(
                        group_level = task_name,
                        estimate_value = as.character(time$toc-time$tic),
                        estimate_name = "time_seconds",
                        strata_level = as.character(iteration)) |>
    tibble::add_row(group_level = task_name,
                    estimate_value = as.character(round((time$toc-time$tic)/60, 1)),
                    estimate_name = "time_minutes",
                    strata_level = as.character(iteration)))
  return(res)
}


incidencePrevalenceBenchmark <-function(cdm , iterations){
  res <- omopgenerics::emptySummarisedResult()
  for (i in 1:iterations){
   x <- IncidencePrevalence::benchmarkIncidencePrevalence(cdm, analysisType = "all")
   x<- x|> dplyr::mutate(strata_name = "iteration",
                         strata_level = as.character(i),
                         estimate_name = "time_minutes")
    x <- dplyr::bind_rows(x,
       x |>
         dplyr::filter(estimate_name == "time_minutes") |>
         dplyr::mutate(
           estimate_name = "time_seconds",
           estimate_value = as.character(as.numeric(estimate_value) * 60)
         )
     )
   res<-dplyr::bind_rows(res, x)


  }
  settings <- dplyr::tibble(
  result_id = unique(res$result_id),
  package_name = "",
  package_version = "",
  result_type = "summarise_incidence_prevalence_benchmark"
   )
   res <- res |>
     omopgenerics::newSummarisedResult(settings = settings)
  return(res)

}

cdmConnectorBenchmark <- function(cdm, iterations){

  res<-list()
  for (i in 1:iterations){
    res<-dplyr::bind_rows(res, CDMConnector::benchmarkCDMConnector(cdm)|>
      dplyr::mutate(strata_level = as.character(i)))
  }

  res <- res |>
    tidyr::pivot_longer(
      cols = c(.data$time_taken_secs, .data$time_taken_mins),
      names_to = "estimate_name",
      values_to = "estimate_value",
      names_transform = list(estimate_name = ~ case_when(
        . == "time_taken_secs" ~ "time_seconds",
        . == "time_taken_mins" ~ "time_minutes"
      ))
    ) |>
    visOmopResults::uniteAdditional(cols = c("dbms", "person_n"))|>
    dplyr::mutate(result_id = 1L,
                  cdm_name = omopgenerics::cdmName(cdm),
                  group_name = "task",
                  strata_name = "iteration",
                  variable_name = "overall",
                  variable_level = "overall",
                  estimate_type = "numeric",
                  estimate_value = as.character(.data$estimate_value))|>
    dplyr::rename("group_level" = "task")

  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = "",
    package_version = "",
    result_type = "summarise_cdm_connector_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(res)

}
