
db_names <- unique(data$cdm_name) 


db_list <- paste0("- ", db_names, collapse = "\n")


background_md <- glue::glue("
# Benchmark

This Shiny app presents results from analyses conducted on the following databases:

{db_list}

These results compare the time performance of various R packages across different common data models. The packages analysed include:

- **CDMConnector** (https://darwin-eu.github.io/CDMConnector/) 
- **IncidencePrevalence** (https://darwin-eu.github.io/IncidencePrevalence/)
- **DrugUtilisation** (https://darwin-eu.github.io/DrugUtilisation/) 
- **CohortConstructor** (https://ohdsi.github.io/CohortConstructor/) 
- **CodelistGenerator** (https://darwin-eu.github.io/CodelistGenerator/)
- **CohortCharacteristics** (https://darwin-eu.github.io/CohortCharacteristics/)


![](hdruk_logo.svg){width='100px'}
")
writeLines(background_md, "background.md")