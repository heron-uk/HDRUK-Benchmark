# Benchmark

This Shiny app presents the results of benchmarking analyses conducted across seven databases mapped to the OMOP Common Data Model:

- **AurumCPRD**  
- **Barts Health**  
- **Edinburgh** (DataLoch C19)
- **GOSH** (Great Ormond Street Hospital) 
- **IDRIL_1** (Lancashire Teaching Hospitals)
- **LTHT_OMOP** (Leeds Teaching Hospitals)
- **UCLH-6months** (University College London Hospitals)

These benchmarking activities were carried out as part of the onboarding process for data partners joining the HERON-UK network, with the aim of assessing technical readiness and gaining insight into performance across environments.

The analyses evaluate the time performance of R packages designed for working with OMOP CDM data. Specifically, we benchmarked the execution time of selected queries implemented with the following packages:

### General Benchmark

These packages were tested for common queries and cohort-related operations across all databases:


- [**DrugUtilisation**](https://darwin-eu.github.io/DrugUtilisation/)
- [**CohortConstructor**](https://ohdsi.github.io/CohortConstructor/) 
- [**CodelistGenerator**](https://darwin-eu.github.io/CodelistGenerator/) 

### Study-Specific Benchmark

These packages were benchmarked because they are directly used in follow-up studies conducted within the HERON-UK network:

- [**CDMConnector**](https://darwin-eu.github.io/CDMConnector/)
- [**IncidencePrevalence**](https://darwin-eu.github.io/IncidencePrevalence/) 
- [**CohortCharacteristics**](https://darwin-eu.github.io/CohortCharacteristics/) 



<br>

<img src="hdruk_logo.svg" alt="HDR UK logo" style="height: 100px;" />