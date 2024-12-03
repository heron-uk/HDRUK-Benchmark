library(bslib)
library(dplyr)
library(DT)
library(ggplot2)
library(glue)
library(gt)
library(here)
library(omopgenerics)
library(readr)
library(shiny)
library(sortable)
library(stringr)
library(tidyr)
library(visOmopResults)


data_file<-list.files(here::here("data"), recursive = TRUE,
                      full.names = TRUE)

data_file<-data_file[stringr::str_detect(data_file, ".csv")]
results_file<-data_file[stringr::str_detect(data_file, "benchmark")]
results <- list()
for(i in seq_along(results_file)){
  results[[i]]<-omopgenerics::importSummarisedResult(results_file[i])
  
}
data <- dplyr::bind_rows(results) 
