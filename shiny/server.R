
update_input_choices <- function(session, input_id, choices) {
  shiny::updateSelectizeInput(
    session = session,
    inputId = input_id,
    choices = choices,
    selected = choices,
    server = TRUE
  )
}

filter_data <- function(data, input){
  
  res<- visOmopResults::filterSettings(data, .data$result_type %in%
                                         input$summarise_benchmark_grouping_result_type)
  if (nrow(res) == 0){
    return(omopgenerics::emptySummarisedResult())
  }

  res<- res|>
    visOmopResults::splitAll() |>
    dplyr::filter(.data$cdm_name %in% input$summarise_benchmark_grouping_cdm_name,
                  .data$task %in% input$summarise_benchmark_grouping_task,
                  .data$iteration %in% input$summarise_benchmark_grouping_iteration,
                  .data$dbms %in% input$summarise_benchmark_grouping_dbms,
                  .data$estimate_name %in% input$summarise_benchmark_estimate_name)|>
    visOmopResults::uniteStrata("iteration")|>
    (\(df) visOmopResults::uniteAdditional(df, intersect(c("person_n", "dbms", "min_observation_start", "max_observation_end"), colnames(df))))()|>
    visOmopResults::uniteGroup("task")
  

  return(res)

}
simpleTable <- function(result,
                        header = character(),
                        group = character(),
                        hide = character()) {
  # initial checks
  if (length(header) == 0) header <- character()
  if (length(group) == 0) group <- NULL
  if (length(hide) == 0) hide <- character()
  
  if (nrow(result) == 0) {
    return(gt::gt(dplyr::tibble()))
  }
  
  result <- result |>
    omopgenerics::addSettings() |>
    omopgenerics::splitAll() |>
    dplyr::select(-c("result_id", "min_observation_start", "max_observation_end"))
  
  # format estimate column
  formatEstimates <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "mean (SD)" = "<mean> (<sd>)",
    "[Q25 - Q75]" = "[<q25> - <q75>]",
    "range" = "[<min> <max>]",
    "[Q05 - Q95]" = "[<q05> - <q95>]"
  )
  result <- result |>
    visOmopResults::formatEstimateValue(
      decimals = c(integer = 0, numeric = 1, percentage = 0)
    ) |>
    visOmopResults::formatEstimateName(estimateName = formatEstimates) |>
    suppressMessages() |>
    visOmopResults::formatHeader(header = header) |>
    dplyr::select(!dplyr::any_of(c("estimate_type", hide)))
  if (length(group) > 1) {
    id <- paste0(group, collapse = "; ")
    result <- result |>
      tidyr::unite(col = !!id, dplyr::all_of(group), sep = "; ", remove = TRUE)
    group <- id
  }
  result <- result |>
    visOmopResults::formatTable(groupColumn = group)
  return(result)
}
server <- function(input, output, session) {
  source("background.R")
  # download raw data -----
  output$download_raw <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
     omopgenerics::exportSummarisedResult(data, fileName = file)
    }
  )
  
  shiny::observe({
    
    res <- data |>
      visOmopResults::splitAll()
    
    update_input_choices(session, "summarise_benchmark_grouping_cdm_name", base::unique(res$cdm_name))
    
    update_input_choices(session, "summarise_benchmark_grouping_result_type", attr(data, "settings")$result_type)
    update_input_choices(session, "summarise_benchmark_grouping_task", base::unique(res$task))
    update_input_choices(session, "summarise_benchmark_grouping_iteration", base::unique(res$iteration))
    update_input_choices(session, "summarise_benchmark_grouping_dbms", base::unique(res$dbms))
    #update_input_choices(session, "summarise_benchmark_estimate_name", base::unique(res$estimate_name))
    
  })

  getTidyDataSummariseBenchmark <- shiny::reactive({
    
   res <- filter_data(data, input)
  
   res <- res |>
     omopgenerics::addSettings() |>
     omopgenerics::splitAll() |>
     dplyr::select(!c("result_id"))
  
    # columns to eliminate
    colsEliminate <- colnames(res)
    colsEliminate <- colsEliminate[!colsEliminate %in% c(
      input$summarise_benchmark_tidy_columns, "variable_name", "variable_level",
      "estimate_name", "estimate_type", "estimate_value","min_observation_start", "max_observation_end"
    )]
    
    # pivot
    pivot <- input$summarise_benchmark_tidy_pivot
    if (pivot != "none") {
      vars <- switch(pivot,
                     "estimates" = "estimate_name",
                     "estimates and variables" = c("variable_name", "variable_level", "estimate_name")
      )
      res <- res |>
        visOmopResults::pivotEstimates(pivotEstimatesBy = vars)
    }
    
    res |>
      dplyr::select(!dplyr::all_of(colsEliminate))
  })
  output$summarise_benchmark_tidy <- DT::renderDT({
    DT::datatable(
      getTidyDataSummariseBenchmark(),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$summarise_benchmark_tidy_download <- shiny::downloadHandler(
    filename = "tidy_summarise_benchmark.csv",
    content = function(file) {
      getTidyDataSummariseBenchmark() |>
        readr::write_csv(file = file)
    }
  )
  ## output summarise_general_benchmark -----
  ## output 0 -----
  createOutput0 <- shiny::reactive({
    
    result <- filter_data(data, input) 


   simpleTable(
      result,
      header = input$summarise_benchmark_gt_0_header,
      group = input$summarise_benchmark_gt_0_group,
      hide = input$summarise_benchmark_gt_0_hide
    )
  })
  output$summarise_benchmark_gt_0 <- gt::render_gt({
    createOutput0()
  })
  output$summarise_benchmark_gt_0_download <- shiny::downloadHandler(
    filename = paste0("output_gt_summarise_benchmark.", input$summarise_benchmark_gt_0_download_type),
    content = function(file) {
      obj <- createOutput0()
      gt::gtsave(data = obj, filename = file)
    }
  )
  
  
createOutput1 <- shiny::reactive({
  
  result <- filter_data(data, input) 
    

  if (input$summarise_benchmark_ggplot2_1_plotType == "barplot") {
    plot<-visOmopResults::barPlot(result, x = "task", y = input$summarise_benchmark_estimate_name,
                            facet = input$summarise_benchmark_ggplot2_1_facet,
                            colour = input$summarise_benchmark_ggplot2_1_colour) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10))
   
    if (input$log_scale_y_ggplot2_1) {
      plot <- plot + ggplot2::scale_y_log10()
    }
    
    plot
      
  } else if (input$summarise_benchmark_ggplot2_1_plotType == "line") {
    plot<-visOmopResults::scatterPlot(result, x = "task", y = input$summarise_benchmark_estimate_name,
                                facet = input$summarise_benchmark_ggplot2_1_facet,
                                colour = input$summarise_benchmark_ggplot2_1_colour,
                                line = TRUE, point = TRUE, ribbon = FALSE) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10) )
    if (input$log_scale_x_ggplot2_1) {
      plot <- plot + ggplot2::scale_x_log10()
    }
    
    if (input$log_scale_y_ggplot2_1) {
      plot <- plot + ggplot2::scale_y_log10()
    }
    plot
  } else {
    NULL  # Return NULL if plot type is invalid to avoid errors
  }
  
})


output$summarise_benchmark_ggplot2_1 <- shiny::renderPlot({
  createOutput1()
})
output$summarise_benchmark_ggplot2_1_download <- shiny::downloadHandler(
  filename = paste0("output_ggplot2_summarise_benchmark", ".png"),
  content = function(file) {
    obj <- createOutput1()
    ggplot2::ggsave(
      filename = file,
      plot = obj,
      width = as.numeric(input$summarise_benchmark_ggplot2_1_download_width),
      height = as.numeric(input$summarise_benchmark_ggplot2_1_download_height),
      units = input$summarise_general_ggplot2_1_download_units,
      dpi = as.numeric(input$summarise_benchmark_ggplot2_1_download_dpi)
    )
  }
)

createOutput5 <- shiny::reactive({
  result <- filter_data(data, input) |>
    visOmopResults::splitAdditional() |>
    dplyr::mutate(person_n = as.numeric(.data$person_n)) |>
    visOmopResults::splitGroup() |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    visOmopResults::pivotEstimates()
  
  plot<-ggplot2::ggplot(result, 
                  ggplot2::aes(x = person_n, 
                               y = .data[[input$summarise_benchmark_estimate_name]], 
                               color = cdm_name)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ task) +
    ggplot2::labs(
      x = "Person Count",
      y = input$summarise_benchmark_estimate_name,
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10)
    )
  if (input$log_scale_x_ggplot2_5) {
    plot <- plot + ggplot2::scale_x_log10()
  }
 
  if (input$log_scale_y_ggplot2_5) {
    plot <- plot + ggplot2::scale_y_log10()
  }

  plot
})

output$summarise_benchmark_ggplot2_5 <- shiny::renderPlot({
  createOutput5()
})

output$summarise_benchmark_ggplot2_5_download <- shiny::downloadHandler(
  filename = function() {
    paste0("output_ggplot2_size__benchmark", ".png")
  },
  content = function(file) {
    ggplot2::ggsave(
      filename = file,
      plot = createOutput5(),
      width = as.numeric(input$summarise_benchmark_ggplot2_5_download_width),
      height = as.numeric(input$summarise_benchmark_ggplot2_5_download_height),
      units = input$summarise_benchmark_ggplot2_5_download_units,
      dpi = as.numeric(input$summarise_benchmark_ggplot2_5_download_dpi)
    )
  }
)


}
