
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

  res <- data |>
    visOmopResults::splitAll() 

    
  res <- res |> omopgenerics::addSettings(settingsColumn = "result_type" ) |>
    dplyr::mutate(result_type = dplyr::if_else(.data$result_type == "summarise_general_benchmark", "General benchmark", 
                                               dplyr::if_else(.data$result_type == "summarise_incidence_prevalence_benchmark", "IncidencePrevalence benchmark",
                                                              dplyr::if_else(.data$result_type == "summarise_cdm_connector_benchmark", "CDMConnector benchmark",
                                                                             dplyr::if_else(.data$result_type =="summarise_cohort_characteristics_benchmark","CohortCharacteristics benchmark", .data$result_type))))) 
  
 
  
  res <- res |> 
    dplyr::filter(.data$cdm_name %in% input$summarise_benchmark_grouping_cdm_name,
                  .data$task %in% input$summarise_benchmark_grouping_task,
                  #.data$iteration %in% input$summarise_benchmark_grouping_iteration,
                  .data$dbms %in% input$summarise_benchmark_grouping_dbms,
                  .data$estimate_name %in% input$summarise_benchmark_estimate_name,
                  .data$result_type %in% input$summarise_benchmark_grouping_result_type )
  

  res <- res|>  visOmopResults::uniteStrata("iteration")|>
    (\(df) visOmopResults::uniteAdditional(df, intersect(c("person_n", "dbms", "min_observation_start", "max_observation_end", "result_type"), colnames(df))))()|>
    visOmopResults::uniteGroup("task") 
  if (nrow(res) == 0){
    return(omopgenerics::emptySummarisedResult())
  } 
 
  return(res)

}
simpleTable <- function(result) {
  
  header <- c("dbms", "person_n", "cdm_name")
  group <- "result_type"
  hide <- c("variable_name",	"variable_level", "iteration", "result_id", "min_observation_start", "max_observation_end", "estimate_type")
  if (nrow(result) == 0) {
    return(gt::gt(dplyr::tibble()))
  }
  
  
  # format estimate column
  formatEstimates <- c(
    "Time (s)" = "<time_seconds>",
    "Time (min)" = "<time_minutes>"
  )
  
  
  tab <- visOmopResults::visOmopTable(result = result, 
                               estimateName = formatEstimates,
                               header = header, 
                               hide = hide, 
                               groupColumn = group)
  
  return(tab)
}
server <- function(input, output, session) {
  #bslib::bs_themer()
  # download raw data -----
  output$download_raw <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
     omopgenerics::exportSummarisedResult(data, fileName = file)
    }
  )
  
  shiny::observe({
    
    res <- data |>
      omopgenerics::addSettings(settingsColumn = "result_type" ) |>
      dplyr::mutate(result_type = dplyr::if_else(.data$result_type == "summarise_general_benchmark", "General benchmark", 
                                                 dplyr::if_else(.data$result_type == "summarise_incidence_prevalence_benchmark", "IncidencePrevalence benchmark",
                                                                dplyr::if_else(.data$result_type == "summarise_cdm_connector_benchmark", "CDMConnector benchmark",
                                                                               dplyr::if_else(.data$result_type =="summarise_cohort_characteristics_benchmark","CohortCharacteristics benchmark", .data$result_type))))) |>
    
      visOmopResults::splitAll()
    
    update_input_choices(session, "summarise_benchmark_grouping_cdm_name", base::unique(res$cdm_name))
    
    update_input_choices(session, "summarise_benchmark_grouping_result_type", unique(res$result_type))
    update_input_choices(session, "summarise_benchmark_grouping_task", base::unique(res$task))
    update_input_choices(session, "summarise_benchmark_grouping_iteration", base::unique(res$iteration))
    update_input_choices(session, "summarise_benchmark_grouping_dbms", base::unique(res$dbms))
    #update_input_choices(session, "summarise_benchmark_estimate_name", base::unique(res$estimate_name))
    
  })


  ## output summarise_general_benchmark -----
  ## output 0 -----
  createOutput0 <- shiny::reactive({
    
    result <- filter_data(data, input)

  
   simpleTable(
      result
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
  
  result <- filter_data(data, input) |>
   omopgenerics::splitAll()|>
    omopgenerics::pivotEstimates()
    

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
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 8) )
  
    if (input$log_scale_y_ggplot2_1) {
      plot <- plot + ggplot2::scale_y_log10()
    }
    plot + ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 6))
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
    ) 
  if (input$log_scale_x_ggplot2_5) {
    plot <- plot + ggplot2::scale_x_log10()
  }
 
  if (input$log_scale_y_ggplot2_5) {
    plot <- plot + ggplot2::scale_y_log10()
  }

  plot + ggplot2::theme_minimal(base_size = 14) +  # base text size
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 13),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 9),
      axis.text.y = ggplot2::element_text(size = 9),
      strip.text = ggplot2::element_text(size = 9),
      plot.title = ggplot2::element_text(size = 16, face = "bold")
    )
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
