ui <- bslib::page_navbar(
  title = shiny::tags$span(
    shiny::tags$img(
      src = "hdruk_logo.svg",
      width = "auto",
      height = "46px",
      class = "me-3",
      alt = "logo"
    ),
    ""
  ),
  bslib::nav_panel(
    title = "Background",
    icon = shiny::icon("disease"),
    OmopViewer::cardFromMd("background.md")
  ),
  bslib::nav_panel(
    title = "Benchmark",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        bslib::accordion(
          bslib::accordion_panel(
            title = "Information",
            icon = shiny::icon("info"),
            shiny::p("")
          ),
          bslib::accordion_panel(
            title = "Grouping",
            shiny::selectizeInput(
              inputId = "summarise_benchmark_grouping_cdm_name",
              label = "Cdm name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId =  "summarise_benchmark_grouping_result_type",
              label = "Result type",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_benchmark_grouping_task",
              label = "Task",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_benchmark_grouping_iteration",
              label = "Iteration",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_benchmark_grouping_dbms",
              label = "Dbms",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            )
          ),
          bslib::accordion_panel(
            title = "Estimates",
            shiny::selectizeInput(
              inputId = "summarise_benchmark_estimate_name",
              label = "Estimate name",
              choices = c("time_minutes","time_seconds"),
              selected = "time_seconds",
              multiple = FALSE,
              options = list(plugins = "remove_button")
            )
          )
        )
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = "Tidy",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              bslib::popover(
                shiny::icon("download"),
                shiny::downloadButton(outputId = "summarise_benchmark_tidy_download", label = "Download csv")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::selectizeInput(
                  inputId = "summarise_benchmark_tidy_columns",
                  label = "Columns",
                  choices = c("cdm_name", "task", "iteration", "dbms", "person_n"),
                  selected = c("cdm_name", "task", "iteration", "dbms", "person_n"),
                  multiple = TRUE,
                  options = list(plugins = "remove_button")
                ),
                shiny::radioButtons(
                  inputId = "summarise_benchmark_tidy_pivot",
                  label = "Pivot estimates/variables",
                  choices = c("none", "estimates", "estimates and variables"),
                  selected = "none"
                ),
                position = "right"
              ),
              DT::dataTableOutput("summarise_benchmark_tidy")
            )
          )
        ),
        bslib::nav_panel(
          title = "Formatted",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              bslib::popover(
                shiny::icon("download"),
                shiny::selectizeInput(
                  inputId = "summarise_benchmark_gt_0_download_type",
                  label = "File type",
                  selected = "docx",
                  choices = c("docx", "png", "pdf", "html"),
                  multiple = FALSE
                ),
                shiny::downloadButton(outputId = "summarise_benchmark_gt_0_download", label = "Download")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                sortable::bucket_list(
                  header = NULL,
                  sortable::add_rank_list(
                    text = "none",
                    labels = c("task", "iteration", "dbms", "person_n", "variable_name", "variable_level", "estimate_name"),
                    input_id = "summarise_benchmark_gt_0_none"
                  ),
                  sortable::add_rank_list(
                    text = "header",
                    labels = "cdm_name",
                    input_id = "summarise_benchmark_gt_0_header"
                  ),
                  sortable::add_rank_list(
                    text = "group",
                    labels = character(),
                    input_id = "summarise_benchmark_gt_0_group"
                  ),
                  sortable::add_rank_list(
                    text = "hide",
                    labels = character(),
                    input_id = "summarise_benchmark_gt_0_hide"
                  )
                ),
                position = "right"
              ),
              gt::gt_output("summarise_benchmark_gt_0")
            )
          )
        ),
        bslib::nav_panel(
          title = "Plot",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              bslib::popover(
                shiny::icon("download"),
                shiny::numericInput(
                  inputId = "summarise_benchmark_ggplot2_1_download_width",
                  label = "Width",
                  value = 15
                ),
                shiny::numericInput(
                  inputId = "summarise_benchmark_ggplot2_1_download_height",
                  label = "Height",
                  value = 10
                ),
                shiny::selectizeInput(
                  inputId = "summarise_benchmark_ggplot2_1_download_units",
                  label = "Units",
                  selected = "cm",
                  choices = c("px", "cm", "inch"),
                  multiple = FALSE
                ),
                shiny::numericInput(
                  inputId = "summarise_benchmark_ggplot2_1_download_dpi",
                  label = "dpi",
                  value = 300
                ),
                shiny::downloadButton(outputId = "summarise_benchmark_ggplot2_1_download", label = "Download")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::selectizeInput(
                  inputId = "summarise_benchmark_ggplot2_1_colour",
                  label = "Colour",
                  selected = "cdm_name",
                  multiple = TRUE,
                  choices = c("cdm_name", "iteration", "dbms"),
                  options = list(plugins = "remove_button")
                ),
                shiny::selectizeInput(
                  inputId = "summarise_benchmark_ggplot2_1_facet",
                  label = "Facet",
                  selected = "cdm_name",
                  multiple = TRUE,
                  choices = c("cdm_name", "iteration", "dbms"),
                  options = list(plugins = "remove_button")
                ),
                shiny::selectizeInput(
                  inputId = "summarise_benchmark_ggplot2_1_plotType",
                  label = "Plot Type",
                  selected = "barplot",
                  choices = c("barplot", "line"),
                  options = list(plugins = "remove_button")
                ),
                position = "right"
              ),
              shiny::plotOutput("summarise_benchmark_ggplot2_1")
            )
          )
        ),
        bslib::nav_panel(
          title = "Plot by denominator size",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              bslib::popover(
                shiny::icon("download"),
                shiny::numericInput(
                  inputId = "summarise_benchmark_ggplot2_5_download_width",
                  label = "Width",
                  value = 15
                ),
                shiny::numericInput(
                  inputId = "summarise_benchmark_ggplot2_5_download_height",
                  label = "Height",
                  value = 10
                ),
                shiny::selectizeInput(
                  inputId = "summarise_benchmark_ggplot2_5_download_units",
                  label = "Units",
                  selected = "cm",
                  choices = c("px", "cm", "inch"),
                  multiple = FALSE
                ),
                shiny::numericInput(
                  inputId = "summarise_benchmark_ggplot2_5_download_dpi",
                  label = "dpi",
                  value = 300
                ),
                shiny::downloadButton(outputId = "summarise_benchmark_ggplot2_5_download", label = "Download")
              ),
              class = "text-end"
            ),
            shiny::plotOutput("summarise_benchmark_ggplot2_5")
          )
        )
      )
    )
  )
)
