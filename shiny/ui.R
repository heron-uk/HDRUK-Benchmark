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
    title = "Summary",
    icon = shiny::icon("file-alt"),
    OmopViewer::cardSummary(data)
  ),
  bslib::nav_panel(
    title = "Summarise general benchmark",
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
              inputId = "summarise_general_benchmark_grouping_cdm_name",
              label = "Cdm name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_general_benchmark_grouping_task",
              label = "Task",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_general_benchmark_grouping_iteration",
              label = "Iteration",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_general_benchmark_grouping_dbms",
              label = "Dbms",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_general_benchmark_grouping_person_n",
              label = "Person n",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            )
          ),
          bslib::accordion_panel(
            title = "Variables",
            shiny::selectizeInput(
              inputId = "summarise_general_benchmark_variable_name",
              label = "Variable name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            )
          ),
          bslib::accordion_panel(
            title = "Estimates",
            shiny::selectizeInput(
              inputId = "summarise_general_benchmark_estimate_name",
              label = "Estimate name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
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
                shiny::downloadButton(outputId = "summarise_general_benchmark_tidy_download", label = "Download csv")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::selectizeInput(
                  inputId = "summarise_general_benchmark_tidy_columns",
                  label = "Columns",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(plugins = "remove_button")
                ),
                shiny::radioButtons(
                  inputId = "summarise_general_benchmark_tidy_pivot",
                  label = "Pivot estimates/variables",
                  choices = c("none", "estimates", "estimates and variables"),
                  selected = "none"
                ),
                position = "right"
              ),
              DT::dataTableOutput("summarise_general_benchmark_tidy")
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
                  inputId = "summarise_general_benchmark_gt_0_download_type",
                  label = "File type",
                  selected = "docx",
                  choices = c("docx", "png", "pdf", "html"),
                  multiple = FALSE
                ),
                shiny::downloadButton(outputId = "summarise_general_benchmark_gt_0_download", label = "Download")
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
                    input_id = "summarise_general_benchmark_gt_0_none"
                  ),
                  sortable::add_rank_list(
                    text = "header",
                    labels = "cdm_name",
                    input_id = "summarise_general_benchmark_gt_0_header"
                  ),
                  sortable::add_rank_list(
                    text = "group",
                    labels = character(),
                    input_id = "summarise_general_benchmark_gt_0_group"
                  ),
                  sortable::add_rank_list(
                    text = "hide",
                    labels = character(),
                    input_id = "summarise_general_benchmark_gt_0_hide"
                  )
                ),
                position = "right"
              ),
              gt::gt_output("summarise_general_benchmark_gt_0")
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
                  inputId = "summarise_general_benchmark_ggplot2_1_download_width",
                  label = "Width",
                  value = 15
                ),
                shiny::numericInput(
                  inputId = "summarise_general_benchmark_ggplot2_1_download_height",
                  label = "Height",
                  value = 10
                ),
                shiny::selectizeInput(
                  inputId = "summarise_general_benchmark_ggplot2_1_download_units",
                  label = "Units",
                  selected = "cm",
                  choices = c("px", "cm", "inch"),
                  multiple = FALSE
                ),
                shiny::numericInput(
                  inputId = "summarise_general_benchmark_ggplot2_1_download_dpi",
                  label = "dpi",
                  value = 300
                ),
                shiny::downloadButton(outputId = "summarise_general_benchmark_ggplot2_1_download", label = "Download")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::selectizeInput(
                  inputId = "summarise_general_benchmark_ggplot2_1_colour",
                  label = "Colour",
                  selected = "cdm_name",
                  multiple = TRUE,
                  choices = c("cdm_name", "iteration", "dbms"),
                  options = list(plugins = "remove_button")
                ),
                shiny::selectizeInput(
                  inputId = "summarise_general_benchmark_ggplot2_1_facet",
                  label = "Facet",
                  selected = "cdm_name",
                  multiple = TRUE,
                  choices = c("cdm_name", "iteration", "dbms"),
                  options = list(plugins = "remove_button")
                ),
                shiny::selectizeInput(
                  inputId = "summarise_general_benchmark_ggplot2_1_plotType",
                  label = "Plot Type",
                  selected = "barplot",
                  choices = c("barplot", "line"),
                  options = list(plugins = "remove_button")
                ),
                position = "right"
              ),
              shiny::plotOutput("summarise_general_benchmark_ggplot2_1")
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
                  inputId = "summarise_general_benchmark_ggplot2_5_download_width",
                  label = "Width",
                  value = 15
                ),
                shiny::numericInput(
                  inputId = "summarise_general_benchmark_ggplot2_5_download_height",
                  label = "Height",
                  value = 10
                ),
                shiny::selectizeInput(
                  inputId = "summarise_general_benchmark_ggplot2_5_download_units",
                  label = "Units",
                  selected = "cm",
                  choices = c("px", "cm", "inch"),
                  multiple = FALSE
                ),
                shiny::numericInput(
                  inputId = "summarise_general_benchmark_ggplot2_5_download_dpi",
                  label = "dpi",
                  value = 300
                ),
                shiny::downloadButton(outputId = "summarise_general_benchmark_ggplot2_5_download", label = "Download")
              ),
              class = "text-end"
            ),
            shiny::plotOutput("summarise_general_benchmark_ggplot2_5")
          )
        )
      )
    )
  ),
  bslib::nav_panel(
    title = "Summarise incidence prevalence benchmark",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        bslib::accordion(
          bslib::accordion_panel(
            title = "Information",
            icon = shiny::icon("info"),
            shiny::p("")
          ),
          bslib::accordion_panel(
            title = "grouping",
            shiny::selectizeInput(
              inputId = "summarise_incidence_prevalence_benchmark_grouping_cdm_name",
              label = "Cdm name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_incidence_prevalence_benchmark_grouping_task",
              label = "Task",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_incidence_prevalence_benchmark_grouping_iteration",
              label = "Iteration",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_incidence_prevalence_benchmark_grouping_dbms",
              label = "Dbms",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_incidence_prevalence_benchmark_grouping_person_n",
              label = "Person n",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_incidence_prevalence_benchmark_grouping_min_observation_start",
              label = "Min observation start",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_incidence_prevalence_benchmark_grouping_max_observation_end",
              label = "Max observation end",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            )
          ),
          bslib::accordion_panel(
            title = "Variables",
            shiny::selectizeInput(
              inputId = "summarise_incidence_prevalence_benchmark_variable_name",
              label = "Variable name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            )
          ),
          bslib::accordion_panel(
            title = "Estimates",
            shiny::selectizeInput(
              inputId = "summarise_incidence_prevalence_benchmark_estimate_name",
              label = "Estimate name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
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
                shiny::downloadButton(outputId = "summarise_incidence_prevalence_benchmark_tidy_download", label = "Download csv")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::selectizeInput(
                  inputId = "summarise_incidence_prevalence_benchmark_tidy_columns",
                  label = "Columns",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(plugins = "remove_button")
                ),
                shiny::radioButtons(
                  inputId = "summarise_incidence_prevalence_benchmark_tidy_pivot",
                  label = "Pivot estimates/variables",
                  choices = c("none", "estimates", "estimates and variables"),
                  selected = "none"
                ),
                position = "right"
              ),
              DT::dataTableOutput("summarise_incidence_prevalence_benchmark_tidy")
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
                  inputId = "summarise_incidence_prevalence_benchmark_gt_0_download_type",
                  label = "File type",
                  selected = "docx",
                  choices = c("docx", "png", "pdf", "html"),
                  multiple = FALSE
                ),
                shiny::downloadButton(outputId = "summarise_incidence_prevalence_benchmark_gt_0_download", label = "Download")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                sortable::bucket_list(
                  header = NULL,
                  sortable::add_rank_list(
                    text = "none",
                    labels = c("task", "iteration", "dbms", "person_n", "min_observation_start", "max_observation_end", "variable_name", "variable_level", "estimate_name"),
                    input_id = "summarise_incidence_prevalence_benchmark_gt_0_none"
                  ),
                  sortable::add_rank_list(
                    text = "header",
                    labels = "cdm_name",
                    input_id = "summarise_incidence_prevalence_benchmark_gt_0_header"
                  ),
                  sortable::add_rank_list(
                    text = "group",
                    labels = character(),
                    input_id = "summarise_incidence_prevalence_benchmark_gt_0_group"
                  ),
                  sortable::add_rank_list(
                    text = "hide",
                    labels = character(),
                    input_id = "summarise_incidence_prevalence_benchmark_gt_0_hide"
                  )
                ),
                position = "right"
              ),
              gt::gt_output("summarise_incidence_prevalence_benchmark_gt_0")
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
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_1_download_width",
                  label = "Width",
                  value = 15
                ),
                shiny::numericInput(
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_1_download_height",
                  label = "Height",
                  value = 10
                ),
                shiny::selectizeInput(
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_1_download_units",
                  label = "Units",
                  selected = "cm",
                  choices = c("px", "cm", "inch"),
                  multiple = FALSE
                ),
                shiny::numericInput(
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_1_download_dpi",
                  label = "dpi",
                  value = 300
                ),
                shiny::downloadButton(outputId = "summarise_incidence_prevalence_benchmark_ggplot2_1_download", label = "Download")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::selectizeInput(
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_1_colour",
                  label = "Colour",
                  selected = "cdm_name",
                  multiple = TRUE,
                  choices = c("cdm_name", "iteration", "dbms"),
                  options = list(plugins = "remove_button")
                ),
                shiny::selectizeInput(
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_1_facet",
                  label = "Facet",
                  selected = "cdm_name",
                  multiple = TRUE,
                  choices = c("cdm_name", "iteration", "dbms"),
                  options = list(plugins = "remove_button")
                ),
                shiny::selectizeInput(
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_1_plotType",
                  label = "plotType",
                  selected = "barplot",
                  multiple = TRUE,
                  choices = c("barplot", "line"),
                  options = list(plugins = "remove_button")
                ),
                position = "right"
              ),
              shiny::plotOutput("summarise_incidence_prevalence_benchmark_ggplot2_1")
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
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_6_download_width",
                  label = "Width",
                  value = 15
                ),
                shiny::numericInput(
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_6_download_height",
                  label = "Height",
                  value = 10
                ),
                shiny::selectizeInput(
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_6_download_units",
                  label = "Units",
                  selected = "cm",
                  choices = c("px", "cm", "inch"),
                  multiple = FALSE
                ),
                shiny::numericInput(
                  inputId = "summarise_incidence_prevalence_benchmark_ggplot2_6_download_dpi",
                  label = "dpi",
                  value = 300
                ),
                shiny::downloadButton(outputId = "summarise_incidence_prevalence_benchmark_ggplot2_6_download", label = "Download")
              ),
              class = "text-end"
            ),
            shiny::plotOutput("summarise_incidence_prevalence_benchmark_ggplot2_6")
          )
        )

      )
    )
  ),
  bslib::nav_panel(
    title = "Summarise cdm connector benchmark",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        bslib::accordion(
          bslib::accordion_panel(
            title = "Information",
            icon = shiny::icon("info"),
            shiny::p("")
          ),
          bslib::accordion_panel(
            title = "grouping",
            shiny::selectizeInput(
              inputId = "summarise_cdm_connector_benchmark_grouping_cdm_name",
              label = "Cdm name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_cdm_connector_benchmark_grouping_task",
              label = "Task",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_cdm_connector_benchmark_grouping_iteration",
              label = "Iteration",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_cdm_connector_benchmark_grouping_dbms",
              label = "Dbms",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            ),
            shiny::selectizeInput(
              inputId = "summarise_cdm_connector_benchmark_grouping_person_n",
              label = "Person n",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            )
          ),
          bslib::accordion_panel(
            title = "Variables",
            shiny::selectizeInput(
              inputId = "summarise_cdm_connector_benchmark_variable_name",
              label = "Variable name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(plugins = "remove_button")
            )
          ),
          bslib::accordion_panel(
            title = "Estimates",
            shiny::selectizeInput(
              inputId = "summarise_cdm_connector_benchmark_estimate_name",
              label = "Estimate name",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
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
                shiny::downloadButton(outputId = "summarise_cdm_connector_benchmark_tidy_download", label = "Download csv")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::selectizeInput(
                  inputId = "summarise_cdm_connector_benchmark_tidy_columns",
                  label = "Columns",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(plugins = "remove_button")
                ),
                shiny::radioButtons(
                  inputId = "summarise_cdm_connector_benchmark_tidy_pivot",
                  label = "Pivot estimates/variables",
                  choices = c("none", "estimates", "estimates and variables"),
                  selected = "none"
                ),
                position = "right"
              ),
              DT::dataTableOutput("summarise_cdm_connector_benchmark_tidy")
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
                  inputId = "summarise_cdm_connector_benchmark_gt_0_download_type",
                  label = "File type",
                  selected = "docx",
                  choices = c("docx", "png", "pdf", "html"),
                  multiple = FALSE
                ),
                shiny::downloadButton(outputId = "summarise_cdm_connector_benchmark_gt_0_download", label = "Download")
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
                    input_id = "summarise_cdm_connector_benchmark_gt_0_none"
                  ),
                  sortable::add_rank_list(
                    text = "header",
                    labels = "cdm_name",
                    input_id = "summarise_cdm_connector_benchmark_gt_0_header"
                  ),
                  sortable::add_rank_list(
                    text = "group",
                    labels = character(),
                    input_id = "summarise_cdm_connector_benchmark_gt_0_group"
                  ),
                  sortable::add_rank_list(
                    text = "hide",
                    labels = character(),
                    input_id = "summarise_cdm_connector_benchmark_gt_0_hide"
                  )
                ),
                position = "right"
              ),
              gt::gt_output("summarise_cdm_connector_benchmark_gt_0")
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
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_1_download_width",
                  label = "Width",
                  value = 15
                ),
                shiny::numericInput(
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_1_download_height",
                  label = "Height",
                  value = 10
                ),
                shiny::selectizeInput(
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_1_download_units",
                  label = "Units",
                  selected = "cm",
                  choices = c("px", "cm", "inch"),
                  multiple = FALSE
                ),
                shiny::numericInput(
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_1_download_dpi",
                  label = "dpi",
                  value = 300
                ),
                shiny::downloadButton(outputId = "summarise_cdm_connector_benchmark_ggplot2_1_download", label = "Download")
              ),
              class = "text-end"
            ),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                shiny::selectizeInput(
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_1_colour",
                  label = "Colour",
                  selected = "cdm_name",
                  multiple = TRUE,
                  choices = c("cdm_name", "iteration", "dbms"),
                  options = list(plugins = "remove_button")
                ),
                shiny::selectizeInput(
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_1_facet",
                  label = "Facet",
                  selected = "cdm_name",
                  multiple = TRUE,
                  choices = c("cdm_name", "iteration", "dbms"),
                  options = list(plugins = "remove_button")
                ),
                shiny::selectizeInput(
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_1_plotType",
                  label = "plotType",
                  selected = "barplot",
                  multiple = ,
                  choices = c("barplot", "line"),
                  options = list(plugins = "remove_button")
                ),
                position = "right"
              ),
              shiny::plotOutput("summarise_cdm_connector_benchmark_ggplot2_1")
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
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_7_download_width",
                  label = "Width",
                  value = 15
                ),
                shiny::numericInput(
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_7_download_height",
                  label = "Height",
                  value = 10
                ),
                shiny::selectizeInput(
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_7_download_units",
                  label = "Units",
                  selected = "cm",
                  choices = c("px", "cm", "inch"),
                  multiple = FALSE
                ),
                shiny::numericInput(
                  inputId = "summarise_cdm_connector_benchmark_ggplot2_7_download_dpi",
                  label = "dpi",
                  value = 300
                ),
                shiny::downloadButton(outputId = "summarise_cdm_connector_benchmark_ggplot2_7_download", label = "Download")
              ),
              class = "text-end"
            ),
            shiny::plotOutput("summarise_cdm_connector_benchmark_ggplot2_7")
          )
        )
      )
    )
  ),
  bslib::nav_spacer(),
  bslib::nav_item(
    bslib::popover(
      shiny::icon("download"),
      shiny::downloadButton(
        outputId = "download_raw",
        label = "Download raw data",
        icon = shiny::icon("download")
      )
    )
  ),
  bslib::nav_item(
    bslib::popover(
      shiny::icon("circle-info"),
      shiny::tags$img(
        src = "hdruk_logo.svg",
        class = "logo-img",
        alt = "Logo",
        height = "auto",
        width = "30%",
        style = "float:right"
      ),
      "This shiny app was generated with ",
      shiny::a(
        "OmopViewer",
        href = "https://github.com/OHDSI/OmopViewer",
        target = "_blank"
      ),
      shiny::strong("v0.1.0")
    )
  ),
  bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light"))
)
