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
    icon = shiny::icon("file-alt")
    # OmopViewer::cardSummary(data)
  ),
  bslib::nav_panel(
    title = "Timings",
    icon = shiny::icon("clipboard-list"),
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "Tidy",
        bslib::card(
          full_screen = TRUE,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              shiny::selectizeInput(
                inputId = "timings_tidy_columns",
                label = "Columns",
                choices = colnames(data),
                selected = colnames(data),
                multiple = TRUE,
                options = list(plugins = "remove_button")
              ),
              position = "right"
            ),
            DT::dataTableOutput("timings_tidy")
          )
        )
      ), 
      bslib::nav_panel(
        title = "Plot",
        icon = shiny::icon("clipboard-list"),
        bslib::card(
          full_screen = TRUE,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              shiny::selectInput(
                inputId = "time_variable",
                label = "Time taken",
                choices = c("time_taken_secs", "time_taken_mins"),
                selected = "time_taken_secs"
              ),
              shiny::selectInput(
                inputId = "iteration_color",
                label = "Iteration",
                choices = unique(data$iteration),  # Assuming iteration is a column in your data
                selected = unique(data$iteration)[1]
              ),
              shiny::radioButtons(
                inputId = "plot_type",
                label = "Select Plot Type",
                choices = c("Bar Plot" = "bar", "Line Plot" = "line"),
                selected = "bar"
              )
            ),
            shiny::plotOutput("timing_plot")
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
  bslib::nav_item(
    bslib::input_dark_mode(id = "dark_mode", mode = "light")
  )
)
