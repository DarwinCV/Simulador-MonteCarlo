fluidPage(
  introjsUI(),
  theme = shinytheme("spacelab"),
  includeCSS("ractuary-style.css"),
  fluidRow(
    br(),
    headerPanel(
      tags$div( class = "title",
                tags$h1("Simulador de perdidas", class = "titre"),
                tags$h2("Modelo Colectivo de Riesgo", class = "soustitre"),
                tags$br(),
                tags$span(icon("codepen", lib = "font-awesome"), class = "main-icon"),
                actionButton("tour", "Tour", class = "btn btn-info pull-right"),
                actionButton("creditos", "Creditos", class = "btn btn-info pull-right")
                ), 
      windowTitle = "Simulacion de perdidas"
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 3, 
      wellPanel(
        div(
          id = "tour_1",
          actionButton(
            "run_freq", 
            "Simular",
            class = "btn btn-info",
            width = "100%"
          )
        ),
        br(),
        br(),
        div(
          id = "tour_3",
          sliderInput(
            inputId = "obs", 
            label = "# de Observaciones", 
            min = 1000, 
            max = 10000, 
            value = 2000, 
            step = 1000, 
            ticks = FALSE
          )
        ),
        br(),
        hr(style="border-color: #000"),
        div(
          id = "tour_4",
          h3("Frecuencia", class = "well-title"),
          selectInput(
            inputId = "freq_dist", 
            label = "Distribución",
            choices = freq_choices
          ),
          h4("Parametros"),
          fluidRow(
            uiOutput("freq_param_boxes") 
          )
        ),
        br(),
        hr(style="border-color: #000"),
        div(
          id = "tour_5",
          h3("Severidad", class = "well-title"),
          selectInput(
            inputId = "sev_dist", 
            label = "Distribución",
            choices = sev_choices
          ),
          h4("Parametros"),
          fluidRow(
            uiOutput("sev_param_boxes") 
          )
        )
      )
    ),
    
    column(
      width = 9,
      fluidRow(
        column(
          width = 2
        ),
        column(
          width = 8,
          wellPanel(
            h3(
              id = "tour_6",
              class = "well-title",
              "Retención"
            ),
            fluidRow(
              column(
                width = 6,
                id = "tour_7",
                numericInput(
                  inputId = "specific_lim", 
                  label = "Por reclamo", 
                  value = NA
                )
              ),
              column(
                width = 6,
                id = "tour_8",
                numericInput(
                  inputId = "agg_lim", 
                  label = "Aggregado (por observación)", 
                  value = NA
                )
              )
            )
          )
        )
      ),
      fluidRow(  column(4, style='padding:2px;', uiOutput("ui_loss")%>% 
                          shinycssloaders::withSpinner(type = 7, color = "#6BC9FA", hide.ui = FALSE) ),
                 column(4, style='padding:2px;', uiOutput("ui_loss_quantil")%>% 
                          shinycssloaders::withSpinner(type = 7, color = "#6BC9FA", hide.ui = FALSE) ),
                 column(4, style='padding:2px;', uiOutput("ui_loss_ced")%>% 
                          shinycssloaders::withSpinner(type = 7, color = "#6BC9FA", hide.ui = FALSE) )  
      ),
      fluidRow(
        column(
          width = 12,
          tabsetPanel(
            tabPanel(
              title = "Histograma",
              br(),
              fluidRow(
                column(
                  id = "tour_2",
                  width = 12,
                  highchartOutput("hist_plot") %>% withSpinner()
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 2
                ),
                column(
                  width = 8,
                  id = "tour_9",
                  sliderInput(
                    inputId = "ci", 
                    label = "Nivel de confianza",
                    value = 0.95,
                    max = 1.0,
                    min = 0.25,
                    step = 0.01,
                    width = '100%'
                  )
                ),
                column(
                  width = 2
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  id = "tour_10",
                  highchartOutput("hist_plot_total") %>% withSpinner()
                )
              ),
              hr(),
              fluidRow(
                column(
                  12,
                  highchartOutput("hist_plot_ceded") %>% withSpinner()
                )
              )
            ),
            tabPanel(
              title = "Tabla de nivel de confianza",
              h3("Download All Claims"),
              p("Cada fila representa una observación de frecuencia / severidad"),
              downloadButton("download_claims", "Download reclamos"),
              hr(),
              DT::dataTableOutput("sorter")
            )
          )
        )
      )
    )
  )
)

