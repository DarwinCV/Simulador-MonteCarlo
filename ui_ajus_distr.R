wellPanel(fluidRow(align="center",
                   column(12,h3('PARAMETROS'))
),
fluidRow(column(3,
                uiOutput("ui_pdv") %>% shinycssloaders::withSpinner(type = 7, color = "#6BC9FA")
),
column(3, 
       numericInput(inputId = "dias", "Dias de stock", min = 7, max = 30,value = 7),
       materialSwitch(
         inputId = "load_sug",
         label = "Cargar suguerido", 
         status = "primary",
         right = TRUE
       )),
column(5, div(id = "load_data",
              fluidRow(column(8, radioButtons("type","Elija el tipo de archivo",
                                              choices = c('CSV'=1,'EXCEL'=2),
                                              selected = 2,
                                              inline = TRUE),
                              fileInput('inFile', "Subir archivo",
                                        accept = c(
                                          'text/csv',
                                          'text/comma-separated-values,text/plain',
                                          'text/tab-separated-values',
                                          '.csv',
                                          'xls',
                                          '.xlsx'
                                        )),
                              div(downloadLink('plantilla', 'Descargar plantilla'))),
                       column(4, br(), br(), br(), 
                              br(), actionButton('reset', 'Resetear archivo'))
              )
)
)
),
fluidRow(column(3, uiOutput("ui_calcular")),
         column(3, actionBttn( inputId = "borrar",
                               label = "Borrar",
                               block = TRUE,
                               size = "sm",
                               color = "success",
                               style = "simple")),
         column(6)
)
)