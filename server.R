
function(input, output, session) {
  

# Tour --------------------------------------------------------------------

  source("zz-tour.R", local = TRUE)
  set.seed(1234)
   observeEvent(input$creditos,{
    
    showModal(modalDialog(
      HTML( paste(tags$strong("Licencia MIT"),tags$strong("Copyright (c) 2017 Tychobra"),
                  "Version Modificada por: Darwin Cubi",
                  "Contacto: 0996597844",
                  "Correo: darwin.cubi@gmail.com",sep = '<br/>')),
      easyClose = TRUE,
      footer = NULL
    ))
    
    
  })
  


# Render parametros de distribuciones -------------------------------------

  
  # determinar los parámetros y valores por defecto que pertenecen a la distribución de frecuencia
  freq_param_labels <- reactive({
    switch(input$freq_dist,
      "poisson" = list(params = "lambda", values = 10),
      "binomial" = list(params = c("q", "m"), values = c(0.8, 12)),
      "nbinomial" = list(params = c("r", "B"), values = c(2, 5))
    )
  })
  
  # crear cuadros de entrada para los parámetros de frecuencia
  output$freq_param_boxes <- renderUI({
    lapply(1:length(freq_param_labels()[["params"]]), function(i) {
      column(
        width = 6,
        numericInput(
          inputId = paste0(input$freq_dist, "_", freq_param_labels()[["params"]][i]), 
          label = freq_param_labels()[["params"]][i], 
          value = freq_param_labels()[["values"]][i])
      )
    })
  })

  # determinar los parámetros y valores por defecto que pertenecen a la distribución de severidad
  sev_param_labels <- reactive({
    switch(input$sev_dist,
           "lognormal" = list(params = c("mu", "sigma"), values = c(9.0, 2.0)),
           "pareto" = list(params = c("theta", "alpha"), values = c(100000, 3.0)),
           "exponential" = list(params = "theta", values = 50000),
           "gamma" = list(params = c("theta", "alpha"), values = c(25000, 2)),
           "weibull" = list(params = c("theta", "tau"), values = c(25000, 0.5))
    )
  })
  
  # crear cuadros de entrada para los parámetros de severidad
  output$sev_param_boxes <- renderUI({
    lapply(1:length(sev_param_labels()[["params"]]), function(i) {
      column(
        width = 6,
        numericInput(
          inputId = paste0(input$sev_dist,  "_", sev_param_labels()[["params"]][i]), 
          label = sev_param_labels()[["params"]][i], 
          value = sev_param_labels()[["values"]][i]
        )
      )
    })
  })
  


# Render Valuebox ---------------------------------------------------------

  output$ui_loss <- renderUI({
    
    req(ob_total()) 
    
    agg <- ob_total()
    net_mean <- round(mean(agg$total_net_agg), 0)
    
    shinydashboard::valueBox(scales::dollar(net_mean), "Perdida esperada", icon = icon("donate", lib = "font-awesome"),
             width = NULL, color = "teal")
    
    
  })  
  
  output$ui_loss_quantil <- renderUI({
    
    req(quant_agg()) 
    quant_sel <- unname(quant_agg()) %>% round(0)
    quant_pct <- names(quant_agg())
    
    shinydashboard::valueBox(scales::dollar(quant_sel), paste("Perdida al ", quant_pct , " de confianza"), icon = icon("hand-holding-usd", lib = "font-awesome"),
                             width = NULL, color = "teal")
    
    
  })  
  
  output$ui_loss_ced <- renderUI({
    
    req(ob_total()) 
    totes <- ob_total()
    
    ceded_mean <- mean(totes$total_ceded)
    
    shinydashboard::valueBox(scales::dollar(ceded_mean), "Perdida cedida esperada", icon = icon("handshake", lib = "font-awesome"),
                             width = NULL, color = "teal")
    
    
  })  

# Simulacion --------------------------------------------------------------

  ult <- eventReactive(input$run_freq, {
     set.seed(1234) 
     freq <- switch(input$freq_dist,
                     "poisson" = rpois(input$obs, lambda = input$poisson_lambda),
                     "binomial" = rbinom(input$obs, size = input$binomial_m, 
                                         prob = input$binomial_q),
                     "nbinomial" = rnbinom(input$obs, size = input$nbinomial_r, 
                                                prob = (1 / (1 + input$nbinomial_B)))
      )
      #print(freq)
     switch(input$sev_dist,
             "lognormal" = lapply(freq, function(x) rlnorm(x, meanlog = input$lognormal_mu, sdlog = input$lognormal_sigma)),
             "pareto" = lapply(freq, function(x) rpareto(x, scale = input$pareto_theta, shape = input$pareto_alpha)),
             "exponential" = lapply(freq, function(x) rexp(x, rate = 1 / input$exponential_theta)),
             "gamma" = lapply(freq, function(x) rgamma(x, shape = input$gamma_alpha, scale = input$gamma_theta)),
             "weibull" = lapply(freq, function(x) rweibull(x, shape = input$weibull_tau, scale = input$weibull_theta))
      )
     
  })
  
  # convertir lista de reclamaciones en matriz
  tidy_claims <- reactive({
    
    ult_hold <- ult()
    out <- lapply(1:length(ult_hold), function(x) {
      if (length(ult_hold[[x]]) > 0) {
        data.frame("sim" = x, "severity" = ult_hold[[x]])
      }
    })
    
    bind_rows(out) %>%
      mutate(severity = round(severity, 2))
  })
  
  # perdida limitada por reclamo
  below_specific <- reactive({
    if (is.na(input$specific_lim)) {
      out <- tidy_claims() %>%
        mutate(severity_lim = severity)  
    } else {
      out <- tidy_claims() %>%
        mutate(severity_lim = pmin(severity, input$specific_lim))
    }
  
    out
  })
  
  # pérdida total por observación
  ob_total <- reactive({
    out <- below_specific() %>%
      group_by(sim) %>%
      summarise(total_gross = sum(severity),
                total_net_specific = sum(severity_lim))
    
    if (is.na(input$agg_lim)) {
      out <- out %>% mutate(total_net_agg = total_net_specific)
    } else {
      out <- out %>% mutate(total_net_agg = pmin(total_net_specific, input$agg_lim))
    }
    
    out %>%
      mutate(total_ceded = total_gross - total_net_agg)
  })
  
  quant_agg <- reactive({
    quantile(ob_total()$total_net_agg, input$ci)
  })
  

# Plots -------------------------------------------------------------------

  
  plot_subtitle <- eventReactive(input$run_freq, {
    
    n_sel <- input$obs
    freq_sel <- input$freq_dist
    sev_sel <- input$sev_dist
    
    freq_params_sel <- freq_param_labels()$params
    
    freq_params_inputs <- vector("numeric", length = length(freq_params_sel))
    
    for (i in seq_along(freq_params_sel)) {
      freq_params_inputs[i] <- input[[paste0(freq_sel, "_", freq_params_sel[i])]]
    }
    freq_params_sel <- paste0(freq_params_sel, " = ", freq_params_inputs, collapse = ", ")
    
    sev_params_sel <- sev_param_labels()$params
    
    sev_params_inputs <- vector("numeric", length = length(sev_params_sel))
    for (i in seq_along(sev_params_sel)) {
      sev_params_inputs[i] <- input[[paste0(sev_sel, "_", sev_params_sel[i])]]
    }
    sev_params_sel <- paste0(sev_params_sel, " = ", sev_params_inputs, collapse = ", ")
    
    paste0(
      "Observaciones: ", n_sel, 
      "; Frecuencia: ", freq_sel, "(", freq_params_sel, ")",
      "; Severidad: ", sev_sel, "(", sev_params_sel, ")"
    )
  })
  
  output$hist_plot <- renderHighchart({
    agg <- ob_total()
    net_mean <- round(mean(agg$total_net_agg), 0)
    quant_sel <- unname(quant_agg()) %>% round(0)
    quant_pct <- names(quant_agg())
    
    
    hchart(agg$total_net_agg) %>% 
      hc_title(text = "Pérdidas retenidas: neto de exceso de recuperaciones") %>%
      hc_subtitle(text = plot_subtitle()) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = tychobratools::hc_btn_options()  
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(
        title = list(text = "Pérdida neta"),
        plotLines = list(
          list(
            label = list(
              text = paste0("media = ", format(net_mean, big.mark = ","))
            ),
            color = "#FF0000",
            width = 2,
            value = net_mean,
            zIndex = 5
          ),
          list(
            label = list(text = paste0(quant_pct, " Nivel de confianza = ", format(quant_sel, big.mark = ","))),
            color = "#FF0000",
            width = 2,
            value = quant_sel,
            zIndex = 5
          )
        )
      ) %>%
      hc_yAxis(
        title = list(text = "Numero de Observaciones")
      )
  })
  
  quant_totes <- reactive({
    quantile(ob_total()$total_gross, input$ci)
  })
  
  output$hist_plot_total <- renderHighchart({
     totes <- ob_total()
     
     totes_mean <- mean(totes$total_gross)
     quant_sel <- unname(quant_totes()) %>% round(0)
     quant_pct <- names(quant_totes())
     
     hchart(totes$total_gross) %>%
       hc_title(text = "Pérdidas brutas") %>%
       hc_subtitle(text = plot_subtitle()) %>%
       hc_exporting(
         enabled = TRUE
       ) %>%
       hc_legend(enabled = FALSE) %>%
       hc_xAxis(
         title = list(text = "Pérdida bruta"),
         plotLines = list(
           list(
             label = list(
               text = paste0("media = ", format(round(totes_mean, 0), big.mark = ","))
             ),
             color = "#FF0000",
             width = 2,
             value = totes_mean,
             zIndex = 5
           ),
           list(
             label = list(text = paste0(quant_pct, " Nivel de confianza = ", format(quant_sel, big.mark = ","))),
             color = "#FF0000",
             width = 2,
             value = quant_sel,
             zIndex = 5
           )
         )
       ) %>%
       hc_yAxis(
         title = list(text = "Numbero de Observaciones")
       )
  })
  
  quant_ceded <- reactive({
    quantile(ob_total()$total_ceded, input$ci)
  })
  
  output$hist_plot_ceded <- renderHighchart({
    totes <- ob_total()
    
    ceded_mean <- mean(totes$total_ceded)
    quant_sel <- unname(quant_ceded()) %>% round(0)
    quant_pct <- names(quant_ceded())
    
    hchart(totes$total_ceded, breaks = "Scott") %>%
      hc_title(text = "Pérdidas cedidas") %>%
      hc_subtitle(text = plot_subtitle()) %>%
      hc_exporting(
        enabled = TRUE
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(
        title = list(text = "Pérdidas cedidas"),
        plotLines = list(
          list(
            label = list(
              text = paste0("media = ", format(round(ceded_mean, 0), big.mark = ","))
            ),
            color = "#FF0000",
            width = 2,
            value = ceded_mean,
            zIndex = 5
          ),
          list(
            label = list(text = paste0(quant_pct, " Nivel de confianza = ", format(quant_sel, big.mark = ","))),
            color = "#FF0000",
            width = 2,
            value = quant_sel,
            zIndex = 5
          )
        )
      ) %>%
      hc_yAxis(
        title = list(text = "Numbero de Observaciones")
      )
  })
  

# Tabla -------------------------------------------------------------------

  
  # calculo de percentiles 
  cl_values <- function(obs) {
    percentile <- c(.999, 0.995, seq(0.99, 0.9, -0.01), seq(0.85, 0.05, by = -0.05))
    points <- quantile(obs, percentile)
    obs_mean <- mean(obs)
    c(obs_mean, points)
  }
  
  # tablas de resultados de valores correspondientes a los percentiles
  cl_data <- reactive({
     
     net_specific <- cl_values(ob_total()$total_net_specific)
     net <- cl_values(ob_total()$total_net_agg)
     gross <- cl_values(ob_total()$total_gross)
     ceded <- cl_values(ob_total()$total_ceded)
     
     out <- data.frame(net_specific, net, gross, ceded)
     
     cbind("Value At Risk" = c("mean", rownames(out)[-1]), out)
  })
   
  output$sorter <- DT::renderDataTable({
     hold <- cl_data()
     
     sketch <- htmltools::withTags(table(
       class = 'display',
       thead(
         tr(
           th(rowspan = 2, 'Nivel de confianza'),
           th(colspan = 4, 'Peridas')
         ),
         tr(
           th('Neto por solo reclamo'),
           th('Neto retenido'),
           th('Bruto'),
           th('Cedido')
         )
       )
     ))
     
     DT::datatable(
       hold,
       rownames = FALSE,
       container = sketch,
       extensions = "Buttons",
       options = list(
         dom = "Bt",
         buttons = c("excel", "csv", "pdf"),
         ordering = FALSE,
         columnDefs = list(
           list(class = "dt-center", targets = 0)
         ),
         pageLength = nrow(hold)
       )
     ) %>% 
       formatCurrency(
         columns = 2:5,
         currency = "",
         digits = 0
       )
  })
   
  output$download_claims <- downloadHandler(
     filename = function() {
       paste0("ractuary-sim-claims-", Sys.Date(), ".csv")
     },
     content = function(file) {
       write.csv(
         tidy_claims(), 
         file = file,
         row.names = FALSE
       )
     }
  )
}