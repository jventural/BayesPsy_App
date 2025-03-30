# Cargar las librerías necesarias
library(shiny)
library(shinythemes)
library(blavaan)
library(dplyr)
library(ggplot2)
library(readxl)
library(PsyMetricTools)
library(BayesPsyMetrics)
library(DT)
library(openxlsx)
library(RColorBrewer)
library(bibtex)
library(sessioninfo)

# Generar el archivo de referencias con los paquetes adjuntos y leer el bibtex
si <- sessioninfo::session_info()
attached_pkgs <- si$packages$package[si$packages$attached]
bibtex::write.bib(attached_pkgs, file = "references.bib")
bibs <- bibtex::read.bib("references.bib")

# Función para convertir BibTeX a HTML
convert_bib_to_html <- function(bibs) {
  bib_char <- sapply(bibs, function(x) paste(format(x), collapse = " "))
  bib_html <- gsub("_(.*?)_", "<em>\\1</em>", bib_char)
  bib_html <- gsub("\\\\texttt\\{(.*?)\\}", "<code>\\1</code>", bib_html)
  bib_html <- paste0("<p>", bib_html, "</p>")
  paste(bib_html, collapse = "\n")
}

# Definir la interfaz de usuario con 11 pestañas (la última es "How to cite")
ui <- navbarPage("BayesPsy",
                 
                 theme = shinytheme("cerulean"),
                 
                 # ----- Paso 1: Priors -----
                 tabPanel("Paso 1: Priors",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 1: Priors (por defecto)"),
                              actionButton("show_priors", "Mostrar Priors")
                            ),
                            mainPanel(
                              verbatimTextOutput("priors_output")
                            )
                          )
                 ),
                 
                 # ----- Paso 2: Modelo -----
                 tabPanel("Paso 2: Modelo",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 2: Estimación del Modelo"),
                              fileInput("datafile", "Subir archivo de datos (Excel)", accept = ".xlsx"),
                              textInput("model_spec", "Especificación del modelo", value = ""),
                              numericInput("n_chains", "Número de cadenas", value = 3, min = 1),
                              numericInput("burnin", "Burn-in", value = 1000, min = 0),
                              numericInput("sample", "Número de muestras", value = 1000, min = 100),
                              numericInput("seed", "Seed", value = 12345, min = 1),
                              # Inputs para crear el modelo nulo aparecen vacíos por defecto
                              textInput("null_prefix", "Prefijo para create_null_model (e.g., RSE, NSI)", value = ""),
                              textInput("null_range", "Rango de Items (e.g., 1:10)", value = ""),
                              actionButton("run_model", "Ejecutar Modelo")
                            ),
                            mainPanel(
                              verbatimTextOutput("model_summary")
                            )
                          )
                 ),
                 
                 # ----- Paso 3: Trace Plot -----
                 tabPanel("Paso 3: Trace Plot",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 3: Trazas de las cadenas"),
                              actionButton("plot_trace", "Generar Trace Plot"),
                              downloadButton("downloadTracePlot", "Descargar Trace Plot")
                            ),
                            mainPanel(
                              plotOutput("trace_plot")
                            )
                          )
                 ),
                 
                 # ----- Paso 4: Convergencia Local -----
                 tabPanel("Paso 4: Convergencia Local",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 4: Modelo con Iteraciones Duplicadas"),
                              actionButton("run_long_model", "Ejecutar Modelo Largo")
                            ),
                            mainPanel(
                              verbatimTextOutput("convergence_output")
                            )
                          )
                 ),
                 
                 # ----- Paso 5: Histograma Posterior -----
                 tabPanel("Paso 5: Histograma Posterior",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 5: Histograma del Posterior"),
                              downloadButton("downloadPosteriorHist", "Descargar Histograma")
                            ),
                            mainPanel(
                              plotOutput("posterior_hist")
                            )
                          )
                 ),
                 
                 # ----- Paso 6: Diagnósticos MCMC -----
                 tabPanel("Paso 6: Diagnósticos MCMC",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 6: Autocorrelación, neff y PSRF")
                            ),
                            mainPanel(
                              verbatimTextOutput("mcmc_diagnostics")
                            )
                          )
                 ),
                 
                 # ----- Paso 7: Intervalos Creíbles -----
                 tabPanel("Paso 7: Intervalos Creíbles",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 7: Gráfico de Intervalos Creíbles"),
                              actionButton("plot_intervals", "Generar Gráfico de Intervalos"),
                              downloadButton("downloadIntervals", "Descargar Gráfico de Intervalos")
                            ),
                            mainPanel(
                              plotOutput("credible_intervals")
                            )
                          )
                 ),
                 
                 # ----- Paso 8: Sensibilidad de Priors -----
                 tabPanel("Paso 8: Sensibilidad Priors",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 8: Modelo con Priors Custom"),
                              textInput("dp_custom_input", "Priors Custom (dp_custom):", 
                                        value = "lambda = 'normal(0.7,0.5)'"),
                              actionButton("run_custom_model", "Ejecutar Modelo Custom")
                            ),
                            mainPanel(
                              verbatimTextOutput("sensitivity_output")
                            )
                          )
                 ),
                 
                 # ----- Paso 9: Índices de Ajuste -----
                 tabPanel("Paso 9: Índices de Ajuste",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 9: Índices de Ajuste (Fit Indices)"),
                              downloadButton("downloadBayesIndices", "Descargar Gráfico de Índices Bayesianos")
                            ),
                            mainPanel(
                              verbatimTextOutput("fit_indices_output"),
                              plotOutput("bayes_indices_plot")
                            )
                          )
                 ),
                 
                 # ----- Paso 10: Informe Final -----
                 tabPanel("Paso 10: Informe Final",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Paso 10: Informe Final"),
                              downloadButton("download_report", "Descargar Informe Final")
                            ),
                            mainPanel(
                              verbatimTextOutput("final_report")
                            )
                          )
                 ),
                 
                 # ----- Nueva pestaña: How to cite -----
                 tabPanel("How to cite",
                          fluidPage(
                            h2("How to quote this Shiny"),
                            tags$blockquote(
                              "Ventura-León, J. (2025). ",
                              em("BayesPsy"), 
                              " [Aplicación Shiny]. https://github.com/jventural/BayesPsy_app"
                            ),
                            h3("References used to build the application:"),
                            htmlOutput("bibReferences")
                          )
                 )
)

# Definir el servidor
server <- function(input, output, session) {
  
  # Variables reactivas para almacenar los resultados de los modelos
  fit_default <- reactiveVal(NULL)
  fit_long <- reactiveVal(NULL)
  fit_custom <- reactiveVal(NULL)
  null_model_fit <- reactiveVal(NULL)
  
  # ----- Paso 1: Mostrar priors por defecto -----
  observeEvent(input$show_priors, {
    priors_text <- capture.output(dpriors())
    output$priors_output <- renderPrint({
      cat(priors_text, sep = "\n")
    })
  })
  
  # ----- Paso 2: Estimar el Modelo (con barra de progreso) -----
  observeEvent(input$run_model, {
    req(input$datafile)
    withProgress(message = "Ejecutando Modelo", value = 0, {
      incProgress(0.3, detail = "Cargando datos")
      data <- read_excel(input$datafile$datapath)
      incProgress(0.3, detail = "Ejecutando bcfa")
      fit <- bcfa(input$model_spec, data = data, 
                  n.chains = input$n_chains, 
                  burnin = input$burnin, 
                  sample = input$sample, 
                  seed = input$seed)
      incProgress(0.3, detail = "Procesando resultados")
      fit_default(fit)
      summ <- capture.output(summary(fit, standardized = TRUE))
      output$model_summary <- renderPrint({
        cat(summ, sep = "\n")
      })
      incProgress(0.1)
    })
  })
  
  # ----- Paso 3: Trace Plot (con botón de descarga) -----
  observeEvent(input$plot_trace, {
    req(fit_default())
    trace_plot <- plot(fit_default(), pars = 1:5, plot.type = "trace", showplot = FALSE) +
      scale_color_manual(values = RColorBrewer::brewer.pal(3, "Pastel1")) +
      theme_bw()
    output$trace_plot <- renderPlot({
      trace_plot
    })
  })
  
  output$downloadTracePlot <- downloadHandler(
    filename = function() {
      paste("Trace_Plot_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(fit_default())
      trace_plot <- plot(fit_default(), pars = 1:5, plot.type = "trace", showplot = FALSE) +
        scale_color_manual(values = RColorBrewer::brewer.pal(3, "Pastel1")) +
        theme_bw()
      ggsave(file, plot = trace_plot, device = "jpeg", dpi = 600, width = 8, height = 6)
    }
  )
  
  # ----- Paso 4: Convergencia Local (Modelo Largo con iteraciones duplicadas) -----
  observeEvent(input$run_long_model, {
    req(input$datafile, fit_default())
    withProgress(message = "Ejecutando Modelo Largo", value = 0, {
      incProgress(0.3, detail = "Cargando datos")
      data <- read_excel(input$datafile$datapath)
      incProgress(0.3, detail = "Ejecutando bcfa (modelo largo)")
      fit_long_model <- bcfa(input$model_spec, data = data, 
                             n.chains = input$n_chains, 
                             burnin = input$burnin * 2, 
                             sample = input$sample * 2, 
                             seed = input$seed)
      incProgress(0.3, detail = "Comparando coeficientes")
      fit_long(fit_long_model)
      coef_diff <- coef(fit_long_model) - coef(fit_default())
      output$convergence_output <- renderPrint({
        cat("Diferencias en coeficientes (Modelo Largo - Modelo por Defecto):\n")
        print(coef_diff)
      })
      incProgress(0.1)
    })
  })
  
  # ----- Paso 5: Histograma del Posterior (con botón de descarga) -----
  output$posterior_hist <- renderPlot({
    req(fit_default())
    BayesPsyMetrics::plot_bhist_loadings(fit_default())
  })
  
  output$downloadPosteriorHist <- downloadHandler(
    filename = function() {
      paste("Histograma_Posterior_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(fit_default())
      p <- BayesPsyMetrics::plot_bhist_loadings(fit_default())
      ggsave(file, plot = p, device = "jpeg", dpi = 600, width = 8, height = 6)
    }
  )
  
  # ----- Paso 6: Diagnósticos MCMC -----
  output$mcmc_diagnostics <- renderPrint({
    req(fit_default())
    ac10 <- compute_acf_lag10(fit_default())
    neff <- blavInspect(fit_default(), "neff")
    psrf <- blavInspect(fit_default(), "psrf")
    stats <- BayesPsyMetrics::aditional_stat(ac10, neff, psrf)
    cat("Autocorrelación (lag 10):\n")
    print(ac10)
    cat("\nTamaño Efectivo de la Muestra (neff):\n")
    print(neff)
    cat("\nPSRF:\n")
    print(psrf)
    cat("\nResumen de estadísticas adicionales:\n")
    print(stats$summary)
  })
  
  # ----- Paso 7: Intervalos Creíbles -----
  observeEvent(input$plot_intervals, {
    req(fit_default())
    plot_IC <- BayesPsyMetrics::plot_binterval_loadings(fit_default(), labels = TRUE)
    output$credible_intervals <- renderPlot({
      plot_IC
    })
  })
  
  output$downloadIntervals <- downloadHandler(
    filename = function() {
      paste("Intervalos_Creibles_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(fit_default())
      p <- BayesPsyMetrics::plot_binterval_loadings(fit_default(), labels = TRUE)
      ggsave(file, plot = p, device = "jpeg", dpi = 600, width = 8, height = 6)
    }
  )
  
  # ----- Paso 8: Sensibilidad de Priors -----
  observeEvent(input$run_custom_model, {
    req(input$datafile, fit_default(), input$dp_custom_input)
    withProgress(message = "Ejecutando Modelo con Priors Custom", value = 0, {
      incProgress(0.3, detail = "Cargando datos")
      data <- read_excel(input$datafile$datapath)
      incProgress(0.3, detail = "Evaluando dp_custom")
      dp_custom <- eval(parse(text = paste0("dpriors(", input$dp_custom_input, ")")))
      incProgress(0.3, detail = "Ejecutando bcfa con dp_custom")
      fit_custom_model <- bcfa(input$model_spec, data = data, 
                               dp = dp_custom,
                               n.chains = input$n_chains, 
                               burnin = input$burnin, 
                               sample = input$sample, 
                               seed = input$seed)
      incProgress(0.3, detail = "Comparando coeficientes")
      fit_custom(fit_custom_model)
      coef_diff_custom <- coef(fit_custom_model) - coef(fit_default())
      output$sensitivity_output <- renderPrint({
        cat("Diferencias en coeficientes (Modelo con Priors Custom - Modelo por Defecto):\n")
        print(coef_diff_custom)
      })
      incProgress(0.1)
    })
  })
  
  # ----- Paso 9: Índices de Ajuste -----
  observe({
    req(input$datafile, fit_default(), input$null_prefix, input$null_range)
    withProgress(message = "Calculando Índices de Ajuste", value = 0, {
      incProgress(0.3, detail = "Cargando datos")
      data <- read_excel(input$datafile$datapath)
      incProgress(0.3, detail = "Ajustando modelo nulo")
      null_range <- eval(parse(text = input$null_range))
      null_model <- create_null_model(input$null_prefix, null_range)
      fit_null <- bcfa(null_model, data = data, 
                       n.chains = input$n_chains, 
                       burnin = input$burnin, 
                       sample = input$sample, 
                       seed = input$seed)
      incProgress(0.2, detail = "Calculando fit indices")
      null_model_fit(fit_null)
      bfit1 <- blavFitIndices(fit_default(), baseline.model = fit_null, 
                              rescale = "devM", pD = "dic")
      output$fit_indices_output <- renderPrint({
        cat("Índices de ajuste (comparados con el modelo nulo):\n")
        print(bfit1)
      })
      output$bayes_indices_plot <- renderPlot({
        BayesPsyMetrics::plot_bayes_indices(bfit1)
      })
      incProgress(0.1)
    })
  })
  
  output$downloadBayesIndices <- downloadHandler(
    filename = function() {
      paste("Bayes_Indices_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(fit_default())
      data <- read_excel(input$datafile$datapath)
      null_range <- eval(parse(text = input$null_range))
      null_model <- create_null_model(input$null_prefix, null_range)
      fit_null <- bcfa(null_model, data = data, 
                       n.chains = input$n_chains, 
                       burnin = input$burnin, 
                       sample = input$sample, 
                       seed = input$seed)
      bfit1 <- blavFitIndices(fit_default(), baseline.model = fit_null, 
                              rescale = "devM", pD = "dic")
      p <- BayesPsyMetrics::plot_bayes_indices(bfit1)
      ggsave(file, plot = p, device = "jpeg", dpi = 600, width = 8, height = 6)
    }
  )
  
  # ----- Paso 10: Informe Final -----
  output$final_report <- renderPrint({
    req(fit_default(), null_model_fit())
    final_results <- list(
      "Resumen Modelo por Defecto" = summary(fit_default(), standardized = TRUE),
      "Modelo con Iteraciones Duplicadas" = if (!is.null(fit_long())) summary(fit_long(), standardized = TRUE) else "No ejecutado",
      "Sensibilidad (Diferencia coeficientes)" = if (!is.null(fit_custom())) coef(fit_custom()) - coef(fit_default()) else "No ejecutado",
      "Índices de Ajuste" = blavFitIndices(fit_default(), baseline.model = null_model_fit(), rescale = "devM", pD = "dic")
    )
    print(final_results)
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Informe_Final_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(fit_default(), null_model_fit())
      final_results <- list(
        "Resumen_Modelo" = as.data.frame(summary(fit_default(), standardized = TRUE)$PE),
        "Sensibilidad" = as.data.frame(if (!is.null(fit_custom())) coef(fit_custom()) - coef(fit_default()) else NA),
        "Índices_Ajuste" = as.data.frame(blavFitIndices(fit_default(), baseline.model = null_model_fit(), rescale = "devM", pD = "dic"))
      )
      openxlsx::write.xlsx(final_results, file)
    }
  )
  
  # ----- Nueva pestaña: How to cite -----
  output$bibReferences <- renderUI({
    HTML(convert_bib_to_html(bibs))
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
