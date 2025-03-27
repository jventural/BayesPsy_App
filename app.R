library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(PsyMetricTools)
library(blavaan)
library(lavaan)
library(BayesPsyMetrics)
library(openxlsx)
library(wesanderson)
library(RColorBrewer)
library(DT)
library(ggplot2)
library(rlang)
library(sessioninfo)
library(bibtex)

# Generar el archivo de referencias con los paquetes adjuntos y leer el bibtex
si <- sessioninfo::session_info()
attached_pkgs <- si$packages$package[si$packages$attached]
bibtex::write.bib(attached_pkgs, file = "references.bib")
bibs <- bibtex::read.bib("references.bib")

# Se asume que la función convert_bib_to_html() está definida en otro lado o la defines tú.
# Por ejemplo, podrías definir una versión simple:
convert_bib_to_html <- function(bibs) {
  # Convertir cada entrada a texto y unir las líneas
  bib_char <- sapply(bibs, function(x) paste(format(x), collapse = " "))
  # Reemplazar _texto_ por cursivas (<em>texto</em>)
  bib_html <- gsub("_(.*?)_", "<em>\\1</em>", bib_char)
  # Reemplazar \texttt{texto} por código (<code>texto</code>)
  bib_html <- gsub("\\\\texttt\\{(.*?)\\}", "<code>\\1</code>", bib_html)
  # Envolver cada referencia en un párrafo
  bib_html <- paste0("<p>", bib_html, "</p>")
  # Unir todas las entradas
  paste(bib_html, collapse = "\n")
}


ui <- navbarPage("BayesPsy",
                 theme = shinytheme("cerulean"),
                 
                 # 1. Data Upload and Preprocessing
                 tabPanel("Data Upload and Preprocessing",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Data Upload and Preprocessing"),
                              fileInput("archivo", "Select Test File (Excel)", accept = c(".xlsx")),
                              textInput("items", "Items to reverse (e.g., item2, item5)", value = ""),
                              numericInput("num_respuestas", "Number of Responses (e.g., 4)", value = NA, min = 0),
                              checkboxInput("comienza_con_cero", "Starts with zero?", value = FALSE),
                              actionButton("procesar", "Process Data")
                            ),
                            mainPanel(
                              h4("Processed Data"),
                              DT::dataTableOutput("tabla")
                            )
                          )
                 ),
                 
                 # 2. Null Model
                 tabPanel("Null Model",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Null Model Summary"),
                              textInput("null_prefix", "Item Prefix (e.g., RSE)", value = ""),
                              textInput("null_range", "Items (range or list) (e.g., 1:10)", value = ""),
                              numericInput("null_n_chains", "Number of chains", value = 3, min = 1),
                              numericInput("null_burnin", "Burnin", value = 1000, min = 0),
                              numericInput("null_sample", "Samples", value = 1000, min = 1),
                              numericInput("null_seed", "Seed", value = 2025, min = 0),
                              actionButton("procesar_modelo_nulo", "Run Null Model")
                            ),
                            mainPanel(
                              verbatimTextOutput("modeloNulo")
                            )
                          )
                 ),
                 
                 # 3. Model Estimation
                 tabPanel("Model Estimation",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Model Parameters"),
                              textAreaInput("modelo1", "Specify the model:", value = "", rows = 3),
                              numericInput("n_chains", "Number of chains", value = 3, min = 1),
                              numericInput("burnin", "Burnin", value = 1000, min = 0),
                              numericInput("sample", "Samples", value = 1000, min = 1),
                              numericInput("seed", "Seed", value = 2025, min = 0),
                              actionButton("procesar_modelo1", "Run Model")
                            ),
                            mainPanel(
                              h4("Model Results"),
                              verbatimTextOutput("modeloFactorUnico")
                            )
                          )
                 ),
                 
                 # 4. Combined Summary
                 tabPanel("Combined Summary",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Combined Fit Summary"),
                              downloadButton("downloadCombined", "Download Combined Summary")
                            ),
                            mainPanel(
                              tableOutput("combinedModelTable")
                            )
                          )
                 ),
                 
                 # 5. Preliminaries and Factor Results
                 tabPanel("Preliminaries and Factor Results",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Response Rates and Factor Results"),
                              textInput("prelim_columns", "Columns (e.g., RSE1:RSE10)", value = ""),
                              textInput("prelim_items", "Items (e.g., 1:10)", value = ""),
                              textInput("prelim_responses", "Responses (e.g., 1,2,3,4)", value = ""),
                              actionButton("run_prelim", "Run Analysis"),
                              downloadButton("downloadPrelim", "Download Preliminaries Table")
                            ),
                            mainPanel(
                              tableOutput("prelimFactor")
                            )
                          )
                 ),
                 
                 # 6. MCMC Errors
                 tabPanel("MCMC Errors",
                          sidebarLayout(
                            sidebarPanel(
                              h4("MCMC Errors")
                            ),
                            mainPanel(
                              verbatimTextOutput("errores")
                            )
                          )
                 ),
                 
                 # 7. Trace Plot
                 tabPanel("Trace Plot",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Trace Plot for Parameters"),
                              downloadButton("downloadTracePlot", "Download Trace Plot")
                            ),
                            mainPanel(
                              plotOutput("Figura1")
                            )
                          )
                 ),
                 
                 # 8. Model Comparison
                 tabPanel("Model Comparison",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Comparison between Models")
                            ),
                            mainPanel(
                              verbatimTextOutput("comparacion")
                            )
                          )
                 ),
                 
                 # 9. Cómo citar
                 tabPanel("How to cite",
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

server <- function(input, output, session) {
  
  # 1. Data Upload and Preprocessing
  datos <- reactive({
    req(input$archivo)
    read_excel(input$archivo$datapath)
  })
  
  datos_invertidos <- eventReactive(input$procesar, {
    data <- datos()
    items_vector <- if(nchar(input$items) > 0) unlist(strsplit(input$items, split = ",\\s*")) else character(0)
    df_new <- invertir_items(data,
                             items = items_vector,
                             num_respuestas = input$num_respuestas,
                             comienza_con_cero = input$comienza_con_cero)
    df_new
  })
  
  output$tabla <- DT::renderDataTable({
    req(datos_invertidos())
    DT::datatable(datos_invertidos(), options = list(pageLength = 10))
  })
  
  # 2. Null Model
  modelo_nulo <- eventReactive(input$procesar_modelo_nulo, {
    req(datos_invertidos())
    data <- datos_invertidos()
    req(input$null_prefix, input$null_range)
    
    withProgress(message = "Running Null Model...", value = 0, {
      incProgress(0.2, detail = "Processing item range")
      null_range <- tryCatch({
        eval(parse(text = input$null_range))
      }, error = function(e) {
        NULL
      })
      validate(need(!is.null(null_range), "Invalid item range."))
      
      incProgress(0.3, detail = "Creating null model")
      null_model <- create_null_model(input$null_prefix, null_range)
      
      incProgress(0.3, detail = "Fitting null model")
      fit0 <- bcfa(null_model, data = data, 
                   n.chains = input$null_n_chains, 
                   burnin = input$null_burnin, 
                   sample = input$null_sample, 
                   seed = input$null_seed)
      summary_fit0 <- capture.output(summary(fit0))
      
      incProgress(0.2, detail = "Calculating fit indices")
      bnull <- blavFitIndices(fit0, baseline.model = fit0, rescale = "devM", pD = "dic")
      
      list(null_model = null_model, 
           summary_fit0 = summary_fit0, 
           bnull = bnull,
           fit0 = fit0,
           data = data)
    })
  })
  
  output$modeloNulo <- renderPrint({
    req(modelo_nulo())
    cat("Null Model:\n")
    cat(modelo_nulo()$null_model, sep = "\n")
    cat("\n\nNull Model Fit Summary:\n")
    cat(modelo_nulo()$summary_fit0, sep = "\n")
    cat("\n\nFit Indices (Baseline Model):\n")
    print(modelo_nulo()$bnull)
  })
  
  # 3. Model Estimation
  modelo_factor_unico <- eventReactive(input$procesar_modelo1, {
    req(datos_invertidos(), modelo_nulo())
    data <- datos_invertidos()
    withProgress(message = "Running Model...", value = 0, {
      incProgress(0.2, detail = "Fitting the model")
      model1 <- input$modelo1
      fit1 <- bcfa(model1, data = data, 
                   n.chains = input$n_chains, 
                   burnin = input$burnin, 
                   sample = input$sample, 
                   seed = input$seed)
      
      incProgress(0.4, detail = "Calculating model summary")
      summary_fit1 <- capture.output(summary(fit1, standardized = TRUE))
      
      incProgress(0.2, detail = "Obtaining fit measures")
      fitm1 <- fitMeasures(fit1)
      
      incProgress(0.1, detail = "Calculating additional indices")
      bfit1 <- blavFitIndices(fit1, baseline.model = modelo_nulo()$fit0, 
                              rescale = "devM", pD = "dic")
      
      incProgress(0.1, detail = "Finalizing")
      list(model_spec = model1,
           fit1 = fit1,
           summary_fit1 = summary_fit1,
           fitm1 = fitm1,
           bfit1 = bfit1)
    })
  })
  
  output$modeloFactorUnico <- renderPrint({
    req(modelo_factor_unico())
    cat("Model Specification:\n")
    cat(modelo_factor_unico()$model_spec, "\n\n")
    cat("Model Fit Summary:\n")
    cat(modelo_factor_unico()$summary_fit1, sep = "\n")
    cat("\n\nFit Measures:\n")
    print(modelo_factor_unico()$fitm1)
    cat("\n\nAdditional Indices (compared with Null Model):\n")
    print(modelo_factor_unico()$bfit1)
  })
  
  # 4. Combined Summary
  combined_summary <- reactive({
    req(modelo_nulo(), modelo_factor_unico())
    combined_model_summary <- BayesPsyMetrics::get_combined_Fit_Indices_Blavaan(
      list(modelo_nulo()$bnull, modelo_factor_unico()$bfit1),
      credMass = 0.95,
      fits = c(modelo_nulo()$fit0, modelo_factor_unico()$fit1)
    )
    as.data.frame(combined_model_summary)
  })
  
  output$combinedModelTable <- renderTable({
    req(combined_summary())
    combined_summary()
  })
  
  output$downloadCombined <- downloadHandler(
    filename = function() {
      paste("Combined_Summary_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(combined_summary(), file, row.names = FALSE)
    }
  )
  
  # 5. Preliminaries and Factor Results
  result_prelim <- eventReactive(input$run_prelim, {
    req(datos_invertidos(), modelo_factor_unico())
    df <- datos_invertidos()
    
    # Selecciona las columnas ingresadas por el usuario (por ejemplo RSE1:RSE10)
    numeric_df <- df %>% select(!!rlang::parse_expr(input$prelim_columns))
    
    # Convierte el texto de "Items (e.g., 1:10)" en un vector de items
    items_vec <- tryCatch(eval(parse(text = input$prelim_items)), error = function(e) NULL)
    validate(need(!is.null(items_vec), "Invalid items input."))
    
    # Convierte la cadena "1,2,3,4" en un vector de respuestas
    responses_vec <- if(nchar(input$prelim_responses) > 0) strsplit(input$prelim_responses, ",\\s*")[[1]] else character(0)
    
    df_preliminar <- rates_responses(numeric_df,
                                     items = items_vec,
                                     responses = responses_vec)
    
    fit1_results <- get_factor_blavaan(modelo_factor_unico()$fit1)
    combined_results <- bind_cols(df_preliminar, fit1_results)
    combined_results
  })
  
  output$prelimFactor <- renderTable({
    req(result_prelim())
    result_prelim()
  })
  
  output$downloadPrelim <- downloadHandler(
    filename = function() {
      paste("Preliminaries_and_Factor_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(result_prelim(), file, row.names = FALSE)
    }
  )
  
  # 6. MCMC Errors
  errores_reactive <- reactive({
    req(modelo_factor_unico())
    calculate_mcmc_standard_error(modelo_factor_unico()$fit1)
  })
  
  output$errores <- renderPrint({
    req(errores_reactive())
    print(errores_reactive())
  })
  
  # 7. Trace Plot
  output$Figura1 <- renderPlot({
    req(modelo_factor_unico())
    p <- plot(modelo_factor_unico()$fit1, pars = 1:5, plot.type = "trace") +
      scale_color_manual(values = RColorBrewer::brewer.pal(3, "Pastel1")) +
      theme_bw()
    print(p)
  })
  
  output$downloadTracePlot <- downloadHandler(
    filename = function() {
      paste("Trace_Plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(modelo_factor_unico())
      p <- plot(modelo_factor_unico()$fit1, pars = 1:5, plot.type = "trace") +
        scale_color_manual(values = RColorBrewer::brewer.pal(3, "Pastel1")) +
        theme_bw()
      ggsave(file, plot = p, device = "png", dpi = 300, width = 8, height = 6)
    }
  )
  
  # 8. Model Comparison
  output$comparacion <- renderPrint({
    req(modelo_factor_unico(), modelo_nulo())
    bc01 <- blavCompare(modelo_factor_unico()$fit1, modelo_nulo()$fit0)
    print(bc01)
  })
  
  # 9. Cómo citar: Mostrar las referencias en HTML
  output$bibReferences <- renderUI({
    HTML(convert_bib_to_html(bibs))
  })
}

shinyApp(ui, server)
