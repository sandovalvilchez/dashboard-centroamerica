# app.R
# Dashboard interactivo en Shiny (plantilla genérica)
# Soporta carga de archivos: CSV, Excel (xls/xlsx) y SAV

library(shiny)
library(dplyr)
library(readr)
library(readxl)
library(haven)
library(ggplot2)
library(DT)
library(tools)

ui <- fluidPage(
  titlePanel("Dashboard Interactivo - Plantilla Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controles y filtros"),
      
      # 1) Carga de datos local (Excel, CSV, SAV)
      fileInput(
        inputId = "file",
        label   = "Cargar base de datos (CSV, Excel o SAV):",
        accept  = c(
          ".csv",
          ".xls", ".xlsx",
          ".sav"
        )
      ),
      
      # 2) (Opcional) lectura desde URL (CSV o API simple)
      textInput(
        inputId = "url_datos",
        label   = "Enlace a datos (CSV en la web, opcional):",
        placeholder = "https://.../datos.csv"
      ),
      actionButton("cargar_url", "Cargar desde URL"),
      tags$hr(),
      
      # Selectores de variables (se llenan dinámicamente)
      selectInput("var_cat", "Variable categórica para filtrar:", choices = NULL),
      uiOutput("niveles_cat_ui"),
      
      selectInput("var_num", "Variable numérica para el gráfico:", choices = NULL),
      
      width = 3
    ),
    
    mainPanel(
      # Espacio para gráficos / mapa
      h4("Gráfico estadístico (responsivo a los filtros)"),
      plotOutput("plot_principal", height = "350px"),
      
      tags$hr(),
      
      # Espacio para DataTable
      h4("Tabla de datos filtrada"),
      DTOutput("tabla_datos"),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  
  #----------------------------
  # 1. Carga de datos (reactiva)
  #----------------------------
  
  datos <- reactiveVal(NULL)
  
  # a) Cargar archivo local
  observeEvent(input$file, {
    req(input$file)
    ext  <- file_ext(input$file$name)
    path <- input$file$datapath
    
    df <- switch(
      ext,
      "csv"  = readr::read_csv(path, show_col_types = FALSE),
      "xls"  = readxl::read_excel(path),
      "xlsx" = readxl::read_excel(path),
      "sav"  = haven::read_sav(path),
      {
        showNotification("Formato no soportado. Use CSV, Excel o SAV.", type = "error")
        return(NULL)
      }
    )
    
    datos(df)
  })
  
  # b) Cargar desde URL (ej. enlace a CSV)
  observeEvent(input$cargar_url, {
    req(input$url_datos)
    url <- input$url_datos
    
    # Intenta leer CSV desde URL
    df <- tryCatch(
      {
        readr::read_csv(url, show_col_types = FALSE)
      },
      error = function(e) {
        showNotification("No se pudo leer la URL. Verifique el enlace o el formato.", type = "error")
        return(NULL)
      }
    )
    
    if (!is.null(df)) {
      datos(df)
    }
  })
  
  #----------------------------
  # 2. Actualizar filtros según datos
  #----------------------------
  
  observeEvent(datos(), {
    df <- datos()
    req(df)
    
    # Variables categóricas: factor o character
    vars_cat <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    # Variables numéricas
    vars_num <- names(df)[sapply(df, is.numeric)]
    
    if (length(vars_cat) == 0) {
      vars_cat <- ""
    }
    if (length(vars_num) == 0) {
      vars_num <- ""
    }
    
    updateSelectInput(
      session,
      "var_cat",
      choices  = vars_cat,
      selected = vars_cat[1]
    )
    
    updateSelectInput(
      session,
      "var_num",
      choices  = vars_num,
      selected = vars_num[1]
    )
  })
  
  # Selector de niveles de la variable categórica
  output$niveles_cat_ui <- renderUI({
    df <- datos()
    req(df)
    req(input$var_cat)
    if (input$var_cat == "" || is.null(df[[input$var_cat]])) {
      return(NULL)
    }
    
    niveles <- sort(unique(as.character(df[[input$var_cat]])))
    
    selectInput(
      inputId  = "niveles_cat",
      label    = "Valores a incluir:",
      choices  = niveles,
      selected = niveles,
      multiple = TRUE
    )
  })
  
  #----------------------------
  # 3. Datos filtrados
  #----------------------------
  
  datos_filtrados <- reactive({
    df <- datos()
    req(df)
    
    # Filtrar por variable categórica
    if (!is.null(input$var_cat) && input$var_cat != "" &&
        !is.null(input$niveles_cat) && length(input$niveles_cat) > 0) {
      df <- df %>%
        filter(.data[[input$var_cat]] %in% input$niveles_cat)
    }
    
    df
  })
  
  #----------------------------
  # 4. Gráfico principal
  #----------------------------
  
  output$plot_principal <- renderPlot({
    df <- datos_filtrados()
    req(df)
    req(input$var_num)
    
    validate(
      need(input$var_num %in% names(df), "Seleccione una variable numérica válida.")
    )
    
    ggplot(df, aes(x = .data[[input$var_num]])) +
      geom_histogram(bins = 30) +
      labs(
        x = input$var_num,
        y = "Frecuencia",
        title = paste("Distribución de", input$var_num)
      ) +
      theme_minimal()
  })
  
  #----------------------------
  # 5. DataTable
  #----------------------------
  
  output$tabla_datos <- renderDT({
    df <- datos_filtrados()
    req(df)
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX    = TRUE
      )
    )
  })
}

shinyApp(ui = ui, server = server)
