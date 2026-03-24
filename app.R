library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(DT)
library(plotly)
library(writexl)
library(tibble)
library(tidyr)
library(googlesheets4)
library(gargle)

# =========================
# CONFIGURACIÓN
# =========================

# base_dir <- "/home/ignacio/Descargas/openalex_analysis_from_orcid"

gs4_auth(cache = TRUE, email = TRUE)


to_character_safe <- function(x) {
  if (is.list(x)) {
    sapply(x, function(y) {
      if (length(y) == 0 || is.null(y)) return(NA_character_)
      as.character(y[[1]])
    })
  } else {
    as.character(x)
  }
}

parse_inicio_year <- function(x) {
  x <- to_character_safe(x)
  
  out <- suppressWarnings(as.Date(x))
  year_num <- suppressWarnings(as.numeric(format(out, "%Y")))
  
  idx_na <- is.na(year_num) & !is.na(x)
  if (any(idx_na)) {
    year_num[idx_na] <- suppressWarnings(
      as.numeric(format(as.Date(paste0("01-", x[idx_na]), format = "%d-%b-%y"), "%Y"))
    )
  }
  
  year_num
}

to_scalar <- function(x) {
  if (is.list(x)) {
    sapply(x, function(y) {
      if (length(y) == 0 || is.null(y)) return(NA)
      as.character(y[[1]])
    })
  } else {
    as.character(x)
  }
}

to_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(to_scalar(x)))
}


authors_summary <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1TWy3BpabOspf4JqToRxbeVlQEEN_q6x26SjbHwlqOhA/edit?gid=437409061#gid=437409061"
)

authors_summary <- authors_summary %>%
  mutate(
    works_count = to_numeric_safe(works_count),
    cited_by_count = to_numeric_safe(cited_by_count),
    citations_per_publication = to_numeric_safe(citations_per_publication),
    field_weighted_citation_impact = to_numeric_safe(field_weighted_citation_impact),
    h_index = to_numeric_safe(h_index),
    output_in_top_10_percent = to_numeric_safe(output_in_top_10_percent),
    oldest_publication_since_1996 = to_numeric_safe(oldest_publication_since_1996),
    Anio_ingreso_FI = to_numeric_safe(Anio_ingreso_FI)
  )


doctorandos <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1mY-GCLrCTB19obA6uRZSS64tdRCYiGp8MboshYu0lio/edit?gid=512178860#gid=512178860"
)

doctorandos <- doctorandos %>%
  mutate(
    `Apellido y nombre` = to_character_safe(`Apellido y nombre`),
    `Programa de doctorado` = to_character_safe(`Programa de doctorado`),
    `Universidad donde lo realiza` = to_character_safe(`Universidad donde lo realiza`),
    `Área` = to_character_safe(`Área`),
    `Director de tesis` = to_character_safe(`Director de tesis`),
    `Beca CONICET` = to_character_safe(`Beca CONICET`),
    `Cargo docente Dedicación Exclusiva UA` = to_character_safe(`Cargo docente Dedicación Exclusiva UA`),
    `Cargo docente Dedicación Semiexclusiva UA` = to_character_safe(`Cargo docente Dedicación Semiexclusiva UA`),
    `Otras variantes` = to_character_safe(`Otras variantes`),
    `Status_suspendido` = to_character_safe(`Status_suspendido`),
    `Status_graduado` = to_character_safe(`Status_graduado`),
    `Departamento` = to_character_safe(`Departamento`),
    inicio_year = parse_inicio_year(`Fecha de inicio`)
  )

pubs_file <- file.path("data", "publications_from_orcid.csv")
pubyear_file <- file.path("data", "citations_by_publication_year.csv")
cityear_file <- file.path("data", "citations_by_citation_year.csv")
cityear_paper_file <- file.path("data", "citations_by_citation_year_by_paper.csv")

# =========================
# CARGA DE DATOS
# =========================
#authors_summary <- read_csv(authors_file, show_col_types = FALSE)
pubs              <- read_csv(pubs_file, show_col_types = FALSE)
cit_pubyear       <- read_csv(pubyear_file, show_col_types = FALSE)
cit_cityear       <- read_csv(cityear_file, show_col_types = FALSE)
cit_cityear_paper <- read_csv(cityear_paper_file, show_col_types = FALSE)

# =========================
# COLUMNAS DE DEPARTAMENTO
# =========================
dept_cols <- c(
  "Electrónica",
  "Cs. Básicas",
  "Biomédica",
  "Mecánica",
  "Humanidades",
  "Industriales",
  "Cs. de datos",
  "Informática", 
  "IA"
)

# por seguridad: dejar solo las que realmente existan
dept_cols <- intersect(dept_cols, names(authors_summary))


# Me quedo con investigadores con status ok
investigadores <- authors_summary %>%
  filter(status == "ok") %>%
  arrange(input_name) %>%
  pull(input_name)

# =========================
# UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .metric-card {
        background: #f8f9fa;
        border-radius: 10px;
        padding: 14px;
        margin-bottom: 12px;
        border: 1px solid #e5e5e5;
        min-height: 95px;
      }
      .metric-label {
        font-size: 13px;
        color: #666;
        margin-bottom: 6px;
      }
      .metric-value {
        font-size: 24px;
        font-weight: 700;
        color: #1f2d3d;
      }
      .author-box {
        background: #ffffff;
        border-radius: 10px;
        padding: 14px 18px;
        margin-bottom: 15px;
        border: 1px solid #e5e5e5;
      }
      .author-title {
        font-size: 28px;
        font-weight: 700;
        margin-bottom: 4px;
      }
      .author-subtitle {
        font-size: 14px;
        color: #666;
      }
      .help-box {
        background: #fcfcfc;
        border-radius: 10px;
        padding: 18px 20px;
        margin-top: 20px;
        margin-bottom: 20px;
        border: 1px solid #e5e5e5;
      }
      .help-box h3 {
        margin-top: 0;
      }
      .help-box h4 {
        margin-top: 18px;
        margin-bottom: 8px;
      }
      .help-box p {
        margin-bottom: 10px;
        line-height: 1.5;
      }
    "))
  ),
  
  titlePanel("Producción científica de FI"),
  
  tabsetPanel(
    tabPanel(
      "Investigador",
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "investigador",
            label = "Elegir investigador",
            choices = investigadores,
            selected = investigadores[1]
          ),
          
          hr(),
          
          h4("Filtros de gráficos"),
          
          uiOutput("ui_year_range"),
          
          width = 3
        ),
        
        mainPanel(
          uiOutput("author_header"),
          
          fluidRow(
            column(width = 2, uiOutput("box_works")),
            column(width = 2, uiOutput("box_citations")),
            column(width = 2, uiOutput("box_cpp")),
            column(width = 2, uiOutput("box_fwci")),
            column(width = 2, uiOutput("box_hindex")),
            column(width = 2, uiOutput("box_top10"))
          ),
          
          fluidRow(
            column(width = 4, uiOutput("box_oldest")),
            column(width = 8)
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 12,
              h3("Evolución anual"),
              DTOutput("tabla_evolucion_anual")
            )
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              h3("Citas por año de publicación"),
              plotlyOutput("plot_pubyear", height = "350px")
            ),
            column(
              width = 6,
              h3("Citas por año de citación"),
              plotlyOutput("plot_cityear", height = "350px")
            )
          ),
          
          br(),
          hr(),
          
          fluidRow(
            column(
              width = 9,
              h3("Publicaciones")
            ),
            column(
              width = 3,
              br(),
              downloadButton("download_papers_excel", "Descargar Excel")
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              DTOutput("tabla_papers")
            )
          ),
          
          br(),
          fluidRow(
            column(
              width = 12,
              div(
                class = "help-box",
                h3("¿Qué significa cada indicador?"),
                
                h4("1. Scholarly Output"),
                p("Indica la cantidad total de publicaciones académicas asociadas al investigador en la fuente consultada. En esta app funciona como una aproximación al volumen de producción científica relevada."),
                
                h4("2. Citations"),
                p("Es la cantidad total de citas recibidas por el conjunto de publicaciones del investigador. Una cita ocurre cuando otro trabajo académico referencia uno de sus papers."),
                
                h4("3. Citations per Publication"),
                p("Es el promedio de citas por publicación. Se calcula como el total de citas dividido por la cantidad de trabajos. Ayuda a comparar impacto relativo entre investigadores con distinto volumen de producción."),
                
                h4("4. Field-Weighted Citation Impact"),
                p("Es un indicador de impacto relativo normalizado por área temática. Un valor cercano a 1 indica desempeño similar al promedio esperado para trabajos comparables; valores mayores a 1 indican impacto por encima del promedio, y menores a 1 indican impacto por debajo del promedio. En esta app se estima a partir de los datos disponibles en OpenAlex."),
                
                h4("5. h-index"),
                p("El índice h busca combinar productividad e impacto. Un investigador tiene índice h = h si posee al menos h publicaciones con al menos h citas cada una. Por ejemplo, un h-index de 10 significa que tiene 10 trabajos con 10 o más citas."),
                
                h4("6. Output in Top 10%"),
                p("Cuenta cuántas publicaciones del investigador se ubican dentro del 10% superior en desempeño de citación, considerando comparaciones normalizadas por campo. Es una forma de identificar trabajos especialmente influyentes dentro de su área."),
                
                h4("7. Oldest publication (since 1996)"),
                p("Muestra el año de la publicación más antigua detectada en la base, considerando solamente trabajos desde 1996 en adelante. Sirve como referencia aproximada de antigüedad o trayectoria visible en la fuente analizada."),
                
                h4("8. Histograma: Citas por año de publicación"),
                p("Este gráfico agrupa las publicaciones según el año en que fueron publicadas y muestra la suma de citas que acumulan esos trabajos. Permite ver qué cohortes de publicación concentraron mayor impacto. En la versión interactiva, cada barra puede desagregarse por paper."),
                
                h4("9. Histograma: Citas por año de citación"),
                p("Este gráfico muestra cuántas citas recibió el investigador en cada año calendario. A diferencia del gráfico anterior, aquí el eje temporal representa el año en que se recibieron las citas, no el año en que se publicaron los trabajos. Sirve para visualizar la dinámica reciente del impacto."),
                
                h4("Aclaración importante"),
                p("Estas métricas dependen de la cobertura y estructura de la base utilizada. Por eso deben interpretarse como indicadores bibliométricos aproximados y comparativos, no como una medida absoluta o exhaustiva de calidad científica.")
              )
            )
          )
        )
      )
    ),

    
    tabPanel(
      "Facultad",
      
      br(),
      
      fluidRow(
        column(
          width = 4,
          h4("Rango de años"),
          uiOutput("ui_year_range_dept")
        )
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 12,
          h3("Evolución anual agregada"),
          DTOutput("tabla_evolucion_departamento")
        )
      ),
      
      br(),
      hr(),
      fluidRow(
        column(
          width = 12,
          h3("Investigadores por departamento"),
          plotlyOutput("plot_departamentos_donut", height = "550px")
        )
      ),
      
      br(),
      hr(),
      
      fluidRow(
        column(
          width = 9,
          h3("Indicadores por investigador")
        ),
        column(
          width = 3,
          br(),
          downloadButton("download_investigadores_excel", "Descargar Excel")
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          DTOutput("tabla_investigadores_facultad")
        )
      ),
      
      br(),
      hr(),
      
      fluidRow(
        column(
          width = 12,
          h3("Doctorado"),
          p(HTML("<b>Año de comienzo de la carrera:</b> 2017"))
        )
      ),
      fluidRow(
        column(
          width = 6,
          h4("Estado actual de los doctorandos"),
          plotlyOutput("plot_doctorado_status", height = "420px")
        ),
        column(
          width = 6,
          h4("Doctorandos en curso por departamento"),
          plotlyOutput("plot_doctorado_departamento", height = "420px")
        )
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 12,
          h4("Tiempo de permanencia en doctorado (en curso)"),
          plotlyOutput("plot_doctorado_permanencia", height = "350px")
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {

  # -------------------------
  # Slider de años para pestaña Departamento
  # -------------------------
  output$ui_year_range_dept <- renderUI({
    years1 <- suppressWarnings(as.numeric(cit_pubyear$year))
    years2 <- suppressWarnings(as.numeric(cit_cityear$citation_year))
    
    all_years <- c(years1, years2)
    all_years <- all_years[!is.na(all_years)]
    
    req(length(all_years) > 0)
    
    min_year <- min(all_years)
    max_year <- max(all_years)
    
    sliderInput(
      "year_range_dept",
      label = NULL,
      min = min_year,
      max = max_year,
      value = c(max(2015, min_year), max_year),
      sep = ""
    )
  })
  
  investigadores_facultad_tabla <- reactive({
    req(length(dept_cols) > 0)
    
    df <- authors_summary %>%
      filter(status == "ok") %>%
      distinct(input_name, .keep_all = TRUE) %>%
      mutate(across(all_of(dept_cols), to_character_safe))
    
    # construir string de departamentos por investigador
    departamentos_por_fila <- apply(df[, dept_cols, drop = FALSE], 1, function(x) {
      marcas <- trimws(tolower(as.character(x)))
      deps <- dept_cols[marcas %in% c("x", "si", "sí")]
      if (length(deps) == 0) return("Sin departamento")
      paste(deps, collapse = ", ")
    })
    
    df %>%
      mutate(
        Departamento = departamentos_por_fila,
        Investigador = input_name,
        works_count = ifelse(is.na(works_count), 0, works_count),
        cited_by_count = ifelse(is.na(cited_by_count), 0, cited_by_count),
        citations_per_publication = round(ifelse(is.na(citations_per_publication), 0, citations_per_publication), 2),
        field_weighted_citation_impact = round(ifelse(is.na(field_weighted_citation_impact), 0, field_weighted_citation_impact), 2),
        h_index = ifelse(is.na(h_index), 0, h_index),
        output_in_top_10_percent = ifelse(is.na(output_in_top_10_percent), 0, output_in_top_10_percent),
        oldest_publication_since_1996 = ifelse(is.na(oldest_publication_since_1996), NA, oldest_publication_since_1996)
      ) %>%
      select(
        Investigador,
        Departamento,
        works_count,
        cited_by_count,
        citations_per_publication,
        output_in_top_10_percent
      )
  })
  doctorandos_base <- reactive({
    doctorandos
  })
  
  doctorandos_en_curso_departamento <- reactive({
    doctorandos_base() %>%
      mutate(
        status_suspendido_flag = !is.na(Status_suspendido) &
          trimws(tolower(Status_suspendido)) == "x",
        status_graduado_flag = !is.na(Status_graduado) &
          trimws(tolower(Status_graduado)) == "x",
        Departamento = trimws(as.character(Departamento))
      ) %>%
      filter(!status_suspendido_flag, !status_graduado_flag) %>%
      filter(!is.na(Departamento), Departamento != "") %>%
      group_by(Departamento) %>%
      summarise(
        n = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(n)) %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        label_text = paste0(
          Departamento, "<br>",
          n, " doctorandos<br>",
          pct, "%"
        )
      )
  })
  
  # -------------------------
  # Evolución anual global: papers por año
  # -------------------------
  papers_by_year_all <- reactive({
    req(input$year_range_dept)
    
    pubs %>%
      mutate(
        year = as.numeric(year),
        cited_by_count = as.numeric(cited_by_count),
        fwci = suppressWarnings(as.numeric(fwci)),
        is_in_top_10_percent = as.logical(is_in_top_10_percent)
      ) %>%
      filter(
        !is.na(year),
        year >= input$year_range_dept[1],
        year <= input$year_range_dept[2]
      ) %>%
      group_by(year) %>%
      summarise(
        papers = n(),
        avg_citations = mean(cited_by_count, na.rm = TRUE),
        fwci_mean = mean(fwci, na.rm = TRUE),
        top10 = sum(is_in_top_10_percent, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(year)
  })
  
  # -------------------------
  # Evolución anual global: citas recibidas por año
  # -------------------------
  citations_received_by_year_all <- reactive({
    req(input$year_range_dept)
    
    cit_cityear %>%
      mutate(
        citation_year = as.numeric(citation_year),
        citations = as.numeric(citations)
      ) %>%
      filter(
        !is.na(citation_year),
        !is.na(citations),
        citation_year >= input$year_range_dept[1],
        citation_year <= input$year_range_dept[2]
      ) %>%
      group_by(citation_year) %>%
      summarise(
        citations = sum(citations, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(citation_year)
  })
  
  # -------------------------
  # Evolución anual global: número acumulado de investigadores
  # -------------------------
  investigadores_acumulados_por_anio <- reactive({
    req(input$year_range_dept)
    
    years_seq <- seq(input$year_range_dept[1], input$year_range_dept[2])
    
    ingresos <- authors_summary %>%
      distinct(input_name, .keep_all = TRUE) %>%
      mutate(Anio_ingreso_FI = suppressWarnings(as.numeric(Anio_ingreso_FI))) %>%
      filter(!is.na(Anio_ingreso_FI))
    
    df <- data.frame(year = years_seq)
    
    df$n_investigadores <- sapply(
      years_seq,
      function(y) sum(ingresos$Anio_ingreso_FI <= y, na.rm = TRUE)
    )
    
    df
  })
  # -------------------------
  # Tabla transpuesta global
  # -------------------------
  evolucion_departamento_tabla <- reactive({
    req(input$year_range_dept)
    
    df_papers <- papers_by_year_all()
    
    df_citations <- citations_received_by_year_all() %>%
      rename(
        year = citation_year,
        citations_received = citations
      )
    
    df_investigadores <- investigadores_acumulados_por_anio() %>%
      rename(
        nro_investigadores = n_investigadores
      )
    
    df <- full_join(df_papers, df_citations, by = "year") %>%
      full_join(df_investigadores, by = "year") %>%
      arrange(year) %>%
      mutate(
        papers = ifelse(is.na(papers), 0, papers),
        citations_received = ifelse(is.na(citations_received), 0, citations_received),
        avg_citations = ifelse(is.na(avg_citations), NA, round(avg_citations, 2)),
        fwci_mean = ifelse(is.na(fwci_mean), NA, round(fwci_mean, 2)),
        top10 = ifelse(is.na(top10), 0, top10),
        nro_investigadores = ifelse(is.na(nro_investigadores), 0, nro_investigadores)
      ) %>%
      select(
        year,
        papers,
        citations_received,
        avg_citations,
        fwci_mean,
        top10,
        nro_investigadores
      )
    
    req(nrow(df) > 0)
    
    df_t <- as.data.frame(t(df[, -1, drop = FALSE]))
    colnames(df_t) <- df$year
    df_t <- tibble::rownames_to_column(df_t, var = "Métrica")
    
    df_t$Métrica <- c(
      "Papers publicados",
      "Citas recibidas",
      "Citas promedio por paper",
      "FWCI promedio",
      "Papers Top 10%",
      "Nro de investigadores"
    )
    
    df_t
  })
  
  
    # -------------------------
  # Resumen por departamento
  # -------------------------

  dept_summary <- reactive({
    req(length(dept_cols) > 0)
    
    authors_summary %>%
      distinct(input_name, .keep_all = TRUE) %>%
      select(input_name, all_of(dept_cols)) %>%
      mutate(across(all_of(dept_cols), to_character_safe)) %>%
      pivot_longer(
        cols = all_of(dept_cols),
        names_to = "departamento",
        values_to = "marca"
      ) %>%
      mutate(
        marca = trimws(tolower(as.character(marca))),
        marca = ifelse(is.na(marca), "", marca)
      ) %>%
      filter(marca %in% c("x", "si", "sí")) %>%
      group_by(departamento) %>%
      summarise(
        n_investigadores = n_distinct(input_name),
        .groups = "drop"
      ) %>%
      arrange(desc(n_investigadores))
  })

  output$plot_departamentos_donut <- renderPlotly({
    df <- dept_summary()
    
    validate(
      need(nrow(df) > 0, "No hay datos válidos de departamentos para graficar.")
    )
    
    df <- df %>%
      mutate(
        pct = round(100 * n_investigadores / sum(n_investigadores), 1),
        text_full = paste0(
          departamento, "<br>",
          pct, "%<br>",
          "n=", n_investigadores
        )
      )
    
    plot_ly(
      data = df,
      labels = ~departamento,
      values = ~n_investigadores,
      type = "pie",
      hole = 0.62,
      
      # 🔥 TODO el control del texto
      text = ~text_full,
      textinfo = "text",
      textposition = "outside",
      
      # 🔥 MÁS GRANDE
      outsidetextfont = list(color = "#333", size = 16),
      
      marker = list(
        colors = c(
          "#4E79A7", "#F28E2B", "#E15759",
          "#76B7B2", "#59A14F", "#EDC948"
        ),
        line = list(color = "white", width = 2)
      ),
      
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Investigadores: %{value}<br>",
        "Porcentaje: %{percent}<extra></extra>"
      ),
      
      showlegend = FALSE
    ) %>%
      layout(
        margin = list(t = 20, b = 170, l = 20, r = 20)
      )
  })
  
  
  output$plot_doctorado_departamento <- renderPlotly({
    df <- doctorandos_en_curso_departamento()
    
    validate(
      need(nrow(df) > 0, "No hay doctorandos en curso con departamento informado.")
    )
    
    df <- df %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        text_full = paste0(
          Departamento, "<br>",
          pct, "%<br>",
          "n=", n
        )
      )
    
    plot_ly(
      data = df,
      labels = ~Departamento,
      values = ~n,
      type = "pie",
      hole = 0.62,
      text = ~text_full,
      textinfo = "text",
      textposition = "outside",
      outsidetextfont = list(color = "#333", size = 16),
      marker = list(
        colors = c(
          "#4E79A7", "#F28E2B", "#E15759",
          "#76B7B2", "#59A14F", "#EDC948",
          "#B07AA1", "#9C755F"
        ),
        line = list(color = "white", width = 2)
      ),
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Doctorandos: %{value}<br>",
        "Porcentaje: %{percent}<extra></extra>"
      ),
      showlegend = FALSE
    ) %>%
      layout(
        margin = list(t = 80, b = 100, l = 20, r = 20)
        #margin = list(t = 20, b = 20, l = 20, r = 20)
      )
  })
  
  # -------------------------
  # Resumen de doctorandos
  # -------------------------
  doctorandos_base <- reactive({
    doctorandos
  })
  
  doctorandos_permanencia <- reactive({
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    doctorandos_base() %>%
      filter(!is.na(inicio_year)) %>%
      mutate(
        status_suspendido_flag = !is.na(Status_suspendido) &
          trimws(tolower(Status_suspendido)) == "x",
        status_graduado_flag = !is.na(Status_graduado) &
          trimws(tolower(Status_graduado)) == "x"
      ) %>%
      filter(!status_suspendido_flag, !status_graduado_flag) %>%
      mutate(
        permanencia = current_year - inicio_year,
        etiqueta = `Apellido y nombre`      ) %>%
      filter(!is.na(permanencia), permanencia >= 0) %>%
      arrange(permanencia, inicio_year, `Apellido y nombre`)
  })

doctorandos_status_resumen <- reactive({
  df <- doctorandos_base()
  
  n_suspendido <- sum(
    !is.na(df$Status_suspendido) &
      trimws(tolower(df$Status_suspendido)) == "x",
    na.rm = TRUE
  )
  
  n_graduado <- sum(
    !is.na(df$Status_graduado) &
      trimws(tolower(df$Status_graduado)) == "x",
    na.rm = TRUE
  )
  
  # En curso = no suspendido y no graduado
  n_total <- nrow(df)
  n_en_curso <- n_total - n_suspendido - n_graduado
  
  tibble(
    status = c("En curso", "Suspendido", "Graduado"),
    n = c(n_en_curso, n_suspendido, n_graduado)
  ) %>%
    mutate(
      pct = round(100 * n / sum(n), 1),
      label_text = paste0(status, "<br>", n, " doctorandos<br>", pct, "%")
    )
})  
output$plot_doctorado_status <- renderPlotly({
  df <- doctorandos_status_resumen()
  req(nrow(df) > 0)
  
  df <- df %>%
    mutate(
      pct = round(100 * n / sum(n), 1),
      text_full = paste0(
        status, "<br>",
        pct, "%<br>",
        "n=", n
      )
    )
  
  plot_ly(
    data = df,
    labels = ~status,
    values = ~n,
    type = "pie",
    hole = 0.62,
    
    # 🔥 estilo nuevo (igual a los otros)
    text = ~text_full,
    textinfo = "text",
    textposition = "outside",
    
    outsidetextfont = list(color = "#333", size = 16),
    
    marker = list(
      colors = c(
        "#2ca25f",  # verde
        "#de2d26",  # rojo
        "#3182bd"   # azul
      ),
      line = list(color = "white", width = 2)
    ),
    
    hovertemplate = paste(
      "<b>%{label}</b><br>",
      "Cantidad: %{value}<br>",
      "Porcentaje: %{percent}<extra></extra>"
    ),
    
    showlegend = FALSE
  ) %>%
    layout(
      margin = list(t = 80, b = 20, l = 20, r = 20)
    )
})

output$plot_doctorado_permanencia <- renderPlotly({
  df <- doctorandos_permanencia()
  req(nrow(df) > 0)
  
  # asegurar que aparezca una barra por cada año entero
  anios <- seq(min(df$permanencia, na.rm = TRUE), max(df$permanencia, na.rm = TRUE))
  
  df <- df %>%
    mutate(
      permanencia_f = factor(permanencia, levels = anios),
      y = 1,
      hover_text = paste0(
        "<b>Doctorando:</b> ", `Apellido y nombre`,
        "<br><b>Año de inicio:</b> ", inicio_year,
        "<br><b>Permanencia:</b> ", permanencia, " años"
      )
    )
  
  p <- ggplot(
    df,
    aes(
      x = permanencia_f,
      y = y,
      fill = etiqueta,
      text = hover_text
    )
  ) +
    geom_col(color = "white", width = 0.8) +
    geom_text(
      aes(label = etiqueta),
      position = position_stack(vjust = 0.5),
      size = 3,
      lineheight = 0.9
    ) +
    labs(
      x = "Años de permanencia",
      y = "Cantidad de doctorandos"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11)
    )
  
  ggplotly(p, tooltip = "text") %>%
    layout(
      showlegend = FALSE
    )
})
  # -------------------------
  # Investigador seleccionado
  # -------------------------
  autor_sel <- reactive({
    req(input$investigador)
    
    authors_summary %>%
      filter(input_name == input$investigador) %>%
      slice(1)
  })
  
  # -------------------------
  # Datos filtrados
  # -------------------------
  papers_filtrados <- reactive({
    req(input$investigador)
    
    pubs %>%
      filter(input_name == input$investigador) %>%
      arrange(desc(year), title)
  })
  
  pubyear_filtrado_raw <- reactive({
    req(input$investigador)
    
    cit_pubyear %>%
      filter(input_name == input$investigador) %>%
      mutate(year = as.numeric(year)) %>%
      arrange(year)
  })
  
  cityear_filtrado_raw <- reactive({
    req(input$investigador)
    
    cit_cityear %>%
      filter(input_name == input$investigador) %>%
      mutate(citation_year = as.numeric(citation_year)) %>%
      arrange(citation_year)
  })
  
  # -------------------------
  # Sliders dinámicos
  # -------------------------
  output$ui_year_range <- renderUI({
    df1 <- pubyear_filtrado_raw()
    df2 <- cityear_filtrado_raw()
    
    years1 <- df1$year
    years2 <- df2$citation_year
    
    all_years <- c(years1, years2)
    all_years <- all_years[!is.na(all_years)]
    
    req(length(all_years) > 0)
    
    min_year <- min(all_years)
    max_year <- max(all_years)
    
    sliderInput(
      "year_range",
      "Rango de años",
      min = min_year,
      max = max_year,
      value = c(min_year, max_year),
      sep = ""
    )
  })
  
  cityear_paper_filtrado <- reactive({
    req(input$investigador, input$year_range)
    
    cit_cityear_paper %>%
      filter(input_name == input$investigador) %>%
      mutate(
        citation_year = as.numeric(citation_year),
        citations = as.numeric(citations),
        title = ifelse(is.na(title), "Sin título", title)
      ) %>%
      filter(
        !is.na(citation_year),
        !is.na(citations),
        citation_year >= input$year_range[1],
        citation_year <= input$year_range[2]
      ) %>%
      arrange(citation_year, title)
  })
  
  pubyear_filtrado <- reactive({
    df <- pubyear_filtrado_raw()
    req(input$year_range)
    
    df %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })
  
  cityear_filtrado <- reactive({
    df <- cityear_filtrado_raw()
    req(input$year_range)
    
    df %>%
      filter(
        citation_year >= input$year_range[1],
        citation_year <= input$year_range[2]
      )
  })
  
  # -------------------------
  # EVOLUCIÓN ANUAL
  # -------------------------
  
  papers_by_year <- reactive({
    req(input$investigador, input$year_range)
    
    pubs %>%
      filter(input_name == input$investigador) %>%
      mutate(
        year = as.numeric(year),
        cited_by_count = as.numeric(cited_by_count),
        fwci = suppressWarnings(as.numeric(fwci)),
        is_in_top_10_percent = as.logical(is_in_top_10_percent)
      ) %>%
      filter(
        !is.na(year),
        year >= input$year_range[1],
        year <= input$year_range[2]
      ) %>%
      group_by(year) %>%
      summarise(
        papers = n(),
        avg_citations = mean(cited_by_count, na.rm = TRUE),
        fwci_mean = mean(fwci, na.rm = TRUE),
        top10 = sum(is_in_top_10_percent, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(year)
  })
  
  citations_received_by_year <- reactive({
    req(input$investigador, input$year_range)
    
    cit_cityear %>%
      filter(input_name == input$investigador) %>%
      mutate(
        citation_year = as.numeric(citation_year),
        citations = as.numeric(citations)
      ) %>%
      filter(
        !is.na(citation_year),
        !is.na(citations),
        citation_year >= input$year_range[1],
        citation_year <= input$year_range[2]
      ) %>%
      arrange(citation_year)
  })
  
  evolucion_anual_tabla <- reactive({
    req(input$investigador, input$year_range)
    
    df_papers <- papers_by_year()
    
    df_citations <- citations_received_by_year() %>%
      rename(
        year = citation_year,
        citations_received = citations
      )
    
    df <- full_join(df_papers, df_citations, by = "year") %>%
      arrange(year) %>%
      mutate(
        papers = ifelse(is.na(papers), 0, papers),
        citations_received = ifelse(is.na(citations_received), 0, citations_received),
        avg_citations = ifelse(is.na(avg_citations), NA, round(avg_citations, 2)),
        fwci_mean = ifelse(is.na(fwci_mean), NA, round(fwci_mean, 2)),
        top10 = ifelse(is.na(top10), 0, top10)
      ) %>%
      select(
        year,
        papers,
        citations_received,
        avg_citations,
        fwci_mean,
        top10
      )
    
    # pasar a formato transpuesto
    df_t <- as.data.frame(t(df[, -1, drop = FALSE]))
    colnames(df_t) <- df$year
    
    df_t <- tibble::rownames_to_column(df_t, var = "Métrica")
    
    # nombres más amigables
    df_t$Métrica <- c(
      "Papers publicados",
      "Citas recibidas",
      "Citas promedio por paper",
      "FWCI promedio",
      "Papers Top 10%"
    )
    
    df_t
  })
  # -------------------------
  # Header del autor
  # -------------------------
  output$author_header <- renderUI({
    a <- autor_sel()
    
    display_name <- a$display_name
    input_name   <- a$input_name
    orcid        <- a$orcid
    anio_ingreso <- a$Anio_ingreso_FI
    
    tags$div(
      class = "author-box",
      tags$div(
        class = "author-title",
        ifelse(is.na(display_name) | display_name == "", input_name, display_name)
      ),
      tags$div(
        class = "author-subtitle",
        HTML(
          paste0(
            "<b>Nombre en listado:</b> ", input_name,
            "<br><b>ORCID:</b> ", ifelse(is.na(orcid) | orcid == "", "No disponible", orcid),
            "<br><b>Año de ingreso a FI:</b> ", ifelse(is.na(anio_ingreso), "No disponible", anio_ingreso)
          )
        )
      )
    )
  }) 
  # -------------------------
  # Cajas de métricas
  # -------------------------
  make_metric_box <- function(label, value) {
    tags$div(
      class = "metric-card",
      tags$div(class = "metric-label", label),
      tags$div(class = "metric-value", value)
    )
  }
  
  fmt_num <- function(x, digits = 2) {
    if (is.null(x) || length(x) == 0) return("NA")
    
    x <- suppressWarnings(as.numeric(x))
    
    if (is.na(x)) return("NA")
    
    format(round(x, digits), nsmall = digits, trim = TRUE)
  }
  
  output$box_works <- renderUI({
    a <- autor_sel()
    make_metric_box("Scholarly Output", ifelse(is.na(a$works_count), "NA", a$works_count))
  })
  
  output$box_citations <- renderUI({
    a <- autor_sel()
    make_metric_box("Citations", ifelse(is.na(a$cited_by_count), "NA", a$cited_by_count))
  })
  
  output$box_cpp <- renderUI({
    a <- autor_sel()
    make_metric_box("Citations per Publication", fmt_num(a$citations_per_publication, 2))
  })
  
  output$box_fwci <- renderUI({
    a <- autor_sel()
    make_metric_box("Field-Weighted Citation Impact", fmt_num(a$field_weighted_citation_impact, 2))
  })
  
  output$box_hindex <- renderUI({
    a <- autor_sel()
    make_metric_box("h-index", ifelse(is.na(a$h_index), "NA", a$h_index))
  })
  
  output$box_top10 <- renderUI({
    a <- autor_sel()
    make_metric_box("Output in Top 10%", ifelse(is.na(a$output_in_top_10_percent), "NA", a$output_in_top_10_percent))
  })
  
  output$box_oldest <- renderUI({
    a <- autor_sel()
    make_metric_box("Oldest publication (since 1996)", ifelse(is.na(a$oldest_publication_since_1996), "NA", a$oldest_publication_since_1996))
  })
  
  output$tabla_investigadores_facultad <- renderDT({
    df <- investigadores_facultad_tabla()
    req(nrow(df) > 0)
    
    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        autoWidth = FALSE,
        scrollX = TRUE,
        ordering = TRUE,
        dom = "tip",
        order = list(list(2, "desc"))
    ),
      colnames = c(
        "Investigador (Departamento)",
        "Departamento",
        "Scholarly Output",
        "Citations",
        "Citations per Publication",
        "Output in Top 10%"
      )
    )
  })
  
  output$tabla_evolucion_departamento <- renderDT({
    df <- evolucion_departamento_tabla()
    req(nrow(df) > 0)
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        dom = "t"
      )
    )
  })
  
  output$tabla_evolucion_anual <- renderDT({
    df <- evolucion_anual_tabla()
    req(nrow(df) > 0)
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        dom = "t"
      )
    )
  })
  
  output$download_investigadores_excel <- downloadHandler(
    filename = function() {
      paste0("indicadores_investigadores_facultad.xlsx")
    },
    content = function(file) {
      df <- investigadores_facultad_tabla()
      writexl::write_xlsx(df, path = file)
    }
  )
  
  output$download_evolucion_excel <- downloadHandler(
    filename = function() {
      nombre <- gsub("[^A-Za-z0-9_-]", "_", input$investigador)
      paste0("evolucion_anual_", nombre, ".xlsx")
    },
    content = function(file) {
      df <- evolucion_anual_tabla()
      writexl::write_xlsx(df, path = file)
    }
  )    
  # -------------------------
  # Tabla de papers
  # -------------------------
  output$tabla_papers <- renderDT({
    df <- papers_filtrados()
    
    req(nrow(df) > 0)
    
    df <- df %>%
      mutate(
        doi = ifelse(
          is.na(doi) | doi == "",
          "",
          paste0('<a href="', doi, '" target="_blank">', doi, '</a>')
        )
      ) %>%
      select(title, year, venue, doi, cited_by_count)
    
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      colnames = c(
        "Título",
        "Año",
        "Revista / Venue",
        "DOI",
        "Citas"
      )
    )
  })
  
  output$download_papers_excel <- downloadHandler(
    filename = function() {
      nombre <- gsub("[^A-Za-z0-9_-]", "_", input$investigador)
      paste0("publicaciones_", nombre, ".xlsx")
    },
    content = function(file) {
      
      df <- papers_filtrados() %>%
        select(title, year, venue, doi, cited_by_count)
      
      writexl::write_xlsx(df, path = file)
    }
  )
  
  # -------------------------
  # Plot: año de publicación
  # -------------------------
  
  output$plot_pubyear <- renderPlotly({
    req(input$investigador, input$year_range)
    
    df <- papers_filtrados() %>%
      mutate(
        year = as.numeric(year),
        cited_by_count = as.numeric(cited_by_count),
        title = ifelse(is.na(title), "Sin título", title),
        hover_text = paste0(
          "<b>Título:</b> ", title,
          "<br><b>Año:</b> ", year,
          "<br><b>Citas:</b> ", cited_by_count
        )
      ) %>%
      filter(
        !is.na(year),
        !is.na(cited_by_count),
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    
    req(nrow(df) > 0)
    
    p <- ggplot(
      df,
      aes(
        x = factor(year),
        y = cited_by_count,
        fill = title,
        text = hover_text
      )
    ) +
      geom_col() +
      labs(
        x = "Año de publicación",
        y = "Citas totales"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  # -------------------------
  # Plot: año de citación
  # -------------------------
  output$plot_cityear <- renderPlotly({
    df <- cityear_paper_filtrado()
    req(nrow(df) > 0)
    
    df <- df %>%
      mutate(
        hover_text = paste0(
          "<b>Título:</b> ", title,
          "<br><b>Año de citación:</b> ", citation_year,
          "<br><b>Citas ese año:</b> ", citations
        )
      )
    
    p <- ggplot(
      df,
      aes(
        x = factor(citation_year),
        y = citations,
        fill = title,
        text = hover_text
      )
    ) +
      geom_col() +
      labs(
        x = "Año de citación",
        y = "Citas"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })

}

# =========================
# APP
# =========================
shinyApp(ui = ui, server = server)