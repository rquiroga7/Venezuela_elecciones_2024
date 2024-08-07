library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Visualización de los datos en las actas de resultadosconvzla.com"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select_estado", "Seleccione el Estado", NULL, multiple = TRUE),
      conditionalPanel(
        condition = "input.select_estado.indexOf('Todos') === -1",
        selectInput("select_municipio", "Seleccione el Municipio", NULL, multiple = TRUE)
      ),
      conditionalPanel(
        condition = "input.select_municipio.indexOf('Todos') === -1",
        selectInput("select_parroquia", "Seleccione la Parroquia", NULL, multiple = TRUE)
      )
    ),
    mainPanel(
      plotlyOutput("scatterplot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  # Load the data
  data <- read.csv("results.csv", header = TRUE)

# Update selectInput choices
updateSelectInput(session, "select_estado", choices = c("Todos", sort(unique(data$ESTADO))))

  # Create the "OTROS" column and remove the component columns
  data <- data %>%
    group_by(Codcv, ESTADO, municipio, parroquia) %>%
    summarise(
      Maduro = sum(Maduro),
      Gonzalez = sum(Gonzalez),
      OTROS = sum(Martinez) + sum(Bertucci) + sum(Brito) + sum(Ecarri) + sum(Fermin) + sum(Ceballos) + sum(Marquez) + sum(Rausseo) + sum(Otros),
      #for votantes keep first value
      votantes = first(votantes),
      .groups = "drop"
    ) %>%
    mutate(votos_totales = rowSums(across(Maduro:OTROS)))

# Update select_municipio choices based on selected estado
observeEvent(input$select_estado, {
  municipios <- unique(data$municipio[data$ESTADO %in% input$select_estado])
  if (length(municipios) == 1) {
    updateSelectInput(session, "select_municipio", choices = municipios, selected = municipios)
  } else {
    updateSelectInput(session, "select_municipio", choices = c("Todos", sort(municipios)), selected = "Todos")
  }
  updateSelectInput(session, "select_parroquia", selected = "Todos")
})

# Update select_parroquia choices based on selected municipio
observeEvent(input$select_municipio, {
  if ("Todos" %in% input$select_municipio) {
    updateSelectInput(session, "select_parroquia", choices = c("Todos", sort(unique(data$parroquia))))
  } else {
    updateSelectInput(session, "select_parroquia", choices = c("Todos", sort(unique(data$parroquia[data$municipio %in% input$select_municipio]))), selected = "Todos")
  }
})

# Define the reactive expression for the selected estado, municipio, and parroquia
selected_data <- reactive({
  if ("Todos" %in% input$select_estado) {
    data
  } else {
    filtered_data <- data %>% filter(ESTADO %in% input$select_estado)
    if ("Todos" %in% input$select_municipio) {
      filtered_data
    } else {
      filtered_data <- filtered_data %>% filter(municipio %in% input$select_municipio)
      if ("Todos" %in% input$select_parroquia) {
        filtered_data
      } else {
        filtered_data %>% filter(parroquia %in% input$select_parroquia)
      }
    }
  }
})

# Define the scatterplot
output$scatterplot <- renderPlotly({
  color_var <- if ("Todos" %in% input$select_estado) "ESTADO" else if ("Todos" %in% input$select_municipio) "municipio" else "parroquia"
  p <- ggplot(selected_data(), aes(x = Gonzalez, y = Maduro, color = get(color_var), text = paste("Codcv:", Codcv, "<br>ESTADO:", ESTADO, "<br>municipio:", municipio, "<br>parroquia:", parroquia, "<br>Maduro:", Maduro, "<br>Gonzalez:", Gonzalez, "<br>OTROS:", OTROS, "<br>votos_totales:", votos_totales))) +
    geom_point(alpha = 0.5, size = 1) +
    labs(color = color_var) +
    theme_classic()
  p <- ggplotly(p, tooltip = "text")
  p <- layout(p, legend = list(orientation = "h", x = 0.5, y = -0.45, xanchor = "center"))
  p
})
}

shinyApp(ui, server)