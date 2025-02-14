
# Cargar paquetes necesarios
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(rsconnect)

#Cargar bases
victimas_2024 <- read_excel("C:/Users/valen/OneDrive/Escritorio/cv/Secretaria de la Mujer/victimas_2024.xlsx")
View(victimas_2024)
victimas_estado <- read_excel("C:/Users/valen/OneDrive/Escritorio/cv/Secretaria de la Mujer/victimas_estado.xlsx")
View(victimas_estado)

victimas_nacional <- victimas_2024

# Limpiar nombres de columnas
names(victimas_estado) <- trimws(names(victimas_estado))

# Transformar datos nacionales para gráficos
victimas_nacional_long <- victimas_nacional[-1,] %>% 
  select(-Clave, -`Tipo de delito, subtipo y modalidad`, -Total) %>%
  pivot_longer(cols = everything(), names_to = "Mes", values_to = "Victimas") %>%
  mutate(Victimas = as.numeric(Victimas))

# Transformar datos por tipo de delito
victimas_delito <- victimas_nacional %>%
  select(`Tipo de delito, subtipo y modalidad`, Total) %>%
  filter(!is.na(`Tipo de delito, subtipo y modalidad`))

# UI de la app Shiny
ui <- fluidPage(
  titlePanel("Análisis de datos con visualizaciones basadas en la base de datos de víctimas del Secretariado Ejecutivo Nacional de Seguridad Pública (SESNSP)"),
  tags$h4("Datos: Secretariado Ejecutivo Nacional de Seguridad Pública (SESNSP)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("estado", "Selecciona un estado:", choices = unique(victimas_estado$Estado)),
      width = 3
    ),
    mainPanel(
      plotlyOutput("grafico_nacional"),
      plotlyOutput("grafico_estados"),
      plotlyOutput("grafico_delitos"),
      plotlyOutput("grafico_resultados_nacionales"),
      plotlyOutput("grafico_estado_detalle")
    )
  )
)

# Server de la app Shiny
server <- function(input, output) {
  # Gráfico de evolución mensual interactivo
  output$grafico_nacional <- renderPlotly({
    p <- ggplot(victimas_nacional_long, aes(x = Mes, y = Victimas)) +
      geom_line(color = "purple", size = 1) +
      geom_point(color = "purple", size = 2) +
      theme_minimal() +
      labs(title = "Evolución Mensual de Víctimas (2024)", x = "Mes", y = "Número de Víctimas") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p)
  })
  
  # Gráfico de víctimas por estado interactivo
  output$grafico_estados <- renderPlotly({
    top_3 <- victimas_estado %>% arrange(desc(`Total vítimas`)) %>% head(3)
    p <- ggplot(victimas_estado, aes(x = reorder(Estado, `Total vítimas`), y = `Total vítimas`)) +
      geom_bar(stat = "identity", aes(fill = Estado %in% top_3$Estado), show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "purple")) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Víctimas por Estado (2024)", x = "Estado", y = "Número de Víctimas") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p)
  })

  # Gráfico de víctimas por tipo de delito interactivo
  output$grafico_delitos <- renderPlotly({
    p <- ggplot(victimas_delito, aes(x = reorder(`Tipo de delito, subtipo y modalidad`, Total), y = Total)) +
      geom_bar(stat = "identity", fill = "darkred") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Número de Víctimas por Tipo de Delito", x = "Tipo de Delito", y = "Número de Víctimas") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p)
  })
  
  # Gráfico de violencia específica por estado
  output$grafico_estado_detalle <- renderPlotly({
    data_estado <- victimas_estado %>% filter(Estado == input$estado)
    p <- ggplot(data_estado, aes(x = names(data_estado)[-1], y = as.numeric(unlist(data_estado[-1])))) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Violencia en", input$estado), x = "Tipo de Delito", y = "Número de Víctimas") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p)
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

install.packages("rsconnect")

rsconnect::setAccountInfo(name='valentinabarros', token= '068CBB138F00EAEDFF83F077BD71EB2E', secret='<SECRET>')
rsconnect::setAccountInfo(name='valentinabarros', token='068CBB138F00EAEDFF83F077BD71EB2E', secret='0qZbNiOrJ29ImWvvbrPT7qs5NmeU+XjU5+SXzzkN')


setwd ("C:\Users\valen\OneDrive\Escritorio\cv\Secretaria de la Mujer\App_2.R")
library(rsconnect)
deployApp()

