library(shiny)
library(bslib)
library(ggplot2)
library(readr)

ui <- page_sidebar(
  title = "Visualisation de données",
  sidebar = sidebar(
    # Upload du fichier
    fileInput("file", "Choisir un fichier CSV",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

    # Sélection de la variable (apparaît uniquement après chargement du fichier)
    uiOutput("var_select")
  ),

  # Zone principale avec le graphique
  card(
    plotOutput("plot")
  )
)

server <- function(input, output, session) {
  # Lecture réactive du fichier avec point-virgule comme séparateur
  data <- reactive({
    req(input$file)
    read_csv2(input$file$datapath)  # read_csv2 utilise ; comme séparateur
  })

  # UI dynamique pour sélectionner la variable
  output$var_select <- renderUI({
    req(data())
    selectInput("variable", "Choisir une variable à visualiser",
                choices = names(data()))
  })

  # Création du graphique
  output$plot <- renderPlot({
    req(input$variable)

    # Récupération du type de la variable sélectionnée
    var_type <- class(data()[[input$variable]])[1]

    # Création du graphique selon le type de variable
    if (var_type %in% c("numeric", "integer", "double")) {
      # Pour les variables numériques : boxplot
      ggplot(data(), aes_string(y = input$variable)) +
        geom_boxplot(fill = "lightblue", color = "darkblue") +
        theme_minimal() +
        labs(title = paste("Distribution de", input$variable),
             y = input$variable)
    } else {
      # Pour les variables catégorielles : diagramme en barres
      ggplot(data(), aes_string(x = input$variable)) +
        geom_bar(fill = "lightblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Distribution de", input$variable),
             x = input$variable,
             y = "Fréquence")
    }
  })
}

shinyApp(ui, server)
