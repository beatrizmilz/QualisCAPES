library(shiny)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)

qualis_capes <- readr::read_rds("output/qualis_capes.rds")

ui <- dashboardPage(
  
  # Título
  dashboardHeader(title = "Pesquisa de periódicos", titleWidth = 260),
  ## Conteúdo da sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pesquisa de Periódicos", tabName = "dashboard", icon = icon("search")),
      menuItem("Sobre o projeto", tabName = "sobre", icon = icon("graduation-cap")),
      menuItem(" Acesse o código", tabName = "github", icon = icon("github-square"))
    )
  ),
  
  ## Conteúdo da parte principal
  ## Body content
  dashboardBody(
    tabItems(
      # Tab do dashboard - pesquisa
      tabItem(tabName = "dashboard",
              # Colocar aqui o conteúdo da página da pesquisa
              h2("Pesquisa de periódicos"),
              strong("O filtro ainda não está funcionando como deveria! Em construção."),
              br(),br(),
               fluidRow( #uma linha
                box(#caixa em uma linha
                  pickerInput(
                    inputId = "area_de_avaliacao_i",
                    label = "Área de Avaliação",
                    choices =  sort(unique(qualis_capes$area_de_avaliacao)), 
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE,
                    selected = TRUE
                  )), 
                
                box(
                    sliderTextInput(
                    inputId = "estrato_i",
                    label = "Escolha a classificação no periódicos CAPES",
                   choices = sort(unique(qualis_capes$estrato)),
                   selected = c("A1", "C"),
                   grid = TRUE
                  )
                ) # caixa em uma linha
              ),
              
              # Adiciona a tabela
                DTOutput("tabela_periodicos")
      ),
      
      # Tab do projeto
      tabItem(tabName = "sobre",
              # Colocar aqui a parte do projeto
              h2("Sobre o projeto"),
              "Em breve"
      ),
      # Tab do github  
      tabItem(tabName = "github",
              # Colocar aqui a parte do projeto
              h2("Acesse o código"),
              "Em breve"
      )
      
    )
  )
)



server <- function(input, output) {
  
  filtered_data <- reactive({
   qualis_capes <- qualis_capes %>%
     filter(estrato %in% input$estrato_i & 
              area_de_avaliacao %in% input$area_de_avaliacao_i) %>% 
     arrange(estrato) %>% 
     rename(ISSN = issn,
            `Título` = titulo,
            `Área de Avaliação` = area_de_avaliacao,
            Estrato = estrato,
            `Período de avaliação` = avaliacao) 

    
   qualis_capes
  })
  
  output$tabela_periodicos <- renderDT({
    filtered_data() 
    } , escape = FALSE)
  
  
}

shinyApp(ui, server)