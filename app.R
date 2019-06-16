library(shiny)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)

qualis_capes <- readr::read_rds("output/qualis_capes.rds")

ui <- dashboardPage(skin = "black",
                    
                    # Título
                    dashboardHeader(title = "Pesquisa de periódicos", titleWidth = 260),
                    ## Conteúdo da sidebar
                    dashboardSidebar(
                      width = 260,
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
                               
                                fluidRow( #uma linha
                                  box(#caixa em uma linha
                                    selectizeInput(
                                      inputId = "area_de_avaliacao_i",
                                      label = "Área de Avaliação",
                                      choices =  sort(unique(qualis_capes$area_de_avaliacao)), 
                                      multiple = TRUE,
                                      selected = FALSE
                                    )), 
                                  
                                  box(
                                    sliderTextInput(
                                      inputId = "estrato_i",
                                      label = "Escolha a classificação no periódicos CAPES",
                                      choices = sort(unique(qualis_capes$estrato)),
                                      selected = c("A1", "C"),
                                      grid = TRUE )
                                    
                                  ) # caixa em uma linha
                                ),
                                #adiciona os value boxes
                                valueBoxOutput("n_total_periodicos", width = 4), 
                                valueBoxOutput("n_periodicos_filtrados", width = 4), 
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
  
  filtra_varios <- function(data, lista) {
    purrr::reduce(lista, ~{
      filter(.x, stringr::str_detect(area_conc, .y))
    }, .init = data)
  }
  
  
  filtered_data <- reactive({
    
    req( input$area_de_avaliacao_i)
    
    qualis_capes2 <- qualis_capes %>%
      filter(as.character(estrato) >= as.character(input$estrato_i[1]), 
             as.character(estrato) <= as.character(input$estrato_i[2])) %>% 
      group_by(issn) %>% 
      mutate(area_conc = paste(area_de_avaliacao, collapse = " - ")) %>% 
      ungroup() %>% 
      filtra_varios(input$area_de_avaliacao_i) %>% 
      filter(area_de_avaliacao %in% input$area_de_avaliacao_i)  %>% 
      arrange(issn)
    
    
    qualis_capes2
    
    
  })
  
  output$n_total_periodicos <- renderValueBox({
    valueBox(
      value = length(unique(qualis_capes$issn)),
      subtitle =  "Número de periódicos cadastrados",
      icon = icon("fas fa-running"), #icone nao ta funcionando
      color = "blue"
    )
  })
  
  output$n_periodicos_filtrados <- renderValueBox({
    valueBox(
      value = length(unique(filtered_data()$issn)),
      subtitle =  "Número de periódicos filtrados",
      icon = icon("fas fa-running"), #icone nao ta funcionando
      color = "blue"
    )
  })
  
  output$tabela_periodicos <- renderDT({
    filtered_data()  %>%
      rename(ISSN = issn,
             `Título` = titulo,
             `Área de Avaliação` = area_de_avaliacao,
             Estrato = estrato,
             `Áreas em que o periódico foi avaliado` = area_conc) 
  } , escape = FALSE)
  
}

shinyApp(ui, server)