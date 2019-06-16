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
                        menuItem("Saiba mais", tabName = "sobre", icon = icon("graduation-cap")),
                        menuItem("   Acesse o código", 
                                 href = "https://github.com/beatrizmilz/QualisCAPES",
                                 icon = icon("github-square")
                                 
                        )
                      )
                    ),
                    
                    ## Conteúdo da parte principal
                    ## Body content
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                        tabItems(
                          # Tab do dashboard - pesquisa
                          tabItem(tabName = "dashboard",
                                  # Colocar aqui o conteúdo da página da pesquisa
                              
                                  "Essa página foi elaborada com a intenção de facilitar a pesquisa de periódicos para publicação em grupos multi e interdisciplinares. É possível consultar as áreas de avaliação dos programas de pós-graduação no site da", 
                                  tags$a(href = "http://avaliacaoquadrienal.capes.gov.br/resultado-da-avaliacao-quadrienal-2017-2", "CAPES"),
                                  ".", 
                                  br(), br(),
                                  fluidRow( #uma linha
                                    box(#caixa em uma linha
                                      selectizeInput(
                                        inputId = "area_de_avaliacao_i",
                                        label = "Selecione a(s) Área(s) de Avaliação",
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
                                  valueBoxOutput("n_total_periodicos", width = 6), 
                                  valueBoxOutput("n_periodicos_filtrados", width = 6), 
                                  # Adiciona a tabela
                                  DTOutput("tabela_periodicos")
                          ),
                          
                          # Tab do projeto
                          tabItem(tabName = "sobre",
                                  # Colocar aqui a parte do projeto
                                  h2("Sobre esse shiny app"),
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
                          icon = icon("fas fa-book"), #icone nao ta funcionando
                          color = "aqua"
                        )
                      })
                      
                      output$n_periodicos_filtrados <- renderValueBox({
                        valueBox(
                          value = length(unique(filtered_data()$issn)),
                          subtitle =  "Número de periódicos filtrados",
                          icon = icon("fas fa-search"), 
                          color = "green"
                        )
                      })
                      
                      output$tabela_periodicos <- renderDT({
                        filtered_data()  %>%
                          select(-area_conc) %>% 
                          mutate(link = 
                                   paste0("<a href='https://www.google.com/search?q=issn%20",
                                          issn, "%20-%20", titulo, "', target='_blank' /> Saiba mais </a>")) %>% 
                          rename(ISSN = issn,
                                 `Título do Periódico` = titulo,
                                 `Área de Avaliação` = area_de_avaliacao,
                                 `Estrato CAPES` = estrato,
                                 `Pesquisa externa` = link) 
                        
                      } , escape = FALSE)
                      
                    }
                    
                    shinyApp(ui, server)