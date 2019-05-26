library(shiny)
library(DT)
library(shinydashboard)

qualis_capes <- read_rds("output/qualis_capes.rds")

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
               fluidRow( #uma linha
                box(#caixa em uma linha
                  selectInput(
                    inputId = "area_de_avaliacao",
                    label = "Área de Avaliação",
                    choices =  sort(unique(qualis_capes$area_de_avaliacao)),  
                    multiple = TRUE
                  )), 
                
                box(
                  
                  selectInput(
                    inputId = "slider_estrato",
                    label = "Escolha a classificação no periódicos CAPES",
                   choices = sort(unique(qualis_capes$estrato)),
                   multiple = TRUE
                  )
                ) # caixa em uma linha
              ),
              
              # Adiciona a tabela
                DTOutput("resultado_pesquisa")
      ),
      
      # Tab do projeto
      tabItem(tabName = "sobre",
              # Colocar aqui a parte do projeto
              h2("Sobre o projeto")
      ),
      # Tab do github  
      tabItem(tabName = "github",
              # Colocar aqui a parte do projeto
              h2("Acesse o código")
      )
      
    )
  )
)



server <- function(input, output) {
  
}

shinyApp(ui, server)