## app.R ##
library(shinydashboard)
library(plotly)

source("estrutura.R", encoding = "UTF-8")
source("functions.R", encoding = "UTF-8")

ui <- dashboardPage(
  dashboardHeader(title = "DASH FUTEBOL"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Sobre", tabName = "sobre", icon = icon("address-card")),
      menuItem("Série A",tabName = "serie_a", icon = icon("futbol")),
      menuItem("Série B",tabName = "serie_b", icon = icon("futbol"))
    )
    
    
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "sobre", h1("Sobre"),
              fluidRow(
                  box(
                    title = "Contato", status = "primary", solidHeader = TRUE, width = 12,
                    Contato
                  )
              )
      ),
      
      tabItem(tabName = "serie_a", h1("Campeonato Brasileiro Série A"),
              fluidRow(
                
                tabBox(side = "left", width = 12, selected = "Classificação",
                  tabPanel("Classificação",
                   fluidRow(
                     box(
                       title = "Classificação por Ano e Colocação", status = "primary", solidHeader = TRUE, width = 7,
                       tableOutput("serie_a")
                     ),
                     box(
                       title = "Painel de Controle", status = "primary", solidHeader = TRUE, width = 5,
                       sliderInput("a_edicao", "Ano", min = 2006, max = 2019,value = 2006),
                       sliderInput("a_rodada", "Rodada", min = 1, max = 38, value = c(1,38))
                     ),
                     box(
                       title = "Notas", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                       h6("*R19 é a colocação do clube na 19ª rodada. R38 é para a 38ª rodada."),
                       Serie_A_Notas
                     ),
                     box(
                       title = "Perda de Pontos", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                       Serie_A_PontosPerdidos
                     )
                   ),
                  ),
                  
                  tabPanel("Histórico por Rodada e Colocação",
                           fluidRow(
                             box(
                               title = "Histórico por Rodada e Colocação", status = "primary", solidHeader = TRUE, width = 7,
                               tableOutput("historico_a")
                             ),
                             
                             box(
                               title = "Painel de Controle", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                               sliderInput("a_h_posicao", "Colocação", min = 1, max = 20, value = 1),
                               sliderInput("a_h_rodada", "Rodada", min = 1, max = 38, value = c(1,38))
                             ),
                             
                             
                             box(
                               title = "Notas", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                               h6("*R19 é a colocação do clube na 19ª rodada. R38 é para a 38ª rodada."),
                               Serie_A_Notas
                             ),
                             
                             
                             box(
                               title = "Perda de Pontos", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                               Serie_A_PontosPerdidos
                             )
                             
                             
                           )
                           
                           
                  ),
                  tabPanel("Tabela de Jogos",
                           fluidRow(
                             box(
                               title = "Tabela de Jogos", status = "primary", solidHeader = TRUE, width = 7,
                               tableOutput("tabela_a")
                             ),
                             box(
                               title = "Painel de Controle", status = "primary", solidHeader = TRUE, width = 5,
                               sliderInput("tabela_a_edicao", "Ano", min = 2006, max = 2019, value = 2006),
                               sliderInput("tabela_a_rodada", "Rodada", min = 1, max = 38, value = 1)
                             ),
                             
                             box(
                               title = "Notas", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                               h6("*R19 é a colocação do clube na 19ª rodada. R38 é para a 38ª rodada."),
                               Serie_A_Notas
                             )
                             
                             
                           )
                           
                           
                  ),
                  
                  
                  tabPanel("Dados",
                           fluidRow(
                             box(
                               title = "Painel de Controle", status = "primary", solidHeader = TRUE, width = 12,
                               sliderInput("tabela_a_edicao1", "Ano", min = 2006, max = 2019, value = 2006)
                             ),
                             
                             box(
                               title = "Projeção de pontos", status = "primary", solidHeader = TRUE, width = 12, height = 570,
                               plotlyOutput("SA_prj")
                             )
                           )
                  )
                  
                  
                ),
                
                
                
                
              )
      ),
      
      tabItem(tabName = "serie_b", h1("Campeonato Brasileiro Série B"),
              fluidRow(
                
                tabBox(side = "left", width = 12, selected = "Classificação",
                       tabPanel("Classificação",
                                fluidRow(
                                  box(
                                    title = "Classificação por Ano e Colocação", status = "primary", solidHeader = TRUE, width = 7,
                                    tableOutput("serie_b")
                                  ),
                                  box(
                                    title = "Painel de Controle", status = "primary", solidHeader = TRUE, width = 5,
                                    sliderInput("b_edicao", "Ano", min = 2006, max = 2019,value = 2006),
                                    sliderInput("b_rodada", "Rodada", min = 1, max = 38, value = c(1,38))
                                  ),
                                  box(
                                    title = "Notas", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                                    h6("*R19 é a colocação do clube na 19ª rodada. R38 é para a 38ª rodada."),
                                    Serie_B_Notas
                                  ),
                                  box(
                                    title = "Perda de Pontos", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                                    Serie_B_PontosPerdidos
                                  )
                                ),
                       ),
                       
                       tabPanel("Histórico por Rodada e Colocação",
                                fluidRow(
                                  box(
                                    title = "Histórico por Rodada e Colocação", status = "primary", solidHeader = TRUE, width = 7,
                                    tableOutput("historico_b")
                                  ),
                                  
                                  box(
                                    title = "Painel de Controle", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                                    sliderInput("b_h_posicao", "Colocação", min = 1, max = 20, value = 1),
                                    sliderInput("b_h_rodada", "Rodada", min = 1, max = 38, value = c(1,38))
                                  ),
                                  
                                  box(
                                    title = "Notas", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                                    h6("*R19 é a colocação do clube na 19ª rodada. R38 é para a 38ª rodada."),
                                    Serie_A_Notas
                                  ),
                                  
                                  box(
                                    title = "Perda de Pontos", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                                    Serie_B_PontosPerdidos
                                  )
                                  
                                  
                                )
                                
                                
                       ),
                       tabPanel("Tabela de Jogos",
                                fluidRow(
                                  box(
                                    title = "Tabela de Jogos", status = "primary", solidHeader = TRUE, width = 7,
                                    tableOutput("tabela_b")
                                  ),
                                  box(
                                    title = "Painel de Controle", status = "primary", solidHeader = TRUE, width = 5,
                                    sliderInput("tabela_b_edicao", "Ano", min = 2006, max = 2019, value = 2006),
                                    sliderInput("tabela_b_rodada", "Rodada", min = 1, max = 38, value = 1)
                                  ),
                                  
                                  box(
                                    title = "Notas", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 5,
                                    h6("*R19 é a colocação do clube na 19ª rodada. R38 é para a 38ª rodada."),
                                    Serie_B_Notas
                                  )
                                  
                                  
                                )
                                
                                
                       ),
                       

                       
                       tabPanel("Dados",
                                fluidRow(
                                  box(
                                    title = "Painel de Controle", status = "primary", solidHeader = TRUE, width = 12,
                                    sliderInput("tabela_b_edicao1", "Ano", min = 2006, max = 2019, value = 2006)
                                  ),
                                  box(
                                    title = "Projeção de pontos", status = "primary", solidHeader = TRUE, width = 12, height = 570,
                                    plotlyOutput("SB_prj")
                                  )
                                )
                       )
                       
                       
                ),
                
                
                
                
              )
      )
      
      
      
      
    )
  )
  
  
  
)
