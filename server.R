server <- function(input, output) {

  
  output$serie_a <- renderTable({
    classificacao("csv/SA.csv", input$a_edicao, input$a_rodada[1], input$a_rodada[2])
  }, striped = TRUE, digits = 0, width = '100%')
  
  output$serie_b <- renderTable({
    classificacao("csv/SB.csv", input$b_edicao, input$b_rodada[1], input$b_rodada[2])
  }, striped = TRUE, digits = 0, width = '100%')
  
  output$historico_a <- renderTable({
    historico("csv/SA.csv", "csv/SA_1938.csv", input$a_h_posicao, input$a_h_rodada[1], input$a_h_rodada[2])
  }, striped = TRUE, digits = 0, width = '100%')
  
  output$historico_b <- renderTable({
    historico("csv/SB.csv", "csv/SB_1938.csv", input$b_h_posicao, input$b_h_rodada[1], input$b_h_rodada[2])
  }, striped = TRUE, digits = 0, width = '100%')
  
  output$tabela_a <- renderTable({
    tabela("csv/SA.csv", input$tabela_a_edicao, input$tabela_a_rodada)
  }, striped = TRUE, digits = 0)
  
  output$tabela_b <- renderTable({
    tabela("csv/SB.csv", input$tabela_b_edicao, input$tabela_b_rodada)
  }, striped = TRUE, digits = 0)
  
  output$SA_prj <- renderPlotly({
    prj_pts_ano('SA',input$tabela_a_edicao1)
  })
  
  output$SB_prj <- renderPlotly({
    prj_pts_ano('SB',input$tabela_b_edicao1)
  })
  
  
}