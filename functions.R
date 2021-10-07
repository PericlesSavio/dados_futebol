## debug
# str_csv = read.csv('csv/SB.csv', fileEncoding = 'UTF-8')
# 
# str_csv = 'csv/SB.csv'
# str_csv2 = 'csv/SB_1938.csv'
# int_ano = 2006
# int_rodada_I = 1
# int_rodada_F = 5
# int_posicao = 1

########################################

## gerar classificação
classificacao = function(str_csv, int_ano, int_rodada_I, int_rodada_F) {
  classificacao <- function(str_csv, int_ano, int_rodada_I, int_rodada_F) {
    #Pacote
    library(dplyr)
    
    #Carregar tabela
    csv <- read.csv(file=str_csv, header=TRUE, sep=",", encoding = "UTF-8")
    
    #Filtro
    csv <- filter(csv, csv$ano == int_ano, csv$rodada >= int_rodada_I, csv$rodada <= int_rodada_F)
    
    #Definindo variável N para nº de linhas para o FOR
    N <- nrow(csv)
    
    #Criando data frame
    dados <- data.frame(Ano = csv$ano,
                        #Compet = csv$competicao,
                        Rodada = csv$rodada,
                        Data = csv$data,
                        #Hora = csv$horario,
                        Mandante = csv$mandante,
                        GM = csv$gm,
                        GV = csv$gv,
                        Visitante = csv$visitante,
                        Local = csv$local,
                        V = 0,
                        E = 0,
                        D = 0,
                        DIF = 0,
                        PPM = csv$perda_pontos_m,
                        PPV = csv$perda_pontos_v)
    
    #transformar NA em 0
    dados[is.na(dados)] <- 0
    
    # FOR para gerar colunas: V, E, D e SD (vitória, empate, derrota e saldo de gol)
    for (i in 1:N) {
      if (dados$GM[i] > dados$GV[i]) {
        
        dados$V[i] <- 1 #V
        dados$E[i] <- 0 #E
        dados$D[i] <- 0 #E
        dados$DIF[i] <- dados$GM[i] - dados$GV[i] #Saldo de Gols
        
      } else if (dados$GM[i] == dados$GV[i]) {
        
        dados$V[i] <- 0 #V
        dados$E[i] <- 1 #E
        dados$D[i] <- 0 #E
        dados$DIF[i] <- dados$GM[i] - dados$GV[i]
        
      } else if (dados$GM[i] < dados$GV[i]) {
        
        dados$V[i] <- 0 #V
        dados$E[i] <- 0 #E
        dados$D[i] <- 1 #E
        dados$DIF[i] <- dados$GM[i] - dados$GV[i]
        
      } 
    }
    
    #Definindo colunas, criando parte 2 dos dados (visitantes) e união.
    names(dados) <- c("Ano", "Rodada", "Data", "Mandante", "GM", "GV", "Visitante", "Local",
                      "V","E","D","SD","Perda de Pontos Mandante",0)
    dados2 <- data.frame(dados$Ano,dados$Rodada,dados$Data,dados$Visitante,dados$GV,dados$GM,
                         dados$Mandante,dados$Local,dados$D,dados$E,dados$V,dados$SD*(-1),dados$`0`,0)
    names(dados2) <- c("Ano", "Rodada", "Data", "Mandante", "GM", "GV", "Visitante", "Local", "V","E",
                       "D","SD","Perda de Pontos Mandante",0)
    dados3 <- rbind(dados, dados2)
    
    clas <- data.frame(dados3$Mandante,
                       dados3$V*3 + dados3$E - dados3$`Perda de Pontos Mandante`,
                       1,
                       dados3$V,
                       dados3$E,
                       dados3$D,
                       dados3$GM,
                       dados3$GV,
                       dados3$SD)
    
    names(clas) <- c("Clube", "Pts", "J", "V", "E", "D", "GP", "GC", "Sld")
    
    
    clas2 <- cbind(0,
                   aggregate(clas$Pts, by=data.frame(clas$Clube), FUN=sum),
                   aggregate(clas$J, by=data.frame(clas$Clube), FUN=sum)[2],
                   aggregate(clas$V, by=data.frame(clas$Clube), FUN=sum)[2],
                   aggregate(clas$E, by=data.frame(clas$Clube), FUN=sum)[2],
                   aggregate(clas$D, by=data.frame(clas$Clube), FUN=sum)[2],
                   aggregate(clas$GP, by=data.frame(clas$Clube), FUN=sum)[2],
                   aggregate(clas$GC, by=data.frame(clas$Clube), FUN=sum)[2],
                   aggregate(clas$Sld, by=data.frame(clas$Clube), FUN=sum)[2])
    
    names(clas2) <- c("#", "Clube", "Pts", "J", "V", "E", "D", "GP", "GC", "Sld")
    
    clas2 <- clas2[order(-clas2$Pts, -clas2$V, -clas2$Sld, -clas2$GP),]
    clas2[,1] <- 1:20
    #clas2$Ano = int_ano
    #clas2$Rodadas = paste0(int_rodada_I, '-', int_rodada_F)
    return(clas2)
  }
  #tabela = classificacao(str_csv, int_ano, int_rodada_I, int_rodada_F)
  
  ## classificação(pos) nas R19 e R38
  R19_38 <- function(str_csv, int_ano, int_rodada_I, int_rodada_F) {
    R19_R38 <- merge(classificacao(str_csv,int_ano, 1, 19), classificacao(str_csv,int_ano, 1, 38), by = "Clube", all.x = TRUE)
    R19_R38p <- data.frame(R19_R38$Clube,
                           R19_R38$`#.x`,
                           R19_R38$`#.y`)
    colnames(R19_R38p) <- c("Clube", "R19", "R38")
    R19_R38p
  }
  #R = R19_38(str_csv, int_ano, int_rodada_I, int_rodada_F)
  
  #classificação com rodadas 19 e 38
  classificacao_R19_R38 <- function(str_csv, int_ano, int_rodada_I, int_rodada_F) {
    clas2 <- classificacao(str_csv, int_ano, int_rodada_I, int_rodada_F)
    R19_R38 <- R19_38(str_csv, int_ano, int_rodada_I, int_rodada_F)
    SerieX <- merge(clas2, R19_R38, by = "Clube", all.x = TRUE)
    SerieX <- SerieX[order(-SerieX$Pts, -SerieX$V, -SerieX$Sld, -SerieX$GP),]
    SerieX$Ano = int_ano
    SerieX$Rodadas = paste0(int_rodada_I, '-', int_rodada_F)
    SerieX = SerieX[, c('Clube', '#', 'Pts', 'J', 'V', 'E', 'D', 'GP', 'GC', 'Sld', 'Ano', 'Rodadas', 'R19', 'R38')]
    return(SerieX)
  }
  X = classificacao_R19_R38(str_csv, int_ano, int_rodada_I, int_rodada_F)
  X = X[, c('#', 'Clube', 'Pts', 'J', 'V', 'E', 'D', 'GP', 'GC', 'Sld', 'Ano', 'Rodadas', 'R19', 'R38')]
  return(X)
}
#classificacao(str_csv, int_ano, int_rodada_I, int_rodada_F)

## historico de todas as edições por ano
historico <- function(str_csv, str_csv2, int_posicao, int_rodada_I, int_rodada_F) {
  historico <- function(str_csv, int_posicao, int_rodada_I, int_rodada_F) {
    library(dplyr) #Pacote
    #source("SerieA_Tabela.R")
    
    csv <- read.csv(file=str_csv, header=TRUE, sep=",", encoding = "UTF-8")
    ano_calc = min(csv[,1])
    
    #Gerando o data frame "SerieA"
    Serie <- classificacao(str_csv, ano_calc, 1, 1)[0,]
    
    
    for (i in ano_calc:2019) {
      Serie[i-(ano_calc-1),] <- classificacao(str_csv, i, int_rodada_I, int_rodada_F)[int_posicao,]
      Serie[i-(ano_calc-1),11] <- i
      Serie[i-(ano_calc-1),12] <- paste(int_rodada_I,int_rodada_F,sep = "-")
    }
    
    names(Serie) <- c("#", "Clube", "Pts", "J", "V", "E", "D", "GP", "GC", "Sld", "Ano", "Rodada")
    
    Serie <- Serie[order(-Serie[3], -Serie[5], -Serie[10], -Serie[8]),]
    Serie
  }
  
  # dados R19 e R38
  H1938 <- function(str_csv) {
    
    H1938_ano <- function(int_ano) {
      SerieA_1938 <- merge(classificacao(str_csv, int_ano, 1, 19), classificacao(str_csv, int_ano, 1, 38), by = "Clube", all.x = TRUE)
      SerieA_1938p <- data.frame(SerieA_1938[,1],
                                 int_ano,
                                 SerieA_1938[,2],
                                 SerieA_1938[,11],
                                 paste (SerieA_1938[,1],int_ano, sep = "", collapse = NULL))
      colnames(SerieA_1938p) <- c("Clube", "Ano", "R19", "R38", "ID")
      SerieA_1938p
    }
    
    H1938_todosanos <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(H1938_todosanos) <- c("Clube", "Ano", "R19", "R38", "ID")
    
    csv <- read.csv(file=str_csv, header=TRUE, sep=",", encoding = "UTF-8")
    ano_calc = min(csv[,1])
    
    for (int_ano in ano_calc:2019) {
      #SerieAx <- XA(int_ano)
      H1938_todosanos <- rbind(H1938_todosanos, H1938_ano(int_ano))
    }
    H1938_todosanos
  }
  
  #historico de todas as edições por ano + r19 e r38
  historico_dados <- function(str_csv, str_csv2, int_posicao, int_rodada_I, int_rodada_F) {
    
    h <- historico(str_csv, int_posicao, int_rodada_I, int_rodada_F)
    
    h2 <- data.frame(h[,1],
                     h[,2],
                     h[,3],
                     h[,4],
                     h[,5],
                     h[,6],
                     h[,7],
                     h[,8],
                     h[,9],
                     h[,10],
                     h[,11],
                     h[,12],
                     paste(h[,2],h[,11], sep = "", collapse = NULL))
    
    names(h2) <- c("#", "Clube", "Pts", "J", "V", "E", "D", "GP", "GC", "Sld", "Ano", "Rodada", "ID")
    
    #r <- H1938(str_csv)
    r <- read.csv(file=str_csv2, header=TRUE, sep=",", encoding = "UTF-8")
    
    hr <- merge(h2, r, by = "ID", all.x = TRUE)
    
    hr2 <- data.frame(hr[,2],
                      hr[,3],
                      hr[,4],
                      hr[,5],
                      hr[,6],
                      hr[,7],
                      hr[,8],
                      hr[,9],
                      hr[,10],
                      hr[,11],
                      hr[,12],
                      hr[,13],
                      hr[,16],
                      hr[,17])
    
    names(hr2) <- c("#", "Clube", "Pts", "J", "V", "E", "D", "GP", "GC", "Sld", "Ano", "Rodada", "R19", "R38")
    hr2 <- hr2[order(-hr2[,3], -hr2[,5], -hr2[,10]),]
    return(hr2)
  }
  historico_dados(str_csv, str_csv2, int_posicao, int_rodada_I, int_rodada_F)
}
#historico(str_csv, str_csv2, int_posicao, int_rodada_I, int_rodada_F)

## tabela de jogos
tabela <- function(str_csv, int_ano, int_rodada_F) {
  #Pacote
  library(dplyr)
  
  #Carregar tabela
  csv <- read.csv(file=str_csv, header=TRUE, sep=",", encoding = "UTF-8")
  
  #Filtro
  csv <- filter(csv, csv$ano == int_ano, csv$rodada == int_rodada_F)
  
  #Criando data frame
  dados <- data.frame(Data = csv$data,
                      Mandante = csv$mandante,
                      Placar = paste0(csv$gm, '-', csv$gv),
                      Visitante = csv$visitante,
                      Local = csv$local)
  
  return(dados)
}
#tabela(str_csv, int_ano, int_rodada_F)

## projeção pontos
prj_pts_ano <- function(S, ano) {
  library(plotly)
  
  #ler a tabela de projeção
  projecao = read.csv(file=paste0('csv/',S,'_projecao.csv'), header=TRUE, sep=",", encoding = "UTF-8")
  
  for (pos in 1:20) {
    assign(paste0('trace_',pos),filter(projecao, Pos == pos, Ano == ano, Rodada > 3))
  }
  
  x <- c(4:38)
  
  data <- data.frame(x)
  
  fig <- plot_ly(data, x = ~x, y = trace_1$Pts, name = '1º colocado', type = 'scatter', mode = 'lines+markers',
                 hovertemplate = paste(trace_1$Clube, '<br><b>', round(trace_1$Pts, digits=1),
                                       '</b> pontos', '<br>', '<b>', trace_1$Rodada, '</b>ª rodada', sep = '')
  )
  
  trace_projecao <- function(trace, pos, legend) {
    fig %>% add_trace(y = trace$Pts, name = paste0(pos, 'º'), type = 'scatter', mode = 'lines+markers', visible = legend,
                      hovertemplate = paste(trace$Clube, '<br><b>', round(trace$Pts, digits=1), '</b> pontos', '<br>', '<b>',
                                            trace$Rodada, '</b>ª rodada', sep = '')
    )
  }
  
  fig <- trace_projecao(trace_2, 2, 'legendonly')
  fig <- trace_projecao(trace_3, 3, 'legendonly')
  fig <- trace_projecao(trace_4, 4, TRUE)
  fig <- trace_projecao(trace_5, 5, 'legendonly')
  fig <- trace_projecao(trace_6, 6, TRUE)
  fig <- trace_projecao(trace_7, 7, 'legendonly')
  fig <- trace_projecao(trace_8, 8, 'legendonly')
  fig <- trace_projecao(trace_9, 9, 'legendonly')
  fig <- trace_projecao(trace_10, 10, 'legendonly')
  fig <- trace_projecao(trace_11, 11, 'legendonly')
  fig <- trace_projecao(trace_12, 12, TRUE)
  fig <- trace_projecao(trace_13, 13, 'legendonly')
  fig <- trace_projecao(trace_14, 14, 'legendonly')
  fig <- trace_projecao(trace_15, 15, 'legendonly')
  fig <- trace_projecao(trace_16, 16, TRUE)
  fig <- trace_projecao(trace_17, 17, 'legendonly')
  fig <- trace_projecao(trace_18, 18, 'legendonly')
  fig <- trace_projecao(trace_19, 19, 'legendonly')
  fig <- trace_projecao(trace_20, 20, 'legendonly')
  
  fig <- fig %>% layout(
    hovermode = "x unified",
    showlegend = TRUE,
    xaxis = list(title = "Rodada", range = c(4,38)),
    yaxis = list(title = "Pontos", range = c(0,115)),
    height = 500
  )
  
  fig
}
#prj_pts_ano('SA', 2006)