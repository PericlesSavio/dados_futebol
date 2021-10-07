## gera arquivos separados por posição (projeção)
projecao_pts <- function(str_csv, pos) {
  
  projecao <- function(str_csv, int_ano, pos) {
    
    a <- data.frame(
      Pts = integer()
    )
    for (i in 1:38) {
      a[i,1] <- int_ano
      a[i,2] <- pos
      a[i,3] <- (classificacao(str_csv, int_ano, 1, i)[pos,3]/(i))*38
      a[i,4] <- i
      a[i,5] <- classificacao(str_csv, int_ano, 1, i)[pos,2]
    }
    names(a) = c('Ano', 'Pos', 'Pts', 'Rodada', 'Clube')
    a
  }
  
  df <- data.frame(Ano=as.integer(),
                   Pos=as.integer(), 
                   Pts=as.integer(),
                   Rodada=as.integer())
  
  csv_projecao <- function(str_csv, pos) {
    for (i in 2006:2019) {
      df <- rbind(df, projecao(str_csv, i, pos))
      print(i)
    }
    return(df)
  }
  
  x <- csv_projecao(str_csv, pos)
  
  if (str_csv == 'csv/SA.csv') {
    write.csv(x,paste("csv/SA_projecao_", pos ,".csv", sep = ''), fileEncoding = "UTF-8", row.names = FALSE)
  }
  
  if (str_csv == 'csv/SB.csv') {
    write.csv(x,paste("csv/SB_projecao_", pos ,".csv", sep = ''), fileEncoding = "UTF-8", row.names = FALSE)
  }
}
#projecao_pts('csv/SB.csv', 1)

## gera um arquivo único
projecao_pts_1 <- function(S) {
  x = NULL
  
  for (n in 1:20) {
    x1 = read.csv(paste0('csv/',S,'_projecao_',n,'.csv'), header=TRUE, sep=",", encoding = "UTF-8")
    x = rbind(x, x1)
    x
  }
  
  write.csv(x, paste0('csv/', S,'_projecao.csv'), row.names = FALSE, fileEncoding = "UTF-8")
}
#projecao_pts_1('SA')
#projecao_pts_1('SB')








str_csv = 'csv/SB.csv'
str_csv2 = 'csv/SB_1938.csv'
int_ano = 2006
int_rodada_I = 1
int_rodada_F = 5
int_posicao = 1


gerar_classificacao <- function(str_csv, int_ano, int_rodada_I) {
  #Pacote
  library(dplyr)
  
  #Carregar tabela
  csv <- read.csv(file=str_csv, header=TRUE, sep=",", encoding = "UTF-8")
  
  #Filtro
  csv <- filter(csv, csv$ano == int_ano, csv$rodada >= int_rodada_I, csv$rodada <= 38)
  
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
  
  clas <- data.frame(dados3$Ano,
                     dados3$Rodada,
                     dados3$Mandante,
                     dados3$V*3 + dados3$E - dados3$`Perda de Pontos Mandante`,
                     1,
                     dados3$V,
                     dados3$E,
                     dados3$D,
                     dados3$GM,
                     dados3$GV,
                     dados3$SD)
  
  names(clas) <- c("Ano", "Rodada", "Clube", "Pts", "J", "V", "E", "D", "GP", "GC", "Sld")
  
  return(clas)
}
gerar_classificacao(str_csv, int_ano, int_rodada_I)


tabela = NULL
for (int_ano in 2006:2019) {
  for (int_rodada_I in 1:38) {
    tabela2 = gerar_classificacao(str_csv, int_ano, int_rodada_I)
    tabela = rbind(tabela, tabela2)
  }
}

write.csv(tabela, 'csv/SB_calssificacao.csv', row.names = FALSE)




a = read.csv2('csv/SB_calssificacao.csv', header = TRUE)




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

pos = 1






projecao_pts <- function(str_csv, pos) {
  
  projecao <- function(str_csv, int_ano, pos) {
    
    a <- data.frame(
      Pts = integer()
    )
    for (i in 1:38) {
      a[i,1] <- int_ano
      a[i,2] <- pos
      a[i,3] <- (classificacao(str_csv, int_ano, 1, i)[pos,3]/(i))*38
      a[i,4] <- i
      a[i,5] <- classificacao(str_csv, int_ano, 1, i)[pos,2]
    }
    names(a) = c('Ano', 'Pos', 'Pts', 'Rodada', 'Clube')
    a
  }
  
  df <- data.frame(Ano=as.integer(),
                   Pos=as.integer(), 
                   Pts=as.integer(),
                   Rodada=as.integer())
  
  csv_projecao <- function(str_csv, pos) {
    for (i in 2006:2019) {
      df <- rbind(df, projecao(str_csv, i, pos))
    }
    df
  }
  
  x <- csv_projecao(str_csv, pos)
  
  if (str_csv == 'csv/SA.csv') {
    write.csv(x,paste("csv/SA_projeção_", pos ,".csv", sep = ''), fileEncoding = "UTF-8", row.names = FALSE)
  }
  
  if (str_csv == 'csv/SB.csv') {
    write.csv(x,paste("csv/SB_projeção_", pos ,".csv", sep = ''), fileEncoding = "UTF-8", row.names = FALSE)
  }
  
  
  
}
projecao_pts('csv/SB.csv', 1)




projecao_pts('SA.csv', 4)
projecao_pts('SA.csv', 6)
projecao_pts('SA.csv', 16)
projecao_pts('SA.csv', 17)

for (i in 1:20) {
  projecao_pts('SA.csv', i)
}


data = NULL

for (int_ano in 2006:2019) {
  data2 = classificacao(str_csv, int_ano, int_rodada_I, int_rodada_F)
  data = rbind(data2, data)
}

for (variable in vector) {
  
}

classificacao(str_csv, int_ano, int_rodada_I, int_rodada_F)
str_csv = 'csv/SB.csv'
str_csv2 = 'csv/SB_1938.csv'
int_ano = 2006
int_rodada_I = 1
int_rodada_F = 5
int_posicao = 1


















#escrever tabela SX_1938 para deixar o sistema mais rápido
write.csv(H1938("SA.csv"),"csv/SA_1938.csv", fileEncoding = "UTF-8")
write.csv(H1938("csv/SB.csv"),"csv/SB_1938.csv", fileEncoding = "UTF-8", row.names = FALSE)
