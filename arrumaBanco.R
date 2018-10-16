#esse
# settings ----
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# definindo variáveis para importação ----
facParcela   = col_factor(c("1A", "2A", "3A", "1B", "2B", "3B", "1C", "2C", "3C", "1D", "2D", "3D"))
facLinha     = col_factor(c(18, 38, 46, 22, 30, 6, 14, 34, 42, 26, 2, 10))
facTransecto = col_factor(1:3)
facDecomp    = col_factor(1:5)
facEstrutura = col_factor(c("arv", "arv(galho)"))
facPodridao  = col_factor(c("mista", "branca", "marrom"))

nomes = c("parcela", "linha",    "subTransecto", "perimetro",     "direcao",     "decomp",
          "X2", "X5", "estrutura",    "podridao",      "comprimento", "interceptacao", 
          "dia",     "mes",      "ano",          "perimetroBase", "perimetroExtremo")

# importando dados ---- 
#"--ccc-nnc-iiccnn-c-ccc--nn"
dados = read_delim("Planilha_ESTOQUE_2016_a_2018_Diversidade.csv", delim = ";",
                   col_types = cols_only(Parcela             = facParcela,  Linha                         = facLinha, 'Sub transecto'      = facTransecto,
                                         Perimetr            = "n",         direcao                       = "n",      'grau decomp'        = facDecomp, 
                                         '[2-5 cm]'          = "i",         '[5-10 cm]'                   = "i",      'Tipo de planta'     = facEstrutura,
                                         podridao            = facPodridao, comprimento                   = "n",      'pto inter ceptação' = "n", 
                                         dia                 = "n",         mes                           = "n",      ano                  = "n", 
                                         'perimetro na base' = "n",         'perimetro no extremo distal' = "n"))

# Aqui aparecem uns warnings, mas é só para avisar das variáveis que eu tirei.

names(dados) = nomes

levels(dados$estrutura) = c("tronco", "galho")

dados = dados %>% cbind(stringr::str_split_fixed(dados$parcela, "", 2))

names(dados)[18] <- "tratamento"
names(dados)[19] <- "replicacao"

# Arrumando banco ----
dados$X2[!is.na(dados$perimetro)] = NA
dados$X5[!is.na(dados$perimetro)] = NA

dados = dados %>% 
  mutate(diametro = perimetro/pi) %>% 
  select (-perimetro) %>% 
  filter(!is.na(diametro) | !is.na(X2) | !is.na(X5)) %>% 
  mutate(data = lubridate::dmy(paste(dia, mes, ano, sep = "-"))) %>% select(-dia, -mes, -ano)

#Removendo observações nulas
dados <- dados %>% filter(X2!=0 | X5!=0 | !is.na(diametro))
n = 5*(dados %>% filter(!is.na(X2)) %>% {.$X2 %>% sum} + dados %>% filter(!is.na(X5)) %>% {.$X5 %>% sum})

rep_x <- function(dados, n, i, media){
  y = 5*n
  for(a in 1:y){
    dados %<>% rbind(dados[i,] %>% mutate(diametro = media)) 
  }
  return(dados)
}

for(i in 1:266){
  if(!is.na(dados$X2[i])){
    x = dados$X2[i]
    dados <- rep_x(dados,x,i, 3.5)
  }
  
  if(!is.na(dados$X5[i])){
    x = dados$X5[i]
    dados <- rep_x(dados,x,i, 7.5)
  }
  print(100*i/266)
}

dados %<>% filter(!is.na(diametro))

save(dados, file = "dados.rda")

## No próximo arquivo, extrair a variável resposta com dados %>% group_by(linha, parcela, sei lá o q) %>% summarise(vol = pi * sum(diametro)/(8*45)) (acho)
## No próximo arquivo, fazer análises descritivas.
