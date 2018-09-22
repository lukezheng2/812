# settings ----
  library(readr)
  library(dplyr)
  library(lubridate)

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
  dados = read_delim("dados brutos/Planilha_ESTOQUE_2016_a_2018_Diversidade.csv", delim = ";",
                    col_types = cols_only(Parcela             = facParcela,  Linha                         = facLinha, 'Sub transecto'      = facTransecto,
                                          Perimetr            = "n",         direcao                       = "n",      'grau decomp'        = facDecomp, 
                                          '[2-5 cm]'          = "i",         '[5-10 cm]'                   = "i",      'Tipo de planta'     = facEstrutura,
                                          podridao            = facPodridao, comprimento                   = "n",      'pto inter ceptação' = "n", 
                                          dia                 = "n",         mes                           = "n",      ano                  = "n", 
                                          'perimetro na base' = "n",         'perimetro no extremo distal' = "n"))
  
  # Aqui aparecem uns warnings, mas é só para avisar das variáveis que eu tirei.
  
  names(dados) = nomes
  
  levels(dados$estrutura) = c("tronco", "galho")
  
# Arrumando banco ----
  dados$X2[!is.na(dados$perimetro)] = NA
  dados$X5[!is.na(dados$perimetro)] = NA
  
  dados = dados %>% mutate(diametro = perimetro/pi) %>% select (-perimetro) %>% filter(!is.na(diametro) | !is.na(X2) | !is.na(X5))
  
  arrumaX2 = function(dados, linha){
    n = dados$X2[linha]
    add_row(dados,
            parcela          = dados[linha,]$parcela,                  
            linha            = dados[linha,]$linha,                      
            subTransecto     = dados[linha,]$subTransecto,        
            direcao          = dados[linha,]$direcao,                  
            decomp           = dados[linha,]$decomp,                    
            estrutura        = dados[linha,]$estrutura,              
            podridao         = dados[linha,]$podridao,                
            comprimento      = dados[linha,]$comprimento,          
            interceptacao    = dados[linha,]$interceptacao,      
            dia              = dados[linha,]$dia,                          
            mes              = dados[linha,]$mes,                          
            ano              = dados[linha,]$ano,                          
            perimetroBase    = dados[linha,]$perimetroBase,      
            perimetroExtremo = dados[linha,]$perimetroExtremo,
            diametro         = replicate(n, 3.5))
  }
  
  arrumaX5 = function(dados, linha){
    n = dados$X5[linha]
    add_row(dados,
            parcela          = dados[linha,]$parcela,                  
            linha            = dados[linha,]$linha,                      
            subTransecto     = dados[linha,]$subTransecto,        
            direcao          = dados[linha,]$direcao,                  
            decomp           = dados[linha,]$decomp,                    
            estrutura        = dados[linha,]$estrutura,              
            podridao         = dados[linha,]$podridao,                
            comprimento      = dados[linha,]$comprimento,          
            interceptacao    = dados[linha,]$interceptacao,      
            dia              = dados[linha,]$dia,                          
            mes              = dados[linha,]$mes,                          
            ano              = dados[linha,]$ano,                          
            perimetroBase    = dados[linha,]$perimetroBase,      
            perimetroExtremo = dados[linha,]$perimetroExtremo,
            diametro         = replicate(n, 7.5))
  }
  
  for (i in 1:nrow(dados)){
    if (!is.na(dados$X2[i]) & dados$X2[i] != 0)
      dados = arrumaX2(dados, i)
    if (!is.na(dados$X5[i]) & dados$X5[i] != 0)
      dados = arrumaX5(dados, i)
  }
  
  dados = dados %>% filter(is.na(X2) & is.na(X5)) %>% select(-X2, -X5)
  
  dados = dados %>% mutate(data = dmy(paste(dia, mes, ano, sep = "-"))) %>% select(-dia, -mes, -ano)
  
  save(dados, file = "dados.rda")
  
  ## No próximo arquivo, extrair a variável resposta com dados %>% group_by(linha, parcela, sei lá o q) %>% summarise(vol = pi * sum(diametro)/(8*45)) (acho)
  ## No próximo arquivo, fazer análises descritivas.