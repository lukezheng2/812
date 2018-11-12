# settings ----
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(magrittr)

# definindo variáveis para importação ----
  facLinha     = col_factor(c(18, 38, 46, 22, 30, 6, 14, 34, 42, 26, 2, 10))
  facTransecto = col_factor(1:3)
  facDecomp    = col_factor(1:5)
  facEstrutura = col_factor(c("arv", "arv(galho)"))
  facPodridao  = col_factor(c("mista", "branca", "marrom"))
  facAno       = col_factor(c(2016, 2017, 2018))
  
  nomes = c("parcela", "linha",    "subTransecto", "perimetro",     "direcao",     "decomp",
            "X2", "X5", "estrutura",    "podridao",      "comprimento", "interceptacao", 
            "ano",          "perimetroBase", "perimetroExtremo")

# importando dados ---- 
#"--ccc-nnc-iiccnn-c-ccc--nn"
  dados = read_delim("dados brutos/Planilha_ESTOQUE_2016_a_2018_Diversidade.csv", delim = ";",
                   col_types = cols_only(Parcela          = "c",          Linha                = facLinha,
                                         'Sub transecto'  = facTransecto, Perimetr             = "n",
                                         direcao          = "n",          'grau decomp'        = facDecomp, 
                                         '[2-5 cm]'       = "i",          '[5-10 cm]'          = "i",
                                         'Tipo de planta' = facEstrutura, podridao             = facPodridao, 
                                         comprimento      = "n",          'pto inter ceptação' = "n", 
                                         ano              = facAno,       'perimetro na base'  = "n",
                                         'perimetro no extremo distal' = "n"))

# Aqui aparecem uns warnings, mas é só para avisar das variáveis que eu tirei.

  names(dados) = nomes

  levels(dados$estrutura) = c("tronco", "galho")
  
  rm(nomes, facDecomp, facEstrutura, facLinha, facPodridao, facTransecto, facAno)

  dados %<>% separate(parcela, c("tratamento", "replicacao"), sep = 1)
  dados$tratamento = factor(dados$tratamento)
  dados$replicacao = factor(dados$replicacao)

# Arrumando banco ----
# os dados de 2018 não tem contagem
#dados$X2[!is.na(dados$perimetro)] = NA
#dados$X5[!is.na(dados$perimetro)] = NA

  dados$X2[dados$X2 == 0] = NA 
  dados$X5[dados$X5 == 0] = NA
  
  dados = dados %>% 
    mutate(diametro = perimetro/pi) %>% 
    select (-perimetro) %>% 
    filter(!is.na(diametro) | !is.na(X2) | !is.na(X5))

  dadosAnt = dados %>% filter(ano != 2018)
  dadosPos = dados %>% filter(ano == 2018)

  # Calcular quantas observações devem ter ao final ----
  # para ant
  nAnt = (sum(dadosAnt$X2, na.rm = T) + sum(dadosAnt$X5, na.rm = T))*5 + sum(!is.na(dadosAnt$diametro))
  # para pos
  nPos = (sum(dadosPos$X2, na.rm = T) + sum(dadosPos$X5, na.rm = T))*5 + sum(is.na(dadosPos$X2) & is.na(dadosPos$X5))
  
  # alocar data.frames adecuados
  aloca = function(dados, i, n, diam){
    do.call(rbind, rep(list(dados[i,]), n)) %>% mutate(diametro = diam)
  }
  
  ant = rbind(do.call(rbind, map(1:116, function(i){if(!is.na(dadosAnt$X2[i])){aloca(dadosAnt, i, dadosAnt[i,"X2"] * 5, 3.5)}})),
              do.call(rbind, map(1:116, function(i){if(!is.na(dadosAnt$X5[i])){aloca(dadosAnt, i, dadosAnt[i,"X5"] * 5, 7.5)}})),
              dadosAnt %>% filter(is.na(X2) & is.na(X5))) %>% select(-X2, -X5)
  
  pos = rbind(do.call(rbind, map(1:150, function(i){if(!is.na(dadosPos$X2[i])){aloca(dadosPos, i, dadosPos[i,"X2"] * 5, dadosPos$diametro[i])}})),
              do.call(rbind, map(1:150, function(i){if(!is.na(dadosPos$X5[i])){aloca(dadosPos, i, dadosPos[i,"X5"] * 5, dadosPos$diametro[i])}})),
              dadosPos %>% filter(is.na(X2) & is.na(X5))) %>% select(-X2, -X5)

  ant$catDiametro = ifelse(ant$diametro < 5,  "(2-5)",
                    ifelse(ant$diametro < 10, "(5-10)", "(+10)")) %>% factor(levels = c("(2-5)", "(5-10)", "(+10)"))
  
  pos$catDiametro = ifelse(pos$diametro < 5,  "(2-5)",
                    ifelse(pos$diametro < 10, "(5-10)", "(+10)")) %>% factor(levels = c("(2-5)", "(5-10)", "(+10)"))
  
save(ant, pos, file = "dados.rda")

## No próximo arquivo, extrair a variável resposta com dados %>%
#group_by(linha, parcela, sei lá o q) %>% summarise(vol = pi * sum(diametro)/(8*45)) (acho)
## No próximo arquivo, fazer análises descritivas.