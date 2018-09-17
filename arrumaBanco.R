# settings ----
  library(readr)

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
  
# Arrumando o banco ----
  #falta:
  # 1 - converter os perímetros em diâmetros (dados %>% mutate(diametro = perimetro * ...) %>% select (-perimetro))
  # 2 - Tirar as linhas em que diâmetro, X2, X5 são NA (dados %>% filter(!is.na(diametro) | !is.na(X2) | !is.na(X5)))
  # 3 - Fazer as replicações das linhas, onde encontrar valores em X2 (subs 3.5) e X5 (subs 7.5).
  #     Ao final devem ter 579 observações (sum(dados$X2, na.rm = T) + sum(dados$X5, na.rm = T) + sum(!is.na(dados$perimetro)))
  # 4 - salvar com save(dados, file = "dados.rda")
  
  ## No próximo arquivo, extrair a variável resposta com dados %>% group_by(linha, parcela, sei lá o q) %>% summarise(vol = pi * sum(diametro)/(8*45)) (acho)
  ## No próximo arquivo, fazer análises descritivas.