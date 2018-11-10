##extrair variável resposta

#antes de 2018
 respostaAnt = ant %>%
  group_by(linha, tratamento, replicacao, decomp, podridao, ano) %>% summarise(vol = pi * sum(diametro)/(8*45))
 
 #2018
 respostaPos = pos %>%
   group_by(linha, tratamento, replicacao, decomp, podridao, ano) %>% summarise(vol = pi * sum(diametro)/(8*45))
######### Talvez tirar a podridao pois tem mto NA
 
 
