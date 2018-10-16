#settings ----
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  load("dados.rda")
  
  grausDeg = data.frame(decomp = factor(1:5), densidade = c(0.40, 0.30, 0.19, 0.13, 0.09))

# Estimando o volume ----
  massaGrau = dados %>% group_by(tratamento, replicacao, linha, decomp, ano = year(data)) %>%
              summarise(volumeTotal = pi^2/(8*45) * sum(diametro^2))
  
  massaGrau = inner_join(massaGrau, grausDeg) %>% mutate(massa = densidade * volumeTotal)
  
  massaTotal = massaGrau %>% group_by(tratamento, replicacao, linha, ano) %>% summarise(massa = sum(massa))

# ANOVA ----
  aov(massa ~ tratamento, data = massaTotal) %>% anova()
  