#settings ----
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  load("dados.rda")
  
  grausDeg = data.frame(decomp = factor(1:5), densidade = c(0.40, 0.30, 0.19, 0.13, 0.09))

# Estimando o volume ----
  volumesGrau = dados %>% group_by(tratamento, replicacao, linha, decomp, ano = year(data)) %>%
              summarise(volumeTotal = pi^2/(8*45) * sum(diametro^2))
  
  volumesGrau = inner_join(volumesGrau, grausDeg) %>% mutate(massa = densidade * volumeTotal)
  
  volumesTotal = volumesGrau %>% group_by(tratamento, replicacao, linha, ano) %>% summarise(massa = sum(massa))

# ANOVA ----
  aov(massa ~ tratamento, data = volumesTotal) %>% anova()
  