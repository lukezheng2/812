library(ggplot2)
library(reshape2)
library(stringr)
load("dados.rda")

dados %<>% cbind(str_split_fixed(dados$parcela, "", 2))

names(dados)[14] <- "Parcela2"
names(dados)[15] <- "Replicacao"

# Discritivas -------------------------------------------------------------
#Percentuais
dados %>% 
  select(estrutura) %>% 
  table %>% as.data.frame() %>% 
  mutate(Perc = Freq/nrow(dados))

#Perimetro dos troncos
dados %>% 
  mutate(Cat_diametro = ifelse(diametro<5, "(2-5)", NA),
         Cat_diametro = ifelse(diametro<10 & diametro>=5, "(5-10)", Cat_diametro),
         Cat_diametro = ifelse(diametro>=10, "(+10)", Cat_diametro)) %>% 
  select(Cat_diametro, decomp) %>% 
  table %>% as.data.frame() %>% 
  mutate(Perc = Freq/sum(Freq))



dados %>% 
  mutate(Cat_diametro = ifelse(diametro<5, "(2-5)", NA),
         Cat_diametro = ifelse(diametro<10 & diametro>=5, "(5-10)", Cat_diametro),
         Cat_diametro = ifelse(diametro>=10, "(+10)", Cat_diametro)) %>% 
         {ggplot(., aes(factor(Cat_diametro, levels = c("(2-5)", "(5-10)", "(+10)")))) +
             geom_bar(color="black") +
             theme(legend.position = "none") +
             labs(x = "Diâmetro" , y = "Frequência") +
             scale_color_brewer(palette = "Blues")
         }

# Decomposição 

dados %>% 
  ggplot(aes(decomp)) +
  geom_bar(color="black") +
  theme(legend.position = "none") +
  labs(x = "Grau de decomposição" , y = "Frequência") +
  scale_color_brewer(palette = "Blues")



# Estrutura

dados %>% 
  ggplot(aes(estrutura)) +
  geom_bar(color="black") +
  theme(legend.position = "none") +
  labs(x = "Diâmetro" , y = "Frequência") +
  scale_color_brewer(palette = "Blues")


# Podridao 

dados %>% 
  ggplot(aes(podridao)) +
  geom_bar(color="black") +
  theme(legend.position = "none") +
  labs(x = "Podridao" , y = "Frequência") +
  scale_color_brewer(palette = "Blues")


# descritiva - Por parcela -------------------------------------------------------------



#Perimetro dos troncos

p1 <- dados %>% 
  mutate(Cat_diametro = ifelse(diametro<5, "(2-5)", NA),
         Cat_diametro = ifelse(diametro<10 & diametro>=5, "(5-10)", Cat_diametro),
         Cat_diametro = ifelse(diametro>=10, "(+10)", Cat_diametro)) %>% 
         {ggplot(., aes(x = Parcela2,
                        group = factor(Cat_diametro, levels = c("(2-5)", "(5-10)", "(+10)")),
                        fill = factor(Cat_diametro, levels = c("(2-5)", "(5-10)", "(+10)")))) +
             geom_bar(color="black", position = "fill") +
             #theme(legend.position = "none") +
             labs(x = "Parcela" , y = "Percentual") +
             scale_fill_brewer(palette = "Dark2", "Diâmetro (cm)")
         }

# Decomposição 

p2 <- dados %>% 
  ggplot(aes(x = Parcela2, group = decomp, fill = decomp)) +
  geom_bar(color="black", position = "fill") +
  labs(x = "Parcela" , y = "Percentual") +
  scale_fill_brewer(palette = "Dark2", "Grau de decomposição")



# Estrutura

p3 <- dados %>% 
  ggplot(aes(x = Parcela2, group = estrutura, fill = estrutura)) +
  geom_bar(color="black", position = "fill") +
  #theme(legend.position = "none") +
  labs(x = "Parcela" , y = "Percentual") +
  scale_fill_brewer(palette = "Dark2", "Estrutura")

# Podridao 

p4 <- dados %>% 
  ggplot(aes(x = Parcela2, group = podridao, fill = podridao)) +
  geom_bar(color="black", position = "fill") +
  labs(x = "Parcela" , y = "Percentual") +
  scale_fill_brewer(palette =  "Dark2", "Podridão")

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

# Tipo de estrutura por decomposição

dados %>% 
  ggplot(aes(x = estrutura, group = podridao, fill = podridao)) +
  geom_bar(color="black", position = position_dodge()) +
  labs(x = "" , y = "Percentual") +
  scale_fill_brewer(palette =  "Dark2", "Podridão")

#Decomposição x Diametro
dados %>% 
  mutate(Cat_diametro = ifelse(diametro<5, "(2-5)", NA),
         Cat_diametro = ifelse(diametro<10 & diametro>=5, "(5-10)", Cat_diametro),
         Cat_diametro = ifelse(diametro>=10, "(+10)", Cat_diametro)) %>% 
         {ggplot(., aes(x = decomp,
                        group = factor(Cat_diametro, levels = c("(2-5)", "(5-10)", "(+10)")),
                        fill = factor(Cat_diametro, levels = c("(2-5)", "(5-10)", "(+10)")))) +
             geom_bar(color="black") +
             #theme(legend.position = "none") +
             labs(x = "Grau de decomposição" , y = "Frequência") +
             scale_fill_brewer(palette = "Dark2", "Diâmetro (cm)")}
