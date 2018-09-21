library(ggplot2)

# Discritivas -------------------------------------------------------------

#Perimetro dos troncos
dados %>% 
  mutate(Cat_diametro = ifelse(diametro<5, "(2-5)", NA),
         Cat_diametro = ifelse(diametro<10 & diametro>=5, "(5-10)", Cat_diametro),
         Cat_diametro = ifelse(diametro>=10, "(+10)", Cat_diametro)) %>% 
  select(Cat_diametro) %>% 
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

dados %>% 
  mutate(Cat_diametro = ifelse(diametro<5, "(2-5)", NA),
         Cat_diametro = ifelse(diametro<10 & diametro>=5, "(5-10)", Cat_diametro),
         Cat_diametro = ifelse(diametro>=10, "(+10)", Cat_diametro)) %>% 
         {ggplot(., aes(x = parcela,
                        group = factor(Cat_diametro, levels = c("(2-5)", "(5-10)", "(+10)")),
                        fill = factor(Cat_diametro, levels = c("(2-5)", "(5-10)", "(+10)")))) +
             geom_bar(color="black", position = "fill") +
             #theme(legend.position = "none") +
             labs(x = "Diâmetro" , y = "Percentual") +
             scale_fill_brewer(palette = "Dark2", "Diâmetro")
         }

# Decomposição 

dados %>% 
  ggplot(aes(x = parcela, group = decomp, fill = decomp)) +
  geom_bar(color="black", position = "fill") +
  labs(x = "" , y = "Percentual") +
  scale_fill_brewer(palette = "Dark2", "Grau de decomposição")



# Estrutura

dados %>% 
  ggplot(aes(x = parcela, group = estrutura, fill = estrutura)) +
  geom_bar(color="black", position = "fill") +
  #theme(legend.position = "none") +
  labs(x = "" , y = "Percentual") +
  scale_fill_brewer(palette = "Dark2", "Estrutura")

# Podridao 

dados %>% 
  ggplot(aes(x = parcela, group = podridao, fill = podridao)) +
  geom_bar(color="black", position = "fill") +
  labs(x = "" , y = "Percentual") +
  scale_fill_brewer(palette =  "Dark2", "Podridão")
