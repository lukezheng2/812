# Descritivas -------------------------------------------------------------
#Percentuais
rbind(ant,pos) %>% 
  select(estrutura) %>% 
  table %>% as.data.frame() %>% 
  mutate(Perc = Freq/nrow(dados))

#Perimetro dos troncos
rbind(ant,pos) %>% 
  mutate(Cat_diametro = ifelse(diametro<5, "(2-5)", NA),
         Cat_diametro = ifelse(diametro<10 & diametro>=5, "(5-10)", Cat_diametro),
         Cat_diametro = ifelse(diametro>=10, "(+10)", Cat_diametro)) %>% 
  select(Cat_diametro, decomp) %>% 
  table %>% as.data.frame() %>% 
  mutate(Perc = Freq/sum(Freq))



rbind(ant,pos) %>% 
  mutate(Cat_diametro = ifelse(diametro<5, "(2-5)", NA),
         Cat_diametro = ifelse(diametro<10 & diametro>=5, "(5-10)", Cat_diametro),
         Cat_diametro = ifelse(diametro>=10, "(+10)", Cat_diametro)) %>% 
         {ggplot(., aes(factor(Cat_diametro, levels = c("(2-5)", "(5-10)", "(+10)")))) +
             geom_bar(color="black") +
             theme(legend.position = "none") +
             labs(x = "Diâmetro" , y = "Frequência") +
             scale_color_brewer(palette = "Blues")
         }

# Decomposição (como adecomposicao muda de antes e depois de 2018, temos que dividir)

ant %>% 
  ggplot(aes(decomp)) +
  geom_bar(color="black") +
  theme(legend.position = "none") +
  labs(x = "Grau de decomposição" , y = "Frequência") +
  scale_color_brewer(palette = "Blues")

pos %>% 
  ggplot(aes(decomp)) +
  geom_bar(color="black") +
  theme(legend.position = "none") +
  labs(x = "Grau de decomposição" , y = "Frequência") +
  scale_color_brewer(palette = "Blues")

# Estrutura

rbind(ant,pos) %>% 
  ggplot(aes(estrutura)) +
  geom_bar(color="black") +
  theme(legend.position = "none") +
  labs(x = "Diâmetro" , y = "Frequência") +
  scale_color_brewer(palette = "Blues")


# Podridao 

rbind(ant,pos) %>% 
  ggplot(aes(podridao)) +
  geom_bar(color="black") +
  theme(legend.position = "none") +
  labs(x = "Podridao" , y = "Frequência") +
  scale_color_brewer(palette = "Blues")


# descritiva - Por parcela -------------------------------------------------------------



#Perimetro dos troncos

p1 <- rbind(ant,pos) %>% 
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

p2ant <- ant %>% 
  ggplot(aes(x = Parcela2, group = decomp, fill = decomp)) +
  geom_bar(color="black", position = "fill") +
  labs(x = "Parcela" , y = "Percentual") +
  scale_fill_brewer(palette = "Dark2", "Grau de decomposição")

p2pos <- pos %>% 
  ggplot(aes(x = Parcela2, group = decomp, fill = decomp)) +
  geom_bar(color="black", position = "fill") +
  labs(x = "Parcela" , y = "Percentual") +
  scale_fill_brewer(palette = "Dark2", "Grau de decomposição")



# Estrutura

p3 <- rbind(ant,pos) %>% 
  ggplot(aes(x = Parcela2, group = estrutura, fill = estrutura)) +
  geom_bar(color="black", position = "fill") +
  #theme(legend.position = "none") +
  labs(x = "Parcela" , y = "Percentual") +
  scale_fill_brewer(palette = "Dark2", "Estrutura")

# Podridao 

p4 <- rbind(ant,pos) %>% 
  ggplot(aes(x = Parcela2, group = podridao, fill = podridao)) +
  geom_bar(color="black", position = "fill") +
  labs(x = "Parcela" , y = "Percentual") +
  scale_fill_brewer(palette =  "Dark2", "Podridão")

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

# Tipo de estrutura por decomposição

ant %>% 
  ggplot(aes(x = estrutura, group = podridao, fill = podridao)) +
  geom_bar(color="black", position = position_dodge()) +
  labs(x = "" , y = "Percentual") +
  scale_fill_brewer(palette =  "Dark2", "Podridão")

pos %>% 
  ggplot(aes(x = estrutura, group = podridao, fill = podridao)) +
  geom_bar(color="black", position = position_dodge()) +
  labs(x = "" , y = "Percentual") +
  scale_fill_brewer(palette =  "Dark2", "Podridão")
#Decomposição x Diametro
rbind(ant,pos) %>% 
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
