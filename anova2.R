library(magrittr)
library(ggplot2)

load("dados.rda")

grausDeg = data.frame(decomp = factor(1:5), densidade = c(0.40, 0.30, 0.19, 0.13, 0.09))

volAnt = ant %>% filter(ano == 2017) %>% group_by(linha, tratamento, replicacao, ano, decomp) %>% summarise(vol = pi * sum(diametro)/(8*45))
volPos = pos %>% group_by(linha, tratamento, replicacao, ano, decomp) %>% summarise(vol = pi * sum(diametro)/(8*45))
vol10  = rbind(ant, pos) %>% filter(diametro > 10) %>% group_by(linha, tratamento, replicacao, ano, decomp) %>% summarise(vol = pi * sum(diametro)/(8*45))

massaAnt = inner_join(volAnt, grausDeg) %>% mutate(massa = vol * densidade)
massaPos = inner_join(volPos, grausDeg) %>% mutate(massa = vol * densidade)
massa10  = inner_join(vol10,  grausDeg) %>% mutate(massa = vol * densidade)

fit = aov(log(massa) ~ tratamento * replicacao * linha, data = massa10) 
anova(fit)
plot(resid(fit)); abline(h = 0)
qqnorm(residuals(fit)); qqline(residuals(fit))

shapiro.test(resid(fit))
leveneTest(log(massa) ~ tratamento * replicacao * linha, data = massaAnt)

shapiro.test(log(massaPos$massa))
log(massaPos$massa) %>% hist()
ggplot(massaPos, aes(x = log(massa), fill = factor(tratamento))) + geom_histogram(binwidth = .2)

       