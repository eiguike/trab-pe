# No caso (4) apresentar: 
# a) Tabela de Frequencias por intervalo; 
# b) Histograma e polígono de frequencias (EXCEL e R); 
# c) ogiva de galton (R); 
# d) diagrama de caixas (boxplot) (R).

#histograma e polígono de frequências com Java
dados1 <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/4/1240_java.csv",header=TRUE)
h = hist(dados1$time,10, main="Tempo de Execução\nExercício 1240 com Java", xlab='Tempo', ylab ='Frequência')
lines(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts, 0), type = "l")

#histograma e polígono de frequências com C/C++
dados1 <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/4/1240_c.csv",header=TRUE)
h = hist(dados1$time,10, main="Tempo de Execução\nExercício 1240 com C/C++", xlab='Tempo', ylab ='Frequência')
lines(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts, 0), type = "l")


