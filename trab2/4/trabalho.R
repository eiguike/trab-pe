# No caso (4) apresentar:
# a) Tabela de Frequencias por intervalo;
# b) Histograma e polígono de frequencias (EXCEL e R);
# c) ogiva de galton (R);
# d) diagrama de caixas (boxplot) (R).

#histograma e polígono de frequências com Java
dados1 <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/trab2/4/1240_java.csv",header=TRUE)
h = hist(dados1$time,10, main="Tempo de Execução\nExercício 1240 com Java", xlab='Tempo', ylab ='Frequência')
lines(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts, 0), type = "l")

#histograma e polígono de frequências com C/C++
dados2 <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/trab2/4/1240_c.csv",header=TRUE)
h = hist(dados2$time,10, main="Tempo de Execução\nExercício 1240 com C/C++", xlab='Tempo', ylab ='Frequência')
lines(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts, 0), type = "l")

# Ogiva de Galton(Polígono de Frequências (%) Acumuladas))
require (fdth);
freq = fdt(dados2)
plot(freq,type='cfpp', xlab="Limites das classes", ylab="Frequencia", main="Ogiva de Galton")

dados3 <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/trab2/4/1240.csv",header=TRUE)
boxplot(dados3$time~dados1$language, main ='Diagrama de Caixa para Benchmark\nExercício 1240',ylab='Tempo', xlab='Linguagem')

#a seguir duas bibliotecas para obter medidas de simetria e curtose
#tarefa (encontrar uma biblioteca que use as formulas do excel)
library(moments)
skewness(dados1$time)
kurtosis(dados1$time)
 
library(e1071)
skewness(dados1$time)
kurtosis(dados1$time)

boxplot(dados1$time, main ='Diagrama de Caixa para Benchmark para Java\nExercício 1240',ylab='Tempo', xlab='Linguagem')
