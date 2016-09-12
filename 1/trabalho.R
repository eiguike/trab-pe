#1 número de pushs por cada linguagem
dados <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/1/1.csv",header=TRUE)
barplot(dados$events, name = dados$language, main = "Número de Pushs por Linguagem (Q1-2014)")


#2 linguagens criadas por períodos de 7 anos (1950-2014)
dados <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/1/2.csv",header=TRUE)
mp <- barplot(dados$quantity, name = dados$year, main = "Número de Linguagens Criadas\nem períodos de 7 anos (1950-2014)", axisnames=FALSE, ylab = 'Quantidade de Linguagens Criadas')
text(mp, par("usr")[3], labels = labels, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)

