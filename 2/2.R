dados <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/2/agrupamento_formula.csv",header=TRUE)

plot(v,dados$quantity, type='l', xaxt='n', ylab = 'Quantidade de Linguagens Criadas', main = 'Linguagens Criadas desde 1950, \n em um intervalo de 7 anos')
axis(1, at=1:10, labels=FALSE)


labels <- dados$year[1:10]
text(x=dados$year, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),
     labels=labels, srt=45, adj=1, xpd=TRUE)

