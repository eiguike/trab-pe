#gráfico de distribuição acumulada caso discreto - escada (R)
dados <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/2/3.csv",header=TRUE)
plot(ecdf(dados$commits), main = 'Frequência acumulada por Commits\n2014/1-2014/3',
     xlab = 'Commits', ylab = 'Semestre',
     yaxt='n')

#gráfico de linhas verticais
plot(1:3,dados$commits, type='h', main='Commits acumulados entre\nos bimestres de 2014', 
     ylab='Commits', xlab= 'Bimestre', xaxt = 'n')
axis(1,dados$semester)

