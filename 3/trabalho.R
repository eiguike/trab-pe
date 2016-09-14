#gráfico de distribuição acumulada caso discreto - escada (R)
dados <- read.csv(file="/home/floss/Desenvolvimento/trab-pe/3/3.csv",header=TRUE)
plot(ecdf(dados$commits), main = 'Frequência acumulada por Commits\n2013/1-2013/4',
     xlab = 'Commits', ylab = 'Semestre')

#gráfico de linhas verticais
plot(1:4,dados$commits, type='h', main='Commits feitos entre\nos bimestres de 2013', 
     ylab='Commits', xlab= 'Bimestre', xaxt = 'n')
axis(1,dados$semester)

