# para rodar os codigos abaixo devem estar instalados as bibliotecas abaixo
#e1071
#fdth
#modeest
#moments

##  exemplos elaborados por Luzia P. Oliveira

# Como entrar com os dados (digitando)

a <- c(32.3, 62.2, 10.3, 22.0, 13.1, 9.9, 11.9, 20.0, 36.4, 23.5, 18, 22.6, 20.3, 38.3, 19.6, 27.2, 28.9, 18.4, 27.3, 21.7, 23.7, 13.9, 36.3, 32.9, 29.7, 25.4, 23.8, 15.7, 17, 39.2, 22.7, 29.9, 18.3, 33)

a  #para visualizar os dados armazenados no vetor a


# Como importar dados de uma planilha csv

# Gravar a planilha de dados excel .xlsx na extensão csv (separado por virgulas), por exemplo: pelican.csv 

# ver o diretorio onde está o arquivo, por exemplo, clicando com o botão direito no nome do arquivo e depois na opção 
# propriedades (local)  Ex: arquivo pelican.csv em C:\UFSCar

#observacao: no R as barras são inclinadas para a direita (igual ao sinal de divisão)
# ou também podem ser usadas duas barras \\

# dependendo da versão do excel a importação dos dados é feita a partir de uma das linhas de comandos a seguir:

dados <- read.csv2(file="C:\\UFSCar\\Pelican.csv",header=TRUE) 

dados 




# no caso de leitura incorreta usar

dados <- read.csv(file="C:\\UFSCar\\Pelican.csv",header=TRUE) # depende da versão do excel. 

dados

#attach(dados)

summary(dados)  # resumo (algumas medidas descritivas para cada uma das variáveis contidas em dados)
names(dados)    # para ver os nomes das variaveis
head(dados,8)   # para ver as primeiras linhas do banco de dados 

dados$cliente=factor(dados$cliente)  # para que a variavel cliente (coluna de identificacao das linhas) 
                                     # seja considerada um fator (variável qualitativa)


#  TABELAS DE FREQUENCIAS E GRÁFICOS

#variável qualitativa

#informações tabela de frequencia
fa=table(dados$sexo) # frequencias absolutas
fa

fr=fa/length(dados$sexo) #frequencias relativas (proporções)
fr

frp=fa/length(dados$sexo)*100  #frequencias relativas (percentuais)
frp




fa=table(dados$estado_civil) # frequencias absolutas
fa

fr=fa/length(dados$estado_civil) #frequencias relativas (proporções)
fr

frp=fa/length(dados$estado_civil)*100  #frequencias relativas (percentuais)
frp


estado_civil
#gráficos
par(mfrow=c(1,1)) # janela com um único gráfico

# para obtencao de gráficos de barras pode-se usar os comandos barplot ou plot

barplot(fa) # grafico de barras  (observacao: é necessário colocar nomes nos eixos)

?barplot    # para entrar no help da função barplot

barplot(fa, xlab = "sexo", ylab = "nº entrevistados") # eixos com nomes
barplot(fa, xlab = "sexo", ylab = "nº entrevistados", col="lightgreen", ylim=c(0,100)) # para alterar a cor
barplot(fa, xlab = "sexo", ylab = "nº entrevistados", col="lightgreen", ylim=c(0,100)) # para alterar a cor

barplot(fa, xlab = "sexo", ylab = "nº entrevistados",horiz=TRUE, angle=45) #somente para mostrar que as barras podem ser invertidas
?barplot

#também pode ser utilizado o comando plot para obter o gráfico de barras
plot(dados$sexo)  # grafico
plot(dados$sexo, xlab = "sexo", ylab = "nº entrevistados", col="lightgreen", ylim=c(0,100) )  # grafico de barras

plot(dados$sexo,horiz=TRUE)   #somente para mostrar que as barras podem ser invertidas


#exercicio
# fazer o grafico de barras da varivel estado_civil


#exemplo pego na internet barras horizontais com titulos extensos
dados2 <- read.table(text='8 5 4 6 4 4 2
41 58 15 19 19 33 30
60 59 67 54 49 59 56
43 30 66 73 80 56 64', header=F, sep=' ')
dados2

fator_tecnologico_labels <- c(
  "Facilidade de testar e de ser avaliado\n por usuários de um modo geral",
  "Utilização de desenvolvimento e de qualidade\n bem definidos por parte do fabricante",
  "Compatibilidade com a infraestrutura existente,\n com os requisitos/necessidades/demandas, e/ou com a tecnologia\n em vigor",
  "Facilidade de entender, utilizar\n e/ou adaptar",
  "Vantagem(ns) em relação a custos com hardware,\n requisitos mínimos menos exigentes, custos com licença ou suporte, etc",
  "Maior eficiência em relação à solução,\n livre ou não, utilizada atualmente",
  "Maior confiabilidade em relação\n à solução, livre ou não, utilizada\n atualmente"  
)
dados2 <- cbind(dados2, Label=c('Nenhuma influência', 'Pouca influência', 'Influência considerável', 'Muita influência')) 
dados2
dados2 <- reshape(dados2, varying = list(c('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7')), direction='long', idvar='Label', times=fator_tecnologico_labels) # Para usar o ggplot2 os dados precisam estar nesse formato.
dados2

require(ggplot2)

grafico <- ggplot(aes(x=time, y=V1, fill=Label), data=dados2) + # Informa os dados
  geom_bar(stat='identity', position='dodge') + # Informa que o gráfico é de barras
  coord_flip() + # Inverte os eixos X e Y
  xlab('Fatores') + # Coloca titulo no eixo X
  ylab('Frequência') +  # Coloca titulo do eixo Y
  ggtitle("Na sua opinião, em que grau estes fatores influenciam\n a adoção de software livre do ponto de vista tecnológico?") + # Coloca titulo no grafico
  scale_fill_hue('Legenda') # Coloca titulo na legenda

grafico


#PIZZA
fa=table(dados$sexo);fa
fr=fa/sum(fa);fr
frp=fr*100; frp
frp_arred=round(frp,1); frp_arred  # arredondar para uma casa decimal as frequencias 

n=length(fr);n ##obter o numero de classes
pie(fr,xlab="sexo",col=rainbow(n))

#exemplo para incluir % e legenda
par(mfrow=c(1,1))
cols <- c("grey90","grey50")    #"black","grey30","white","grey70","grey50"
pielabels<- paste(frp, "%", sep="")
pie(frp, labels=pielabels, cex=1.2, col=cols)
legend("bottom", c("feminino","masculino"), cex=1.2, fill=cols, horiz=TRUE)  #(dica aumentar a altura da janela gráfica)
?legend

# horiz=TRUE - sem essa opcao a legenda fica na vertical 
# outras posicoes da legenda “bottomright”, “bottom”, “bottomleft”, “left”, “topleft”, “top”, “topright”, “right”, “center”
# cex -> altera o tamanho da fonte
# obs: se ficar sobreposto grafico e legenda alterar o tamanho da janela grafica (com o mouse) e rodar novamente os comandos


#outro exemplo (copiado do help)
## Another case showing pie() is rather fun than science:
## (original by FinalBackwardsGlance on http://imgur.com/gallery/wWrpU4X)
pie(c(Sky = 78, "Sunny side of pyramid" = 17, "Shady side of pyramid" = 5),
    init.angle = 315, col = c("deepskyblue", "yellow", "yellow3"), border = FALSE)

#OBS: clicando em Zoom é possível copiar a imagem usando control C caso a opção export modifique o gráfico obtido


######
#variavel quantitativa 

# tabela de frequencia ponto a ponto
  
# exemplo
x=c(10,11,12,15,rep(11,10),rep(13,17),rep(17,2))
x

#grafico - diagrama de pontos

stripchart(x, method="stack",xlab="x")

#outro exemplo variavel numero de itens
stripchart(dados$itens, method="stack",xlab="nº itens")


# diagrama de barras variável discreta (ponto a ponto)
fa <- table(x) 
fa

fr <- prop.table(fa)*100 
fr

plot(fr, ylab="%", xlab="nome da variavel")

#exemplo dados numero de itens (tabela de frequencia ponto a ponto)              
itens.freqabs = table(dados$itens);  itens.freqabs        #frequencia absoluta por classe
itens.freqrel =  itens.freqabs / length(dados$itens); itens.freqrel  #proporcao por classe
itens.cumfreqabs = cumsum(itens.freqabs); itens.cumfreqabs  #frequencias acumuladas

#para colocar as saidas anteriores em colunas
cbind(itens.freqabs,itens.freqrel, itens.cumfreqabs) 

#(grafico de barras ponto a ponto)
plot(itens.freqabs, ylab="nº entrevistados", xlab="nº itens")
#ou
plot(table(dados$itens), ylab="nº entrevistados", xlab="nº itens")


plot(itens.cumfreqabs, ylab="% entrevistados", xlab="nº itens") # grafico correto abaixo

plot(ecdf(dados$itens))  #grafico frequencia acumulada numero de itens

plot(ecdf(dados$itens), main="") # grafico acima sem o titulo


# tabela de frequencia em classes (passo a passo) 
#exemplo dados numero de itens              
sort(dados$itens)
breaks = seq(1, 19, by=2); breaks 
itens.cut = cut(dados$itens, breaks, right=FALSE);  
itens.freqabs = table(itens.cut);  itens.freqabs        #frequencia absoluta por classe
itens.freqrel =  itens.freqabs / length(dados$itens); itens.freqrel  #proporcao por classe
itens.cumfreqabs = cumsum(itens.freqabs); itens.cumfreqabs  #frequencias acumuladas

#para ver as saidas anteriores em colunas
cbind(itens.freqabs, itens.freqrel, itens.cumfreqabs) 


#####################################
#tabelas de frequencias usando a biblioteca fdth  
#####################################

require(fdth)  #library para criar as tabelas de frequencias
?fdth          #help
             
#A construção de tabelas de distribuição de frequências para variáveis quantitativas contínuas é feita
#agrupando os dados em classes e obtendo as frequências observadas em cada classe. 
#Para fazer isso pode-se utilizar a função fdt pacote fdth. Esta função é definida da seguinte forma:
#fdt(x, k, start, end, h, breaks=c("Sturges", "Scott", "FD")) em que:
#• x - são os dados a serem agrupados;
#• k - numero de classes;
#• start - limite inferior da primeira classe;
#• end - limite superior da ultima classe;
#• h - amplitude da classe;
#• breaks - caso não seja definido k, start, end e h, pode-se utilizar um método para isso


# tabela de frequencia em classes - exemplo 
#exemplo variavel numero de itens
tab=fdt(dados$itens, start=1,h=2,end=19)
print(tab)

#saida
# print(tab)
# Class limits  f   rf rf(%)  cf cf(%)
#        [1,4) 66 0.66    66  66    66
#        [4,7) 26 0.26    26  92    92
#       [7,10)  5 0.05     5  97    97
#      [10,13)  1 0.01     1  98    98
#      [13,16)  1 0.01     1  99    99
#      [16,19)  1 0.01     1 100   100


#exemplo com os dados de vendas
# formula para obter o número de classes
k=1+(3.3*log10(length(dados$itens))); k    #uma formula para obter o numero aproximado de classes
tab=fdt(dados$vendas, k=7); tab # 

sort(dados$vendas)
tab=fdt(dados$vendas, start=10,h=40,end=290); tab  #tabelas de frequencias

#criterios para sugerir as classes
#tab=fdt(dados$vendas,  breaks=c("Sturges")); tab
#tab=fdt(dados$vendas,  breaks=c("Scott")); tab
#tab=fdt(dados$vendas,  breaks=c("FD")); tab

#graficos 

#exemplo graficos a partir de dados tabelados
par(mfrow=c(1,2))

plot(tab, xlab="vendas",ylab="n° clientes")                 #histograma frequencias absolutas
plot(tab,type='cdh', xlab="vendas",ylab="densidade acumulada", ylim=c(0,1))      #densidade acumulada

plot(tab,type='rfh', xlab="vendas",ylab="proporção clientes")      #histograma proporcao
plot(tab,type='cdp', xlab="vendas", ylab="densidade acumulada", ylim=c(0,1), col="black")      #ogiva

#exemplos graficos a partir de dados brutos      (inclui poligono de frequencias)

##poligono de frequencia com histograma
par(mfrow=c(1,1))
histinfo<-hist(a, main="", xlab="nome variavel x", ylab="nome variavel y")
histinfo # para ver os intervalos usados para as classes
# para alterar as configuracoes padroes

histinfo=hist(a, breaks=seq(5,65,by=5), right=FALSE, main="", xlab="nome variavel x", ylab="nome variavel y")
histinfo

# Note that when giving breakpoints, the default for R is that the histogram cells are right-closed (left open) intervals of the form (a,b]. You can change this with the right=FALSE option, which would change the intervals to be of the form [a,b).  This is important if you have a lot of points exactly at the breakpoint.


#Often, we are more interested in density than frequency, since frequency is relative to your 
#sample size. Instead of counting the number of datapoints per bin, R can give the probability
#densities using the freq=FALSE option:
hist(a, main="")
hist(a, freq=FALSE, main="")

#exemplo dados do numero de itens (histograma por classes) 
hist(dados$itens, main="", ylab="nº entrevistados", xlab="nº itens")

#exemplo dados de vendas (histograma por classes) 
hist(dados$vendas, main="", ylab="nº entrevistados", xlab="vendas")

#exemplo idades (histograma por classes) 
hist(dados$idade, main="", ylab="nº entrevistados", xlab="idade")



#histograma e poligono de frequencia
#exemplo variavel idade
par(mfrow=c(1,1))
#somente histograma 
#hist(dados$idade,main="",xlab="idade")
h=hist(dados$idade,main="",xlab="idade",ylab="n° clientes")
##poligono de frequencia com histograma
lines(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts, 0), type = "l")
##somente poligono de frequencia
plot(c(min(h$breaks), h$mids, max(h$breaks)), c(0,h$counts,0), type = "l",main="",xlab="idade",ylab="n° clientes")

#diagrama de caixas
par(mfrow=c(1,1))
boxplot(dados$vendas, ylab = "vendas")
boxplot(dados$vendas ~ dados$sexo, xlab = "sexo", ylab = "vendas")

#medidas descritivas

mean(dados$vendas)       #media
median(dados$vendas)     #mediana

#moda
require(modeest)
mfv(dados$itens)
mfv(dados$idade)
mfv(dados$vendas)

#graficos para checar
stripchart(dados$itens, method="stack",xlab="nº itens")
stripchart(dados$idade, method="stack",xlab="idade")
stripchart(dados$vendas, method="stack",xlab="vendas")

sort(dados$itens)
sort(dados$idade)
sort(dados$vendas)

##Moda de Idade por Sexo
tapply(dados$idade,dados$sexo, mfv)

 
#Também é possivel estimar a moda por diferentes métodos no caso de variavel continua: 
#de Lientz, Chernoff, Venter, Grenander,HSM, HRM, Kernel de Parzen, Tsybakov, Asselin de Beauville e Vieu.
##moda de vendas
mlv(dados$vendas, method = "lientz", bw = 0.2)

#medidas descritivas
quantile(dados$vendas, c(0.10, 0.25, 0.70))  # percentis 10% 25% e 70%
sd(dados$vendas)
var(dados$vendas)
summary(dados$vendas) #(mínimo, 1o quartil, mediana, media, 3o quartil e máximo)

#a seguir duas bibliotecas para obter medidas de simetria e curtose
#tarefa (encontrar uma biblioteca que use as formulas do excel)
library(moments)
skewness(dados$vendas)
kurtosis(dados$vendas)
 
library(e1071)
skewness(dados$vendas)
kurtosis(dados$vendas)


# exemplo para retirar informações = 0

dados$desconto   #exibir dados
sort(dados$desconto) # ordenar dados
length(dados$desconto[dados$desconto>0])   #não considerar informações = 0
summary(dados$desconto[dados$desconto>0])  #resumo sem as informações iguais a zero
desconto_sem_zeros <-dados$desconto[dados$desconto>0]
desconto_sem_zeros
sort(desconto_sem_zeros) # ordenar dados
length(desconto_sem_zeros)   #não considerar informações = 0
summary(desconto_sem_zeros)  #resumo sem as informações iguais a zero
hist(desconto_sem_zeros, main = "", ylab = "no de clientes", col="lightblue") # histograma

hist(dados$desconto, main = "", ylab = "no de clientes", col="lightblue") # histograma

######### Análise estatística conjunta para duas variáveis
#**********************************************************


######### duas variáveis qualitativas

fa=table(dados$sexo,dados$estado_civil) ##frequencia absoluta
fa
fr=fa/sum(fa) ##frequencia relativ
fr
fp=100*fr ##fp=frequencia percentual
fp



######### uma variável qualitativa e uma quantitativa

par(mfrow=c(1,2))    # exibe os gráficos em 1 linha e duas
by(dados$vendas, dados$sexo,summary)  # resumo das vendas por categoria da variável sexo
boxplot(dados$vendas ~ dados$sexo, xlab = "sexo", ylab = "vendas")

by(dados$vendas, dados$estado_civil,summary) # resumo das vendas por categoria da variável estado civil
boxplot(dados$vendas ~ dados$estado_civil, xlab = "estado civil", ylab = "vendas")



######### duas variáveis quantitativas

# Exemplo análise correlação

# DADOS UTILIZADOS  - RELAÇÃO ENTRE COMERCIAIS E VENDAS 

###########  ENTRADA DOS DADOS  #################
x<- c(2, 5, 1, 3, 4, 1, 5, 3, 4, 2)  # número de comerciais
y<- c(50, 57, 41, 54, 54, 38, 63, 48, 59, 46)  # vendas


###########  DIAGRAMA DE DISPERSÃO  #################
par(mfrow=c(1,1))
plot(y~x, xlab = "comerciais", ylab = "vendas", col = "blue") #diagrama de dispersão


###########  COEFICIENTE DE CORRELAÇÃO DE PEARSON  #################
pearson <- cor(y,x)     # coeficiente de Pearson
pearson


###########  COEFICIENTE DE CORRELAÇÃO DE SPEARMAN  #################
spearman <- cor(y, x, method = "spearman")  # coeficiente de Spearman
spearman

###########  OBTENÇÃO DA RETA DE REGRESSÃO  #################
ajuste <- lm(y~x)   # ajuste da reta de regressão
summary(ajuste)

###########  DIAGRAMA DE DISPERSÃO E RETA DE REGRESSÃO AJUSTADA  #################
plot(y~x, xlab = "comerciais", ylab = "vendas", col = "blue")  # diagrama de dispersão
abline(ajuste, col= "red")   # + reta ajustada



