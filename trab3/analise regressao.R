###########  ANÁLISE DE REGRESSÃO NO R-PROJECT  #################
############  Profa. Luzia Pedroso de Oliveira  ##############

###########  REGRESSÃO LINEAR  #################

# DADOS UTILIZADOS  - RELAÇÃO ENTRE COMERCIAIS E VENDAS
###########  ENTRADA DOS DADOS  #################
x<- c(2, 5, 1, 3, 4, 1, 5, 3, 4, 2)  # número de comerciais
y<- c(50, 57, 41, 54, 54, 38, 63, 48, 59, 46)  # vendas


###########  ANÁLISE DESCRITIVA VARIÁVEIS E DIAGRAMA DE DISPERSÃO  #################
par(mfrow=c(2,2))
boxplot(x, ylab = "n° comerciais")
points(mean(x),col="red",pch=3)
mtext(" \n (a)" , side=1, line=4, cex=0.8)   #o valor de cex deve ser ajustado

boxplot(y, ylab = "vendas")
points(mean(y),col="red",pch=3)
mtext(" \n (b)" , side=1, line=4, cex=0.8)   #o valor de cex deve ser ajustado

plot(y~x, xlab = "n° comerciais", ylab = "vendas", col = "blue") #diagrama de dispersão
mtext(" \n (c)" , side=1, line=4, cex=0.8)   #o valor de cex deve ser ajustado


###########  COEFICIENTE DE CORRELAÇÃO DE PEARSON  #################
pearson <- cor(y,x)     # coeficiente de Pearson
pearson


###########  COEFICIENTE DE CORRELAÇÃO DE SPEARMAN  #################
spearman <- cor(y, x, method = "spearman")  # coeficiente de Spearman
spearman


###########  ESTIMACAO DOS COEFICIENTES DE REGRESSAO  #################
ajuste <- lm(y~x)   # ajuste da reta
summary(ajuste)


###########  VERIFICAÇÃO DAS SUPOSIÇÕES DO MODELO DE REGRESSÃO LINEAR POLINOMIAL - GRAU 1  #################
preditos <- predict(ajuste); preditos  # valores preditos ou previstos
residuos <- resid(ajuste); residuos      # resíduos

# para obter os resíduos padronizados
anova(ajuste)
estimativa_dp_res=sqrt(anova(ajuste)$Mean[2]);  estimativa_dp_res   # estimativa do erro padrão dos erros = quadrado médio dos resíduos
res_padronizado=residuos/estimativa_dp_res; res_padronizado # resíduos padronizados

par(mfrow=c(2,2)) # criando um painel para 4 gráficos
boxplot(res_padronizado, ylab="resíduos padronizados") #diagrama de caixas dos resíduos padronizados
points(mean(res_padronizado),col="red",pch=3)

require(car)
qqPlot(res_padronizado, distribution="norm", envelope=.95, xlab="quantis da normal", ylab="resíduos padronizados") #gráfico de probabilidades normal com envelope dos resíduos padroninados

plot(preditos, res_padronizado, xlab="preditos", ylab="resíduos padronizados" ) # diagrama de dispersão preditos x residuos padronizados
abline(h=0)

shapiro.test(res_padronizado)  # teste de hipótese para verificar normalidade dos resíduos padronizados

###########  INTERVALOS DE CONFIANÇA PARA BETA0 e BETA1  #################
require(MASS)
confint(ajuste) # IC 95% de confiança para os parâmetros beta0 e beta1

#### ou (obtencao passo a passo)
# intervalos de confianca para beta0, beta1 e beta2
ajuste <- lm(y~x)   # ajuste da forma quadrática
summary(ajuste)

coef=summary(ajuste)$coefficients[1,1] ; coef
err=summary(ajuste)$coefficients[1,2] ; err
coef + c(-1,1)*err*qt(0.975, (length(y)-2))  # intervalos de confianca para beta0

coef=summary(ajuste)$coefficients[2,1] ; coef
err=summary(ajuste)$coefficients[2,2] ; err
coef + c(-1,1)*err*qt(0.975, (length(y)-2))  # intervalos de confianca para beta1


###########  PREVISÃO PARA A MÉDIA E PARA UMA NOVA OBSERVAÇÃO E RESPECTIVOS INTERVALOS DE CONFIANÇA   #################

#criar um vetor com as novas observações - nesse caso foi criado um vetor contendo os valores 5, 6 e 7 comerciais
novos_x <- data.frame(x = seq(5, 7, 1)) ; novos_x
ajuste <- lm(y~x)

int_conf_nova_obs <- predict(ajuste, novos_x, interval="prediction")  # intervalo de confiança 95% para novas obs (5 6 e 7 comerciais)
int_conf_nova_obs
int_conf_media <- predict(ajuste, novos_x, interval="confidence")  # intervalo de confiança 95% para as médias (5 6 e 7 comerciais)
int_conf_media

# exemplo considerando outro nivel de confiança
int_conf_nova_obs <- predict(ajuste, novos_x, level = 0.90, interval="prediction")  # intervalo de confiança 95% para novas obs (5 6 e 7 comerciais)
int_conf_nova_obs
int_conf_media <- predict(ajuste, novos_x, level = 0.90, interval="confidence")   # intervalo de confiança 95% para as médias (5 6 e 7 comerciais)
int_conf_media

#######  DIAGRAMA DE DISPERSÃO COM BANDAS DE CONFIANÇA PARA AS MÉDIAS E PARA NOVAS OBSERVAÇÕES ########

novos_x <- seq(min(x), max(x), length.out=100) ; novos_x

int_conf_nova_obs <- predict(ajuste, newdata = data.frame(x=novos_x), interval="prediction")  # intervalo de confiança 95% para novas obs (5 6 e 7 comerciais)
int_conf_nova_obs
int_conf_media <- predict(ajuste, newdata = data.frame(x=novos_x), interval="confidence")  # intervalo de confiança 95% para as médias (5 6 e 7 comerciais)
int_conf_media

# gráfico
par(mfrow=c(1,1))
plot(y ~ x, type = 'n', xlab="comerciais", ylab="vendas" )
polygon(c(rev(novos_x), novos_x), c(rev(int_conf_nova_obs[ ,3]), int_conf_nova_obs[ ,2]), col = 'grey95',  border = NA)   #acrescentar bandas para novas observações
polygon(c(rev(novos_x), novos_x), c(rev(int_conf_media[ ,3]), int_conf_media[ ,2]), col = 'grey90',  border = NA) #acrescentar bandas para as médias
points(x, y, type = 'p')
# acrescenta curva
lines(novos_x, int_conf_nova_obs[ ,1], col="blue")
lines(novos_x, int_conf_nova_obs[ ,2], col="blue", lty = 5)
lines(novos_x, int_conf_nova_obs[ ,3], col="blue", lty = 5)  #lty = 'dashed'

#lines(novos_x, int_conf_media[ ,1], col="blue")
lines(novos_x, int_conf_media[ ,2], col="blue", lty = 'dashed')
lines(novos_x, int_conf_media[ ,3], col="blue", lty = 'dashed')  #lty = 'dashed'




###########  REGRESSÃO POLINOMIAL GRAU 2  #################

###########  ANÁLISE DE CORRELAÇÃO E REGRESSÃO  #################

x<- c(20, 25, 30, 35, 40, 50, 60, 65, 70, 75, 80, 90)  #tamanho do lote
y<- c(1.81, 1.7, 1.65, 1.55, 1.48, 1.4, 1.3, 1.26, 1.24, 1.21, 1.2, 1.18)   #custo da unidade


###########  DIAGRAMA DE DISPERSÃO  #################
###########  ANÁLISE DESCRITIVA VARIÁVEIS E DIAGRAMA DE DISPERSÃO  #################
par(mfrow=c(2,2))
boxplot(x, ylab = "tamanho do lote")
points(mean(x),col="red",pch=3)
mtext(" \n (a)" , side=1, line=4, cex=0.8)   #o valor de cex deve ser ajustado

boxplot(y, ylab = "custo da unidade")
points(mean(y),col="red",pch=3)
mtext(" \n (b)" , side=1, line=4, cex=0.8)   #o valor de cex deve ser ajustado

plot(y~x, xlab = "tamanho do lote", ylab = "custo da unidade", col = "blue") #diagrama de dispersão
mtext(" \n (c)" , side=1, line=4, cex=0.8)   #o valor de cex deve ser ajustado


###########  COEFICIENTE DE CORRELAÇÃO DE PEARSON  #################
pearson <- cor(y,x)     # coeficiente de Pearson
pearson


###########  COEFICIENTE DE CORRELAÇÃO DE SPEARMAN  #################
spearman <- cor(y, x, method = "spearman")  # coeficiente de Spearman
spearman



###########  ESTIMACAO DOS COEFICIENTES DE REGRESSAO  #################
x2=x*x;
ajuste_q1 <- lm(y~x+x2)   # ajuste da forma quadrática
summary(ajuste_q1)  #estimativas dos parâmetros beta0, beta1 e beta2

ajuste_q <- lm(y~poly(x,2))

###########  VERIFICAÇÃO DAS SUPOSIÇÕES DO MODELO DE REGRESSÃO LINEAR POLINOMIAL - GRAU 2  #################
preditos <- predict(ajuste_q); preditos  # valores preditos ou previstos
residuos <- resid(ajuste_q); residuos      # resíduos

# para obter os resíduos padronizados
anova(ajuste_q1)
estimativa_dp_res=sqrt(anova(ajuste_q1)$Mean[3]);  estimativa_dp_res   # estimativa do erro padrão dos erros = quadrado médio dos resíduos
res_padronizado=residuos/estimativa_dp_res; res_padronizado # resíduos padronizados

par(mfrow=c(2,2)) # criando um painel para 4 gráficos
boxplot(res_padronizado, ylab="resíduos padronizados") #diagrama de caixas dos resíduos padronizados
points(mean(res_padronizado),col="red",pch=3)

require(car)
qqPlot(res_padronizado, distribution="norm", envelope=.95, xlab="quantis da normal", ylab="resíduos padronizados") #gráfico de probabilidades normal com envelope dos resíduos padroninados

plot(preditos, res_padronizado, xlab="preditos", ylab="resíduos padronizados" ) # diagrama de dispersão preditos x residuos padronizados
abline(h=0)

shapiro.test(res_padronizado)  # teste de hipótese para verificar normalidade dos resíduos padronizados


###########  INTERVALOS DE CONFIANÇA PARA BETA0, BETA1 E BETA2 #################
require(MASS)
confint(ajuste_q1) # IC 95% de confiança para os parâmetros beta0, beta1 e beta2

#### ou (obtencao passo a passo)
# intervalos de confianca para beta0, beta1 e beta2
summary(ajuste_q1)

coef=summary(ajuste_q1)$coefficients[1,1] ; coef
err=summary(ajuste_q1)$coefficients[1,2] ; err
coef + c(-1,1)*err*qt(0.975, (length(y)-3))  # intervalos de confianca para beta0

coef=summary(ajuste_q1)$coefficients[2,1] ; coef
err=summary(ajuste_q1)$coefficients[2,2] ; err
coef + c(-1,1)*err*qt(0.975, (length(y)-3))  # intervalos de confianca para beta1

coef=summary(ajuste_q1)$coefficients[3,1] ; coef
err=summary(ajuste_q1)$coefficients[3,2] ; err
coef + c(-1,1)*err*qt(0.975, (length(y)-3)) # intervalos de confianca para beta1


###########  PREVISÃO PARA A MÉDIA E PARA UMA NOVA OBSERVAÇÃO E RESPECTIVOS INTERVALOS DE CONFIANÇA   #################

#criar um vetor com as novas observações - nesse caso foi criado um vetor contendo os valores 5, 6 e 7 comerciais
novos_x <- data.frame(x = seq(5, 7, 1)) ; novos_x
ajuste_q <- lm(y~poly(x,2))

int_conf_nova_obs <- predict(ajuste_q, novos_x, interval="prediction")  # intervalo de confiança 95% para novas obs (5 6 e 7 comerciais)
int_conf_nova_obs
int_conf_media <- predict(ajuste_q, novos_x, interval="confidence")  # intervalo de confiança 95% para as médias (5 6 e 7 comerciais)
int_conf_media

int_conf_nova_obs <- predict(ajuste_q, novos_x, level = 0.90, interval="prediction")  # intervalo de confiança 90% para novas obs (5 6 e 7 comerciais)
int_conf_nova_obs
int_conf_media <- predict(ajuste_q, novos_x, level = 0.90, interval="confidence")   # intervalo de confiança 90% para as médias (5 6 e 7 comerciais)
int_conf_media

#######  DIAGRAMA DE DISPERSÃO COM BANDAS DE CONFIANÇA PARA AS MÉDIAS E PARA NOVAS OBSERVAÇÕES ########

novos_x <- seq(min(x), max(x), length.out=100) ; novos_x

int_conf_nova_obs <- predict(ajuste_q, newdata = data.frame(x=novos_x), interval="prediction")  # intervalo de confiança 95% para novas obs (5 6 e 7 comerciais)
int_conf_nova_obs
int_conf_media <- predict(ajuste_q, newdata = data.frame(x=novos_x), interval="confidence")  # intervalo de confiança 95% para as médias (5 6 e 7 comerciais)
int_conf_media

# gráfico
par(mfrow=c(1,1))
plot(y ~ x, type = 'n', xlab="tamanho do lote", ylab="custo da unidade" )
polygon(c(rev(novos_x), novos_x), c(rev(int_conf_nova_obs[ ,3]), int_conf_nova_obs[ ,2]), col = 'grey95',  border = NA)   #acrescentar bandas para novas observações
polygon(c(rev(novos_x), novos_x), c(rev(int_conf_media[ ,3]), int_conf_media[ ,2]), col = 'grey90',  border = NA) #acrescentar bandas para as médias
points(x, y, type = 'p')
# acrescenta curva
lines(novos_x, int_conf_nova_obs[ ,1], col="blue")
lines(novos_x, int_conf_nova_obs[ ,2], col="blue", lty = 5)
lines(novos_x, int_conf_nova_obs[ ,3], col="blue", lty = 5)  #lty = 'dashed'
#lines(novos_x, int_conf_media[ ,1], col="blue")
lines(novos_x, int_conf_media[ ,2], col="blue", lty = 'dashed')
lines(novos_x, int_conf_media[ ,3], col="blue", lty = 'dashed')  #lty = 'dashed'



# para comparar as formas de ajustar o polinomio de grau 2
ajuste_q <- lm(y~poly(x,2))
anova(ajuste_q)
resid(ajuste_q)
coef(ajuste_q)
confint(ajuste_q) # IC 95% de confiança para os parâmetros beta0, beta1 e beta2
predict(ajuste_q)

ajuste_q1 <- lm(y~x+x2)   # ajuste da forma quadrática
anova(ajuste_q1)
summary(ajuste_q1)
resid(ajuste_q1)
coef(ajuste_q1)
confint(ajuste_q1) # IC 95% de confiança para os parâmetros beta0, beta1 e beta2
predict(ajuste_q1)

summary(ajuste_q1)
y10=summary(ajuste_q1)$coefficients[1,1]+summary(ajuste_q1)$coefficients[2,1]*5+summary(ajuste_q1)$coefficients[3,1]*25 ; y10

summary(ajuste_q1)$coefficients[1,1]
summary(ajuste_q1)$coefficients[2,1]
summary(ajuste_q1)$coefficients[3,1]

