library(MASS)
library(ISLR)
library(nlme)
library(lme4)
library(ggplot2)
library(babynames)
library(dplyr)
library(hrbrthemes)
library(viridis)

attach(parkinsons_updrs)
names(parkinsons_updrs)
m1 <- parkinsons_updrs 
summary(m1)
dim(m1) #fornece a dimens?o das linhas e colunas do dataset
# motor_UPDRS & total_UPDRS <- Vari?veis dependentes (de interesse)

###############  AN?LISE EXPLORAT?RIA  ##############

#Correlação entre variáveis
cor(parkinsons_updrs[,])

hist(total_UPDRS) #Distribui??o gaussiana (normal) 
hist(motor_UPDRS) #N?o aparenta ser uma distribui??o gaussiana. 
#Neste caso a vari?vel de interesse a ser estudada ser? a vari?vel total_UPDRS = Y

#Verificar se tem missing values
table(is.na(m1)) #(neste caso n?o apresenta qualquer valor em falta)
prop.table(table(is.na(m1))) * 100 #calcula a proporção de missing values
which(is.na(m1)==T) 

#Eliminar os registos com test_time inferior a zero
m1 = m1[m1$test_time > 0,]
dim(m2)

#Verificar se tem valores duplicados
duplicated(m1)

######################### RESPONDER ?S PERGUNTAS: ############################################# 

###Existe algum res?duo (outlier) presente nas observa??es? Se sim, que medidas devemos tomar para 
#o tratamento desse res?duo.?

#Verificar os outliers
boxplot(total_UPDRS, main="Total UPDRS", sub=paste("Outlier rows: ", boxplot.stats(total_UPDRS)$out))
boxplot(motor_UPDRS, main="Motor UPDRS", sub=paste("Outlier rows: ", boxplot.stats(motor_UPDRS)$out))
boxplot(age, main="Age", sub=paste("Outlier rows: ", boxplot.stats(age)$out))
boxplot(subject., main="subject.", sub=paste("Outlier rows: ", boxplot.stats(subject.)$out))
boxplot(Jitter..., main="Jitter...", sub=paste("Outlier rows: ", boxplot.stats(Jitter...)$out))
boxplot(Jitter.Abs., main="Jitter.Abs.", sub=paste("Outlier rows: ", boxplot.stats(Jitter.Abs.)$out))
boxplot(Jitter.RAP, main="Jitter.RAP", sub=paste("Outlier rows: ", boxplot.stats(Jitter.RAP)$out))
boxplot(Jitter.PPQ5, main="Jitter.PPQ5", sub=paste("Outlier rows: ", boxplot.stats(Jitter.PPQ5)$out))
boxplot(Jitter.DDP, main="Jitter.DDP", sub=paste("Outlier rows: ", boxplot.stats(Jitter.DDP)$out))
boxplot(Shimmer, main="Shimmer", sub=paste("Outlier rows: ", boxplot.stats(Shimmer)$out))
boxplot(Shimmer.dB., main="Shimmer.dB.", sub=paste("Outlier rows: ", boxplot.stats(Shimmer.dB.)$out))
boxplot(Shimmer.APQ3, main="Shimmer.APQ3", sub=paste("Outlier rows: ", boxplot.stats(Shimmer.APQ3)$out))
boxplot(Shimmer.APQ5, main="Shimmer.APQ5", sub=paste("Outlier rows: ", boxplot.stats(Shimmer.APQ5)$out))
boxplot(Shimmer.APQ11, main="Shimmer.APQ11", sub=paste("Outlier rows: ", boxplot.stats(Shimmer.APQ11)$out))
boxplot(Shimmer.DDA, main="Shimmer.DDA", sub=paste("Outlier rows: ", boxplot.stats(Shimmer.DDA)$out))
boxplot(NHR, main="NHR", sub=paste("Outlier rows: ", boxplot.stats(NHR)$out))
boxplot(HNR, main="HNR", sub=paste("Outlier rows: ", boxplot.stats(HNR)$out))
boxplot(RPDE, main="RPDE", sub=paste("Outlier rows: ", boxplot.stats(RPDE)$out))
boxplot(DFA, main="DFA", sub=paste("Outlier rows: ", boxplot.stats(DFA)$out))
boxplot(PPE, main="PPE", sub=paste("Outlier rows: ", boxplot.stats(PPE)$out))

###De que forma a progress?o do test_time influ?ncia o total_updrs?

boxplot(total_UPDRS ~ test_time)

xlim = range(test_time)
ylim = range(total_UPDRS)
plot(total_UPDRS ~ test_time, xlab = "test_time", ylab = "total_UPDRS", xlim= xlim, ylim = ylim)

###Qual o atributo ou atributos que mais influenciam a vari?vel total_updrs?

cor.test(total_UPDRS, subject., method=c("pearson", "kendall", "spearman"))       #0.2536427
cor.test(total_UPDRS, age, method=c("pearson", "kendall", "spearman"))            #0.3102899   ***
cor.test(total_UPDRS, sex, method=c("pearson", "kendall", "spearman"))            #-0.09655888
cor.test(total_UPDRS, Jitter..., method=c("pearson", "kendall", "spearman"))      #0.07424667
cor.test(total_UPDRS, Jitter.Abs., method=c("pearson", "kendall", "spearman"))    #0.06692673 
cor.test(total_UPDRS, Jitter.RAP, method=c("pearson", "kendall", "spearman"))     #0.06401542
cor.test(total_UPDRS, Jitter.PPQ5, method=c("pearson", "kendall", "spearman"))    #0.06335178
cor.test(total_UPDRS, Jitter.DDP, method=c("pearson", "kendall", "spearman"))     #0.06402746
cor.test(total_UPDRS, Shimmer, method=c("pearson", "kendall", "spearman"))        #0.09214091
cor.test(total_UPDRS, Shimmer.dB., method=c("pearson", "kendall", "spearman"))    #0.09878973 
cor.test(total_UPDRS, Shimmer.APQ3, method=c("pearson", "kendall", "spearman"))   #0.07936272 
cor.test(total_UPDRS, Shimmer.APQ5, method=c("pearson", "kendall", "spearman"))   #0.08346725 
cor.test(total_UPDRS, Shimmer.APQ11, method=c("pearson", "kendall", "spearman"))  #0.1208375
cor.test(total_UPDRS, Shimmer.DDA, method=c("pearson", "kendall", "spearman"))    #0.07936324 
cor.test(total_UPDRS, NHR, method=c("pearson", "kendall", "spearman"))            #0.06095164 
cor.test(total_UPDRS, HNR, method=c("pearson", "kendall", "spearman"))            #-0.1621168   ***
cor.test(total_UPDRS, RPDE, method=c("pearson", "kendall", "spearman"))           #0.1568965 
cor.test(total_UPDRS, DFA, method=c("pearson", "kendall", "spearman"))            #-0.1134748 
cor.test(total_UPDRS, PPE, method=c("pearson", "kendall", "spearman"))            #0.1561949
cor.test(total_UPDRS, motor_UPDRS, method=c("pearson", "kendall", "spearman"))    #0.9472       ***


###Em m?dia os homens t?m uma tend?ncia a ter total_updrs maior que as mulheres?

par(mfrow=c(1,1))
total_UPDRS.males=total_UPDRS[which(sex=="0")]
total_UPDRS.females=total_UPDRS[which(sex=="1")]

hist(total_UPDRS.males, col="blue")
hist(total_UPDRS.females, add=T)

#Normalized Data
#normalized = (sex-min(sex))/(max(sex)-min(sex))
#hist(normalized, breaks=10, xlab="Normalized Data", col="lightblue", main="")

###Quais s?o as caracter?sticas mais comuns de um individuo que possui um total_updrs 
#superior a 50 UPDRS (Escala Unificada de Avalia??o da Doen?a de Parkinson)?

total_UPDRS.50 <- m1[which(total_UPDRS>= 50 ), ]
fix(total_UPDRS.50 )
hist(total_UPDRS.50$age)          #[70, 72]
hist(total_UPDRS.50$sex)          #masculino
hist(total_UPDRS.50$Jitter...)    #[0.00, 0.01]
hist(total_UPDRS.50$Jitter.Abs.)  #[0e+00, 0.5e-02]
hist(total_UPDRS.50$Jitter.RAP)   #[0.00, 0.005]
hist(total_UPDRS.50$Shimmer)      #[0.02, -0.05]
hist(total_UPDRS.50$Shimmer.APQ5) #[0.00, 0.04]
hist(total_UPDRS.50$Shimmer.APQ11)#[0.02, 0.04]
hist(total_UPDRS.50$HNR)          #[20, 22]
hist(total_UPDRS.50$RPDE)         #[0.45, 0.5]
hist(total_UPDRS.50$DFA)          #[0.72, 0.74]
hist(total_UPDRS.50$PPE)          #[0.2, 0.3]
hist(total_UPDRS.50$motor_UPDRS)  #[36, 38]

###############  AN?LISE PREDITIVA  ##############

#Regress?o com todos os atributos sem dataset de treino e teste
#adicionamos age*sex pois achamos ser um bom termo de intera??o 

x1 <- lm(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
                    + Jitter.Abs. + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer 
                    + Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA 
                    + NHR + HNR + RPDE + DFA + PPE + age*sex, data=m1)
summary(x1)

#Realizamos o step para a regress?o linear sem o dataset de treino e teste para agilizar processo, por?m realizamos ? m?o 
#todo o processo para o dataset com treino e teste, que ser? realizado posteriormente. 

#Podemos concluir qual o melhor modelo atrav?s do step

step(x1 , direction = "backward")
#Resultado:
x6 <- lm(formula = total_UPDRS ~ subject. + age + sex + test_time + 
           motor_UPDRS + Jitter... + Jitter.Abs. + Jitter.RAP + Shimmer + 
           Shimmer.APQ5 + Shimmer.APQ11 + HNR + RPDE + DFA + PPE + age:sex, 
         data = m1)
summary(x6)

extractAIC(x1)
extractAIC(x6)

#Calcular os coeficientes com 95% de certeza
confint(x1,level=0.95)
confint(x6,level=0.95)
#Podemos concluir que em nenhum dos casos, como o zero n?o est? condito com 95% de certeza em nenhum dos intervalos das
#vari?veis, n?o ser? necess?rio eliminar nenhum preditor, logo s?o bons preditores.

m2 <- m1[,-c(10,11,13,14,17)]


#################Regress?o com todos os atributos com dataset de treino e teste######################

#Criar dados de treino e teste
## 75% of the sample size
smp_size <- floor(0.75 * nrow(m1))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(m1)), size = smp_size)

train <- m1[train_ind, ]
test <- m1[-train_ind, ]

dim(train)
dim(test)
dim(m1)

#modelo normal + Rela??o entre age*sex
y1 <- lm(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
                    + Jitter.Abs. + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer 
                    + Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA 
                    + NHR + HNR + RPDE + DFA + PPE + age*sex, data=train)
summary(y1)

#Remover Shimmer.DDA
y2 <- lm(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
                    + Jitter.Abs. + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer 
                    + Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11  
                    + NHR + HNR + RPDE + DFA + PPE + age*sex, data=train)
summary(y2)

#Remover Jitter.DDP
y3 <- lm(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
                    + Jitter.Abs. + Jitter.RAP + Jitter.PPQ5  + Shimmer 
                    + Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11  
                    + NHR + HNR + RPDE + DFA + PPE + age*sex, data=train)
summary(y3)

#Remover Shimmer.APQ3
y4 <- lm(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
                    + Jitter.Abs. + Jitter.RAP + Jitter.PPQ5  + Shimmer 
                    + Shimmer.dB.  + Shimmer.APQ5 + Shimmer.APQ11  
                    + NHR + HNR + RPDE + DFA + PPE + age*sex, data=train)
summary(y4)

#Remover Jitter.PPQ5
y5 <- lm(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
                    + Jitter.Abs. + Jitter.RAP  + Shimmer 
                    + Shimmer.dB.  + Shimmer.APQ5 + Shimmer.APQ11  
                    + NHR + HNR + RPDE + DFA + PPE + age*sex, data=train)
summary(y5)

#Remover Shimmer.dB.
y6 <- lm(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
                    + Jitter.Abs. + Jitter.RAP  + Shimmer 
                    + Shimmer.APQ5 + Shimmer.APQ11  
                    + NHR + HNR + RPDE + DFA + PPE + age*sex, data=train)
summary(y6)

extractAIC(y1)
extractAIC(y2)
extractAIC(y3)
extractAIC(y4)
extractAIC(y5)
extractAIC(y6) # este ? o melhor

#Se quisermos fazer este exerc?cio autom?ticamente fazemos:

step(y1 , direction = "backward")

#Calcular os coeficientes com 95% de certeza
confint(y6,level=0.95)
#Podemos concluir que em nenhum dos casos, como o zero n?o est? condito com 95% de certeza em nenhum dos intervalos das
#vari?veis, n?o ser? necess?rio eliminar nenhum preditor, logo s?o bons preditores.

anova(y1,y6)

#?contrasts
#contrasts(sex)

################################ AN?LISE PREDITIVA E EXPLORA??O DE RESULTADOS ##############################################################

 # SEM DATASET DE TREINO E TESTE# 
prediction <- predict(x6)
length(prediction)
fix(prediction)

 # COM DATASET DE TREINO E TESTE # 
prediction_test = predict(y6, test) 
length(prediction_test)
fix(prediction_test)

 # VISUALIZA??O DE GR?FICOS #
par(mfrow=c(1,2))
hist(prediction, main="Predicted total_UPDRS")
hist(total_UPDRS, main="Real total_UPDRS")

hist(prediction_test,main="Predicted with test total_UPDRS")
hist(total_UPDRS, main="Real total_UPDRS")

par(mfrow=c(1,2))
xlim = range(total_UPDRS)
plot(prediction ~ m1$total_UPDRS, xlab = "Observados", ylab = "Previstos", xlim= xlim, ylim = xlim)
abline(0,1,col="red")
#teste
plot(prediction_test ~ test$total_UPDRS, xlab = "Observados", ylab = "Previstos", xlim= xlim, ylim = xlim)
abline(0,1,col="red")


# C?LCULO DA RAIZ QUADRADA M?DIA DO ERRO -  RMSE (ROOT MEAN SQUARE ERROR)

#rmse_1 = function(total_UPDRS, prediction){ sqrt(mean((total_UPDRS - prediction)^2))}
rmse = function(total_UPDRS, prediction_test){sqrt(mean((total_UPDRS - prediction_test)^2))}


rmse(test$total_UPDRS, prediction_test)

rmse(m1$total_UPDRS, prediction)



##----- MODELOS MISTOS (efeito fixo + efeito aleat?rio) -----##

model1 = lmer(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
              + Jitter.Abs. + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer 
              + Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA 
              + NHR + HNR + RPDE + DFA + PPE + (1|subject.) + (1|test_time), data=train, REML=FALSE)
summary(model1)


model2 = lmer(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
              + Jitter.Abs. + Jitter.RAP  + Shimmer 
              + Shimmer.APQ5 + Shimmer.APQ11  
              + NHR + HNR + RPDE + DFA + PPE + (1|subject.) + (1|test_time), data=train, REML = FALSE)



anova(model1,model2)


predictions_model1 = predict(model1, test, allow.new.levels = TRUE)
length(predictions_model1)

predictions_model2 = predict(model2, test, allow.new.levels = TRUE)
length(predictions_model2)

#Avalia??o dos modelos mistos

rmse(test$total_UPDRS, predictions_model1)
rmse(test$total_UPDRS, predictions_model2)

extractAIC(model1)
extractAIC(model2)


#Modelo Misto sem Dados de Treino e Teste

model3 <- lmer(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + Jitter... 
               + Jitter.Abs. + Jitter.RAP  + Shimmer 
               + Shimmer.APQ5 + Shimmer.APQ11  
               + NHR + HNR + RPDE + DFA + PPE + (1|subject.) + (1|test_time), data=m1, REML = FALSE)
extractAIC(model3)

predictions_model3 = predict(model3, m1, allow.new.levels = TRUE)
length(predictions_model3)
rmse(m1$total_UPDRS, predictions_model3)

