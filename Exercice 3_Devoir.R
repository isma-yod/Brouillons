data<-read.table("C:/Users/YODA ISMAEL/Desktop/Dossier Etudes/Dossiers Master/Semestre2/Régression linéaire/Devoir/Devoir_UT/data.txt",header = T,sep=" ",dec=".")
data

## 1. Créer le vecteur Y contenant la variable que nous voulons modéliser et 
## la matrice X contenant les 4 variables explicatives X1, . . . , X 4. 
## Représenter Y en fonction de chacune des autres variables : observe-t-on
## des liens linéaires ?

X1<-data$X1
X2<-data$X2
X3<-data$X3
X4<-data$X4
X<-cbind(X1,X2,X3,X4)
X
Y<-data$Y

?rmarkdown

### Réprésentation graphique de Y en fonction de chacune des variables
### exogènes
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(rmarkdown)

par(mfrow=c(1,2))

ggplot(data, aes(x = X1 , y = Y)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

ggplot(data, aes(x = X2 , y = Y)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

ggplot(data, aes(x = X3 , y = Y)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

ggplot(data, aes(x = X4 , y = Y)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

cor(data$X1,data$Y)
cor(data$X2,data$Y)
cor(data$X3,data$Y)
cor(data$X4,data$Y)
#### On observe pas de lien entre les variables explicatives, (X1,X3,X4)
#### Et la variable à expliquer Y car le lien entre ces variables 
#### explicative et la variable dépendante ne semble pas etre linéaire.
#### Par contre il semble exister une relation linéaire entre X2 et Y.


#### 2.Réaliser la régression linéaire de Y sur l'ensemble des variables
#### : que vaut le R2 ? Certains coefficients sont-ils non significatifs? 
####   La régression paraît-elle acceptable ?

model<-lm(Y~X1+X2+X3+X4,data=data)
summary(model)

#### Le R_carré du modèle vaut 0,7972. Les coefficients X1,X2 et X4
#### sont significatifs au seuil de 1% et la variable X3 est 
### significative au seuil de 5%. La régréssion parrait acceptable
### car le R_carré est proche de 1 et les paramètres estimées sont tous
### significatifs.

### 3.Pour i allant de 1 à 4, réaliser la régression de Y sur les variables
### {Xj: j =1, . . . , 4, j # i} et représenter les résidus en fonction 
### de Xi.Des liens linéaires plus nets apparaissent-ils ? 
### Voit-on d'autres liens ?


### Régréssion entre Y et X1 et représentation des résidus en fonction de X1
model1<-lm(Y~X1,data=data)
summary(model1)

ggplot(data, aes(x = X1 , y = model1$residuals)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")


model2<-lm(Y~X2,data=data)
summary(model2)

ggplot(data, aes(x = X2 , y = model2$residuals)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

model3<-lm(Y~X3,data=data)
summary(model3)

ggplot(data, aes(x = X3 , y = model3$residuals)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

model4<-lm(Y~X4,data=data)
summary(model4)

ggplot(data,aes(x = data$X4 , y = model4$residuals)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

##### Nous ne constatons pas l'apparition de liens linéaires plus net

### 4.

X1<-data$X1
X2<-data$X2
X3<-data$X3
lnX4<-log(data$X4)
Z<-cbind(X1,X2,X3,lnX4)

second_model_1<-lm(Y~X1+X2+X3+lnX4)
summary(second_model_1)

#### Ce second modèle a un meilleure R_carré que le premier par contre,
#### le paramètre X3 qui était significatif au premier modèle ne l'est
#### plus au second modèle.

#### Retrais de la variable X3 dans le modèle

second_model_2<-lm(Y~X1+X2+lnX4)
summary(second_model_2)

#### D'après les sorties, le modèle sans X3 est préferable car le
#### le retrait de la variable non significative (X3) ne change rien
#### au modèle. Le R_carrée ajustée notamment ne change pas.


### 5.

library(MASS)
X<-cbind(X1,X2,X3,X4,lnX4)
X
data<-as.data.frame(X)
data

modele=lm(Y~., data=data)
summary(modele)

meilleur_modele=stepAIC(modele,~.,trace=TRUE,
                    direction=c("backward"))
summary(modselect_b)

#### Le meilleure modèle est le modèle Y ~ X1 + X2 + lnX4

######  6. On décide de conserver le modèle Y = X0 + X1 +X2
######  +ln(X4) 

###Testons la normalité des résidus.

### Nous allons utiliser le test de Shapiro-Wilk

modele_choisi<-lm(Y~X1+X2+X3+lnX4,data=data)
residus<-modele_choisi$residuals
residus
shapiro.test(residus)

#### Hypothèse nulle : L'échantillon suit une loi normale
#### La p-value de notre test est superieure a 5% donc on ne peut pas
#### rejetter l'hypothèse nulle. Les résidus du modèle suivent une loi
#### normale

### 7. Testons l'hypothèse d'homoscédasticité.
### Nous allons utilisé le test d'homoscédasticité de Breusch-Pagan
### H0 : homoscédasticité des résidus
library(lmtest)
bptest(modele_choisi)
?bptest

#### Verifation graphique en répresentant le nuage de point y_chapeau
#### avec les residus standardisées.


a=rstandard(modele_choisi) ### Residus standardisées 
b=predict(modele_choisi)   ### Valeurs prédites
plot(a,b)

ggplot(data,aes(x = a , y = b)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

##### Les résidus sont repartient uniformement de part et d'autre 
####  de l'axe des abscisses. On peut donc dire que la variance est
#### constante.

a<-seq(1:40)
a
d<-rep(1,100)
d
rep(1,100)
X<-cbind(X1,X2,X3,lnX4)
X
A<-solve(t(X)%*%X)
A

A1<-sqrt(A[1,1])
A1
tb<-qt(0.975,495)
tb
modele_choisi$coefficients

sigma_chapeau<-sqrt(sum(modele_choisi$residuals^2)/495)
sigma_chapeau
Intervalle_B1<-c(2.01242270-qt(0.975,495)*sqrt(sum(modele_choisi$residuals^2)/495)*sqrt(A[1,1]),
                 2.01242270+qt(0.975,495)*sqrt(sum(modele_choisi$residuals^2)/495)*sqrt(A[1,1]))
Intervalle_B1

2.01242270-tb*sigma_chapeau*A1

2.01242270-qt(0.975,495)*sqrt(sum(modele_choisi$residuals^2)/495)*A1
2.01242270+qt(0.975,495)*sqrt(sum(modele_choisi$residuals^2)/495)*A1


3.03279-qt(0.975,495)*sqrt(sum(modele_choisi$residuals^2)/495)*sqrt(A[1,1])

## Question 9

predict(modele_choisi,data.frame(X1=200,X2=200,X3=200,X4=200,lnX4=log(200)))    

x=rbind(1,200,200,200,log(200))
x
betas=rbind(50.92216595,2.01242270,3.03279289,-0.01730588,298.97541608)
sigma_chapeau<-sqrt(sum(modele_choisi$residuals^2)/495)
sigma_chapeau


X0<-cbind(rep(1,100),X1,X2,X3,lnX4)

t(x)%*%betas-qt(0.975,495)*sigma_chapeau*
  sqrt(1+t(x)%*%solve(t(X0)%*%X0)%*%x)

t(x)%*%betas+qt(0.975,495)*sigma_chapeau*
  sqrt(1+t(x)%*%solve(t(X0)%*%X0)%*%x)


t(x)%*%solve(t(X)%*%X)
solve(t(X)%*%X)

predict(modele_choisi,data.frame(X0=1,X1=200,X2=200,X3=200,X4=200,lnX4=log(200)),
        interval="confidence")                   
                   