data<-read.table("C:/Users/YODA ISMAEL/Desktop/Dossier Etudes/Dossiers Master/Semestre2/Régression linéaire/Devoir/Devoir_UT/dataR.txt",header = T,sep=" ",dec=".")
data

###  Calculer la matrice de corrélations des variables explicatives et créer une matrice
#### 5 × 5 dont le terme d'indice (i, j) est la p-valeur associée au test de nullité du
###  coefficient de corrélation (de Pearson) entre Xi
###  et Xj. Doit-on craindre un problème de multicolinéarité ?

## matrice de corrélations des variables explicatives

X1<-data$X1
X2<-data$X2
X3<-data$X3
X4<-data$X4
X5<-data$X5
V_exp<-cbind.data.frame(X1,X2,X3,X4,X5)
V_exp

### Matrice de corrélation des variables explicatives
mcor<-cor(V_exp)
mcor

## Matrice 5 × 5 dont le terme d'indice (i, j) est la p-valeur associée au test de nullité du
### coefficient de corrélation (de Pearson) entre Xi et Xj

library(Hmisc)
rcorr(as.matrix(V_exp[,1:5]),type = c("pearson"))

### Au vu de la matrice des p-value, on constate que pour la grande
### majorité des couples de valeurs(Xi et Xj), L'hypothese nulle de 
### nullité du coefficient de corrélation est rejettée sauf pour 
### le couple (X4,X5). On doit donc craindre un probleme de multicolinéarité
### entre tous les couple de variables sauf le couple (X4,X5).

### 2. En faisant une sélection de variables avec le critère BIC, quelles variables faudrait-il
### conserver ?

library(MASS)
reg=lm(Y ~. , data=data)
par(mfrow=c(1,1))
selected  = stepAIC(reg, direction="backward", k= log(nrow(data)))
selected$anova


#library(leaps)
#m <-regsubsets(Y ~ X1 + X2 + X3 + X4 + X5, data, method = "forward")
#reg.summary <- summary(m)
#min.bic <- which.min(reg.summary$bic)
#points(min.bic, reg.summary$bic[min.bic], col ="red", cex = 2, pch = 20)
#coef(m,4)


### En utilisant la méthode pas à pas, nous obtenons que le modèle
### qui minimise le critère BIC est le modèle prénant en compte 4 
### variables explicatives dont X1,X2,X3 et X5.

## Représenter Y en fonction des valeurs prédites par le modèle.
##Représenter les résidus studentisés. Que remarque-t-on ?

# Les valeurs prédites par le modèle :

prev<-predict(reg)
prev=c(prev)

X11()
ggplot(data, aes(x = prev , y = Y)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")


res_student<-rstandard(reg)
plot(res_student)
abline(h=2,col=2)
abline(h=-2,col=2)

### On remarque l'excistence de valeurs atipiques dans nos
### données. ces valeurs sont celles dont
### la valeur absolue des résidus studentisés est superieur
### à deux. Ces valeurs sont hors de la zone délimiter par notre
### graphique

## 4. Les valeurs éventuelles à supprimers sont:
res_student[abs(res_student) > 2]

## Retirer l'observation ayant une influence trop grande, et rechercher le meilleur
## modèle. Comment le R2 a-t-il évolué ?

## La valeur ayant le plus d'influence est la 160e valeur
## Rétirons là de la base.

data1 = data[ - c(160), ]
data1
attach(data1)

reg1=lm(Y ~. , data=data1)
par(mfrow=c(1,1))
selected  = stepAIC(reg1, direction="backward", k= log(nrow(data)))
selected$anova

## Après l'extraction du 160e individu de la base, on obtient
## le modèle final Y ~ X1 + X2 + X3

## Comaparaison des R_carré des deux modèles
modele1<-lm(Y ~ X1 + X2 + X3 + X5,data=data)
modele2<-lm(Y ~ X1 + X2 + X3,data=data1)
summary(modele1)
summary(modele2)

## Du modele1 au modele2 On constate qu'il y'a une évolution
## du R_carré. En effet le R_carré passe de 0.9237 à 0.941.


