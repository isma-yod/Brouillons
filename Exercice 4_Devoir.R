data<-read.table("C:/Users/YODA ISMAEL/Desktop/Dossier Etudes/Dossiers Master/Semestre2/R�gression lin�aire/Devoir/Devoir_UT/dataR.txt",header = T,sep=" ",dec=".")
data

###  Calculer la matrice de corr�lations des variables explicatives et cr�er une matrice
#### 5 � 5 dont le terme d'indice (i, j) est la p-valeur associ�e au test de nullit� du
###  coefficient de corr�lation (de Pearson) entre Xi
###  et Xj. Doit-on craindre un probl�me de multicolin�arit� ?

## matrice de corr�lations des variables explicatives

X1<-data$X1
X2<-data$X2
X3<-data$X3
X4<-data$X4
X5<-data$X5
V_exp<-cbind.data.frame(X1,X2,X3,X4,X5)
V_exp

### Matrice de corr�lation des variables explicatives
mcor<-cor(V_exp)
mcor

## Matrice 5 � 5 dont le terme d'indice (i, j) est la p-valeur associ�e au test de nullit� du
### coefficient de corr�lation (de Pearson) entre Xi et Xj

library(Hmisc)
rcorr(as.matrix(V_exp[,1:5]),type = c("pearson"))

### Au vu de la matrice des p-value, on constate que pour la grande
### majorit� des couples de valeurs(Xi et Xj), L'hypothese nulle de 
### nullit� du coefficient de corr�lation est rejett�e sauf pour 
### le couple (X4,X5). On doit donc craindre un probleme de multicolin�arit�
### entre tous les couple de variables sauf le couple (X4,X5).

### 2. En faisant une s�lection de variables avec le crit�re BIC, quelles variables faudrait-il
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


### En utilisant la m�thode pas � pas, nous obtenons que le mod�le
### qui minimise le crit�re BIC est le mod�le pr�nant en compte 4 
### variables explicatives dont X1,X2,X3 et X5.

## Repr�senter Y en fonction des valeurs pr�dites par le mod�le.
##Repr�senter les r�sidus studentis�s. Que remarque-t-on ?

# Les valeurs pr�dites par le mod�le :

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
### donn�es. ces valeurs sont celles dont
### la valeur absolue des r�sidus studentis�s est superieur
### � deux. Ces valeurs sont hors de la zone d�limiter par notre
### graphique

## 4. Les valeurs �ventuelles � supprimers sont:
res_student[abs(res_student) > 2]

## Retirer l'observation ayant une influence trop grande, et rechercher le meilleur
## mod�le. Comment le R2 a-t-il �volu� ?

## La valeur ayant le plus d'influence est la 160e valeur
## R�tirons l� de la base.

data1 = data[ - c(160), ]
data1
attach(data1)

reg1=lm(Y ~. , data=data1)
par(mfrow=c(1,1))
selected  = stepAIC(reg1, direction="backward", k= log(nrow(data)))
selected$anova

## Apr�s l'extraction du 160e individu de la base, on obtient
## le mod�le final Y ~ X1 + X2 + X3

## Comaparaison des R_carr� des deux mod�les
modele1<-lm(Y ~ X1 + X2 + X3 + X5,data=data)
modele2<-lm(Y ~ X1 + X2 + X3,data=data1)
summary(modele1)
summary(modele2)

## Du modele1 au modele2 On constate qu'il y'a une �volution
## du R_carr�. En effet le R_carr� passe de 0.9237 � 0.941.


