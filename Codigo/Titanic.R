library(Hmisc)
library(knitr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(gridExtra)
library(ROCR)
library(corrplot)

### Incluir Datasources
Full_Source1 <- read.csv("./Tipología y ciclo de vida de los datos/Practica2/train.csv", stringsAsFactors = F, na.strings = c("NA", ""))
Part1_Source2 <- read.csv("./Tipología y ciclo de vida de los datos/Practica2/test.csv", stringsAsFactors = F, na.strings = c("NA", ""))
Part2_Source2 <- read.csv("./Tipología y ciclo de vida de los datos/Practica2/gender_submission.csv", stringsAsFactors = F, na.strings = c("NA", ""))

### Join de Part1_Source2 (test) & Part2_Source2 (gender_submission)
### para obtener una unica tabla con los mismos campos que Full_Source1 (train).
Full_Source2 <- merge(Part1_Source2, Part2_Source2)
### Union de las tablas Full_Source1 (train) y Full_Source2
titanic <- rbind(Full_Source1, Full_Source2)

### Informacion de la tabla resultante
str(titanic)
summary(titanic)

### NA´s Presentes en cada columas
sapply(titanic, function(x) {sum(is.na(x))})

### conversion a Factor
titanic$Sex <- as.factor(titanic$Sex)
titanic$Pclass <- as.factor(titanic$Pclass)

###Extraer el Título de Nombre y Apellidos
titanic$Surname <- sapply(titanic$Name, function(x) {strsplit(x, split='[,.]')[[1]][1]})
titanic$Surname <- sapply(titanic$Surname, function(x) {strsplit(x, split='[-]')[[1]][1]})
titanic$TitleName <- sapply(titanic$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
titanic$TitleName <- sub(' ', '', titanic$TitleName)
kable(table(titanic$Sex, titanic$TitleName))

###Homogenizar la variable Título
titanic$Title[titanic$TitleName %in% c("Mlle", "Ms","Mme","Mrs",'Master', 'Miss', 'Mr','Don', 'Dona')] <- 0
titanic$Title[!(titanic$TitleName %in% c("Mlle", "Ms","Mme","Mrs",'Master', 'Miss', 'Mr','Don', 'Dona'))] <-1
titanic$Title <- as.factor(titanic$Title)
kable(table(titanic$Sex, titanic$Title))
 
### Informacion de la tabla resultante
summary(titanic)

### Backup de tabla ppal
Titanic_bk1 <- titanic




### Numero de Personas por tiket
TiketsRep<-data.frame(table(titanic$Ticket))
colnames(TiketsRep)[1] <- "Ticket" ### Renombrar columna para facilitar merge
titanic <- merge(titanic, TiketsRep)

### tarifa por persona
titanic$FarePP <- titanic$Fare/titanic$Freq 

### Tabla con la tarifa media por clase y puerto de embarque / se escluyen terifas 0
Fares_Table <- aggregate(FarePP~Pclass+Embarked, data=titanic, FUN=(function(x)
                  {ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))
kable(Fares_Table)


###Pasajeros sin puerto de embarque
kable(titanic[which(is.na(titanic$Embarked)), 
  c( 'Name', 'Survived', 'Title','Pclass', 'Age', 'Sex', 'SibSp', 'Parch', 'Ticket', 'FarePP', 'Embarked')])
###Pasajeros sin Tarifa
kable(titanic[which(is.na(titanic$Fare)), 
  c( 'Name', 'Survived', 'Title','Pclass', 'Age', 'Sex', 'SibSp', 'Parch', 'Ticket', 'FarePP', 'Embarked')])

###Imputacion Pasajeros sin puerto de embarque
titanic$Embarked[titanic$Ticket=='113572'] <- 'C'
### Imputacion al Pasajeros sin Tarifa
titanic$FarePP[titanic$Ticket=='3701'] <- 7.43

### conversion a Factor
titanic$Family <- as.factor(titanic$SibSp + titanic$Parch) ## Se agregan familiares
titanic$Embarked <- as.factor(titanic$Embarked)
### Seleccion de columnas
TitanicPred <- subset(titanic, select = c('Survived', 'Title','Pclass', 'Sex', 'Family', 'FarePP', 'Embarked'))
### Informacion de la tabla resultante
summary (TitanicPred)

library(nortest)
### Comprobación de la normalidad
alpha = 0.05
col.names = colnames(TitanicPred)
for (i in 1:ncol(TitanicPred)) {
  if (is.integer(TitanicPred[,i]) | is.numeric(TitanicPred[,i])) {
    p_val = ad.test(TitanicPred[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      cat(" NO sigue una distribución normal\n")
    }else{
      cat(col.names[i])
      cat(" SI sigue una distribución normal\n")
    }
  }
}
### Comprobación de la homogeneidad de varianzas:
fligner.test(Survived ~ Title, data = TitanicPred)
fligner.test(Survived ~ Pclass, data = TitanicPred)
fligner.test(Survived ~ Sex, data = TitanicPred)
fligner.test(Survived ~ Family, data = TitanicPred)
fligner.test(Survived ~ Embarked, data = TitanicPred)


### Coeficiente de correlación para cada variable cuantitativa
TitanicPred$Survived <- as.factor(TitanicPred$Survived)
TitanicPred$TitleNum <- as.numeric(TitanicPred$Title)
TitanicPred$PclassNum <- as.numeric(TitanicPred$Pclass)
TitanicPred$SexNum <- as.numeric(TitanicPred$Sex)
TitanicPred$FamilyNum <- as.numeric(TitanicPred$Family)
TitanicPred$EmbarkedNum <- as.numeric(TitanicPred$Embarked)

corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")

for (i in 1:(ncol(TitanicPred) - 1)) {
	if (is.integer(TitanicPred[,i]) | is.numeric(TitanicPred[,i])) {
		spearman_test = cor.test(TitanicPred[,i],
														TitanicPred[,length(TitanicPred)],
														method = "spearman")
		corr_coef = spearman_test$estimate
		p_val = spearman_test$p.value
		# Add row to matrix
		pair = matrix(ncol = 2, nrow = 1)
		pair[1][1] = corr_coef
		pair[2][1] = p_val
		corr_matrix <- rbind(corr_matrix, pair)
		rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(TitanicPred)[i]
	}
}
kable(corr_matrix)

###  Welch Two Sample t-test 
TitanicPred$Survived <- as.numeric(TitanicPred$Survived)

###Mujeres mejor supervivencia que hombres
t.test(TitanicPred$Survived[TitanicPred$Sex=="female"],TitanicPred$Survived[TitanicPred$Sex=="male"],alternative="greater")
### 1º clase mejor supervivencia que 2º clase
t.test(TitanicPred$Survived[TitanicPred$Pclass==1],TitanicPred$Survived[TitanicPred$Pclass==2],alternative="greater")
### 2º clase mejor supervivencia que 3º clase
t.test(TitanicPred$Survived[TitanicPred$Pclass==2],TitanicPred$Survived[TitanicPred$Pclass==3],alternative="greater")
### Titulo honorifico mejor supervivencia que sin el
t.test(TitanicPred$Survived[TitanicPred$Title==1],TitanicPred$Survived[TitanicPred$Title==0],alternative="greater")
### con familiares mejor supervivencia que sin familiares
t.test(TitanicPred$Survived[TitanicPred$Family != 0],TitanicPred$Survived[TitanicPred$Family == 0],alternative="greater")

TitanicPred$Survived <- as.numeric(TitanicPred$Survived)-1
TitanicPred$Survived_factor <- as.factor(TitanicPred$Survived)


###  Arbol de Decision
X <-  subset(TitanicPred, select = c('Sex','Pclass','Family','FarePP','Embarked' )) 
y <- TitanicPred[,'Survived_factor']

TitanicDecisionTreeModel <- C50::C5.0( X, y) 
summary( TitanicDecisionTreeModel )
plot (TitanicDecisionTreeModel)




#### GRAFICAS

### Supervivencia sengun Sexos
p1 <- ggplot(TitanicPred, aes(x = Sex, fill = Survived_factor)) +
  geom_bar(stat='count', position='dodge') + 
  labs(x = 'Sex, TitanicPred data') +
        geom_label(stat='count', aes(label=..count..))

### Supervivencia por Clase 
p2 <- ggplot(TitanicPred, aes(x = Pclass, fill = Survived_factor )) +
  geom_bar(stat='count', position='dodge')  +
  labs(x = 'Pclass, TitanicPred data') + geom_label(stat='count', aes(label=..count..)) +
   theme(legend.position="none") + theme_grey() 

grid.arrange(p1,p2, nrow=1)

### Supervivencia por Clase y sexo
p3 <- ggplot(TitanicPred, aes(x = Pclass, fill = Survived_factor )) +
  geom_bar(stat='count', position='dodge') + facet_grid(.~Sex) +
  labs(x = 'Pclass, TitanicPred data') + geom_label(stat='count', aes(label=..count..)) +
   theme(legend.position="none") + theme_grey() 
grid.arrange(p3, nrow=1)



TitanicPred$FamilyBoolNum[TitanicPred$Family==0] <- 0
TitanicPred$FamilyBoolNum[TitanicPred$Family!=0] <- 1
TitanicPred$FamilyBool <- as.factor(TitanicPred$FamilyBoolNum)

### Supervivencia por Familia 
p4 <- ggplot(TitanicPred, aes(x = FamilyBool, fill = Survived_factor )) +
  geom_bar(stat='count', position='dodge')  +
  labs(x = 'FamilyBool, TitanicPred data') + geom_label(stat='count', aes(label=..count..)) +
   theme(legend.position="none") + theme_grey() 

### Supervivencia por Familia y sexo
p5 <- ggplot(TitanicPred, aes(x = FamilyBool, fill = Survived_factor )) +
  geom_bar(stat='count', position='dodge') + facet_grid(.~Sex) +
  labs(x = 'FamilyBool, TitanicPred data') + geom_label(stat='count', aes(label=..count..)) +
   theme(legend.position="none") + theme_grey() 
   
grid.arrange(p4,p5, nrow=1)

### Supervivencia por Familia sexo y Clase
p6 <- ggplot(TitanicPred, aes(x = Pclass, fill = Survived_factor )) +
  geom_bar(stat='count', position='dodge') + facet_grid(.~Sex+FamilyBool) +
  labs(x = 'Pclass, TitanicPred data') + geom_label(stat='count', aes(label=..count..)) +
   theme(legend.position="none") + theme_grey() 

grid.arrange(p6, nrow=1)

###  Arbol de Decision
plot (TitanicDecisionTreeModel)
