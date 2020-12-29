library(readxl)
library(lattice)
library(ggplot2)
library(tidyverse)
library(MuMIn)
library(broom)
library(caret)
library(FSA)
library(dunn.test)
library(DescTools)
library(rcompanion)
library(multcompView)
library(GmAMisc)
library(PMCMR)
library(gamm4)
library(mgcv)

#Cargar archivo y añadir el nombre de las colunas
EVI <- read_excel("EVI.xlsx")
EVI$Formacion <- as.factor(EVI$Formacion)

#Para ver la media y la mediana de cada formacion
Summarize(EVI ~ Formacion,
          data = EVI)

histogram(~ EVI | Formacion,
          data=EVI,
          layout=c(1,6))


boxplot(EVI ~ Formacion,
        data = EVI,
        ylab="EVI",
        xlab="Formación")

#Existe  una diferencia significativa en las distribuciones en los valores entre formaciones, que se evidencia en histogramas
#y prueba de kruskall wallis
kruskal.test(EVI ~ Formacion,
             data = EVI)



# Ordenar las formaciones  por mediana
EVI$Formacion = factor(EVI$Formacion,
                       levels=c("Junquillo", "Tubul", "Galega", "Junco", "Cebadilla", "Llinto"))

### Dunn test, dos formas, ambas con tablas de letras para saber cuales son iguales. 
#Forma 1
Post = posthoc.kruskal.dunn.test(EVI ~ Formacion, data=EVI)
PT1 = Post$p.value
PT2 = fullPTable(PT1)

PT1
multcompLetters(PT2,  
                compare="<",  
                threshold=0.05,
                Letters=letters,  
                reversed = FALSE)

#MODELO EXPLICATIVO


#Cargar archivo y añadir el nombre de las colunas
llinto <- read_excel("Llinto.xlsx")
junco <- read_excel("Junco.xlsx")
galega <- read_excel("Galega.xlsx")
cebadilla <- read_excel("Cebadilla.xlsx")
junquillo <- read_excel("Junquillo.xlsx")
tubul <- read_excel("Tubul.xlsx")

#Se les suma una cte para eliminar valores negativos de NDVI, EVI y T_min
llinto <- llinto[,2:8] + 1.2
junco <- junco[,2:8] + 1.2
galega <- galega[,2:8] + 1.2
cebadilla <- cebadilla[,2:8] + 1.2
junquillo <- junquillo[,2:8] + 1.2
tubul <- tubul[,2:8] + 1.2

##SABER SI LAS VARIABLES ESTAN CORRELACIONADAS
#Correlación de variables.Las variables pueden estar en un mismo modelo cuando tienen una correlación menor a 0.7, 
#-1 significa que no se considera la primera columna (que es la variable respuesta), abs es porque se utilizan los valores absolutos

corr <- abs(cor(llinto[,3:7])) <= 0.7
corr[!lower.tri(corr)] <- NA #De la matriz anterior, solo se conserva un valor y no por duplicado


#metodo de crossvalidation, la base de datos se dividira en 5
ctrl <- trainControl(method = "cv", number = 5)


Modelo_llinto_gamma <- train(EVI ~ pp + T_min + T_max + T_mean + PET, data = llinto, family = Gamma, method = "glm", trControl = ctrl)
Modelo_llinto_gaussian <- train(EVI ~ pp + T_min + T_max + T_mean + PET, data = llinto, family = gaussian, method = "glm", trControl = ctrl)
options(na.action = "na.omit")


Selected_Modelo_llinto_gamma <- dredge(Modelo_llinto_gamma, subset = corr)
Selected_Modelo_llinto_gamma <- dredge(Modelo_llinto_gaussian, subset = corr)
