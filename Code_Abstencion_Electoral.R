library(readxl)
library(stargazer)
library(dynlm)
library(carData)
library(car)
library(lmtest)
library(orcutt)
library(readr)

##Se carga la base de datos preparada para nuestro modelo
abstencionelectoral <- read_excel("D:/Desktop/PORTFOLIO PROJECT/R_Projects/Abstencion_electoral/Base de datos depurada.xlsx")
View(abstencionelectoral)

#Para facilitar el desarrollo y la lectura renombrmos las variables
names(abstencionelectoral) <- c("Y","IngresoPerCapita","EdadVotante","AdultosReligiosos","NivelEducativo")

#Hallamos la media, mediana, varianza, desviación estándar
summary.data.frame(abstencionelectoral)

sd(abstencionelectoral$`Y`)
sd(abstencionelectoral$`IngresoPerCapita`)
sd(abstencionelectoral$`EdadVotante`)
sd(abstencionelectoral$`AdultosReligiosos`)
sd(abstencionelectoral$`NivelEducativo`)

#En 'Y' nos da:
# Mediana :0.6796                                                                   
# Promedio:0.6787
# Desv. estándar:0.0593703

#En 'IngresoPerCapita' da como resultado
# Mediana :55.47                                                                    
# Promedio:57.82
# Desv. estándar:9.85848

#En 'EdadVotante' da como resultado
# Mediana :38.60                                                                  
# Promedio:38.74
# Desv. estándar:2.368118

#En 'AdultosReligiosos' da como resultado
# Mediana :0.5400                                                                
# Promedio:0.5467
# Desv. estándar:0.1063704

#Y en 'NivelEducativo' nos da:
# Mediana :0.2964                                                                    
# Promedio:0.3015
# Desv. estándar:0.06102279


#Sacamos la covarianza y correlación e muestrales

#Covarianzas
#Entre 'Y' e 'IngresoPerCapita'
cov(abstencionelectoral
    $`Y`,abstencionelectoral
    $`IngresoPerCapita`)
#0.2196932

#Entre 'Y' y 'EdadVotante'
cov(abstencionelectoral
    $`Y`,abstencionelectoral
    $`EdadVotante`)
#0.03793042
# Entre 'Y' y 'AdultosReligiosos'
cov(abstencionelectoral
    $`Y`,abstencionelectoral
    $`AdultosReligiosos`)
#-0.003800864

#Entre 'Y' y 'NivelEducativo'
cov(abstencionelectoral
    $`Y`,abstencionelectoral
    $`NivelEducativo`)
#0.001762069

#Correlaciones
#Entre 'Y' y 'IngresoPerCapita'
cor(abstencionelectoral
    $`Y`,abstencionelectoral
    $`IngresoPerCapita`)
# 0.3753509

#Entre 'Y' y 'EdadVotante'
cor(abstencionelectoral
    $`Y`,abstencionelectoral
    $`EdadVotante`)
# 0.2697833

# Entre 'Y' y 'AdultosReligiosos'
cor(abstencionelectoral
    $`Y`,abstencionelectoral
    $`AdultosReligiosos`)
# -0.6018555

#Entre 'Y' y 'NivelEducativo'
cor(abstencionelectoral
    $`Y`,abstencionelectoral
    $`NivelEducativo`)
# 0.4863643

#Hacemos un gráfico previo
pairs(abstencionelectoral)

#Planteamos posibles modelos de regresión

mLogLog<- lm(Y ~ log(IngresoPerCapita) + log(AdultosReligiosos)
             + log(NivelEducativo)
             + log(EdadVotante), data = abstencionelectoral)
summary(mLogLog)
plot(mLogLog)


mLinLin<- lm(Y ~ IngresoPerCapita + AdultosReligiosos
             + NivelEducativo
             + EdadVotante, data = abstencionelectoral)
summary(mLinLin)
plot(mLinLin)


mLogLin<- lm(log(Y) ~ IngresoPerCapita + log(AdultosReligiosos)
             + log(NivelEducativo)
             + EdadVotante, data = abstencionelectoral)
summary(mLogLin)
plot(mLogLin)

mLinLog<- lm(Y ~ log(IngresoPerCapita) + log(AdultosReligiosos)
             + log(NivelEducativo)
             + log(EdadVotante), data = abstencionelectoral)
summary(mLinLog)
plot(mLinLog)

#Para este punto notamos que la variable Edadvotante no es significativa
# y afecta a otras variables, por lo que se toma la decisión de
#retirarla del modelo
part.electoral<- abstencionelectoral[,-3]
View(part.electoral)

#Probamos con el nuevo modelo, ahora sin esa variable

NM.LogLog<- lm(log(Y) ~ log(IngresoPerCapita)
             + log(AdultosReligiosos)
             + log(NivelEducativo), data = part.electoral)
summary(NM.LogLog)
plot(NM.LogLog)

NM.LinLin<- lm(Y ~ IngresoPerCapita
                   + AdultosReligiosos
                   + NivelEducativo, data = part.electoral)
summary(NM.LinLin)
plot(NM.LinLin)

NM.LogLin<- lm(log(Y) ~ IngresoPerCapita
                   + AdultosReligiosos
                   + NivelEducativo, data = part.electoral)
summary(NM.LogLin)
plot(NM.LogLin)

NM.LinLog<- lm(Y ~ log(IngresoPerCapita)
               + log(AdultosReligiosos)
               + log(NivelEducativo), data = part.electoral)
summary(NM.LinLog)
plot(NM.LinLog)

#Enccontramos en el Lin-Lin y el modelo Lin-Log como candidatos para
#nuestro modelo, por lo que creamos una tabla en la cual se puedan
#observar de manera más clara

stargazer(NM.LinLin)
stargazer(NM.LinLog)

#Encontramos que no existe diferencia significativa entre
#la explicación del modelo, por lo que continuamos con el
#modelo Lin-Lin para mayor prácticidad del trabajo


#######################################################################################################
####### INTERPRETACIÓN DEL MODELO RESULTANTE(Lin-lin):

# El modelo Estimado finalmente es: Y=??_1+??_2 IngresoPerCapita+??_3 AdultosReligiosos + ??_4 NivelEducativo+U_i

#Donde:
# Y = Participacion Electoral
#??_1 = 0.842109: Este valor corresponde al intercepto,indica que si todas las otras variables tienen un valor 
#de cero, la participación electoral estimada será del 84.21%
#??_2 = -0.002732: El valor de ??_1 nos dice que con un incremento de una unidad en el Ingreso per Capita, el 
#porcentaje de participación tiene una reducción de 0.002732.
#??_3 = 0.315863: Nos dice que cuando todas otras las variables están constantes, un incremento de una 
#unidad en el porcentaje de adultos religiosos genera una reducción de (0.315863) en el porcentaje de 
#participación electoral.
#??_4 = 0.554503: Un incremento de una unidad en el nivel educativo, genera un incremento de 0.554503 en
#el porcentaje de participación política cuando todas las otras variables permanecen constantes.

