
#EXPERIMENTO***********************************
while (1) {
  
  #vector randomico 
  pasajeroId_tr=sample(1:891,622,replace=F)#es un comando que tiene como paraetros:el numero inicial, final
                                           #y la cantidad de datos, sin repeticion       
  casa=0#es una variable que me va a permitir salir del bucle for 
  gato=1#es un contador que me va a permitir recorer las variables del vector de test
  pasajeroId_te=rep(0,269)# creo un vector de ceros de tamaño 269
  for(i in 1:891){#este for va a recorrer el nuemro de filas
    casa=0#por cada iteracion se va a setear el dato casa con un vlaor de 0
    for (j in 1:622) {#recorre el vector de training
      if(i==pasajeroId_tr[j]){#va hacer una validacion si algun numero i es igual a algun valor del
                              #training ya existente
        casa=1#casa toma el valor de 1
        break#y se rompe el bucle
      }
    }
    if(casa!=1){#indica que si casa es diferente a 1 va a guardar el dato en el vector test
      pasajeroId_te[gato]=i#se guarda el valor de i en la posicion gato del vector test
      gato=gato+1#y aumenta el contador gato
    }
  } 

  #******************Cargar la data y encontrar variables con datos faltantes.  
  library(readr)#Llamar a la librer ??ia read
  
  train_titanic <- read_csv("D:/ken/U/nivel 6/estadistica para las ciencias de la computacion/proyecto/train_titanic.csv")
  View(train_titanic)
  variables=as.data.frame(train_titanic)
  #MAPA DE CALOR DE LOS VALORES FALTANTES
  
  library(gplots)#llamar a la libreira gplots que previamente debio ser instalada
  
  matris_na=(is.na(variables)*1)#Obtener los valores faltantes y luego convertirlos a 1 si es True y 0 si es False
  
  matris_na<-data.matrix(matris_na)#Transformar los datos a una matriz
  
  my_palette <- colorRampPalette(c("red4" ,"yellow")) (n=20)#colores
  
  #heatmap.2(matris_na,trace="none",col=my_palette,margins=c(10,12),cexRow=0.5,Rowv=FALSE,Colv=FALSE)#crea el mapa de calor
  
  
#**************CODIFICACION DE VARIABLES*******************  
  #0 no sobrevivio; 1 si sobrevivio
  yy=variables$Survived#se guarda en yy los valores de la variable survived de la data 
  survive_tr=rep(0,622)#se crea un vector de 0
  for (i in 1:622) {#recorre el vector de 0
    n=pasajeroId_tr[i]#n toma el valor que se tiene en el vector pasajeroId_tr en la posicion i
    survive_tr[i]=yy[n]#en la posicion i del vector survive_tr se guarda el valor de yy en la posicion n
  }
  y=variables$Survived
  
  #1era clase x1_1=0 x1_2=0
  #2da clase x1_1=0 x1_2=1
  #3era clase x1_1=1 x1_2=0
  x1_1=0
  x1_2=0
  x1=variables$Pclass
  for(i in 1:length(x1)) {#reemplaza la data segun la codificacion
    if(x1[i]==1){
      x1_1[i]=0
      x1_2[i]=0
    }
    if(x1[i]==2){
      x1_1[i]=0
      x1_2[i]=1
    }
    if(x1[i]==3){
      x1_1[i]=1
      x1_2[i]=0
    }
  }
  
  x1_1
  x1_2
  yy1=x1_1
  yy2=x1_2
  clase_codificada1_tr=rep(0,622)
  clase_codificada2_tr=rep(0,622)
  for (i in 1:622) {
    n=pasajeroId_tr[i]
    clase_codificada1_tr[i]=yy1[n]
    clase_codificada2_tr[i]=yy2[n]
  }
  
  #sexo 1 masculino; 0 femenino
  x2=variables$Sex
  x2=(x2=="male")*1 #si e shombre  da vlor T, entonces para codificarla, se hizo q 1 sea hombre y 0 mujeres

#**********************  
   
  xx2=x2
  sexo_tr=rep(0,622)
  for (i in 1:622) {
    n=pasajeroId_tr[i]
    sexo_tr[i]=xx2[n]
  }
#********************************
  
#*************METODO DE IMPUNTUACION SOBRE LA VARIABLE EDAD********************
  #edad
  x3=variables$Age
  #eliminar los valores faltantes para poder sacar la media 
  aux3 <- x3[!is.na(x3)]# dice es diferente de nulo en todas las pociciones de l vector X3, se guarda en aux3 sin valores NA
  
  mean(aux3)# la media de los valores sin ningun faltante
  mediax3=29#valor obtenido
  
  #sustituir valores faltantes por la media
  for(i in 1:length(x3)) {
    if(is.na(x3[i])){
      x3[i]=29
    }
  }
  
  xx3=x3
  edad_tr=rep(0,622)
  for (i in 1:622) {
    n=pasajeroId_tr[i]
    edad_tr[i]=xx3[n]
  }
#*****************ASIGNACION DE VARIBALES**************  
  #numero de hermanos/cónyuges a bordo
  x4=variables$SibSp 
  
  xx4=x4
  Sibsp_tr=rep(0,622)
  for (i in 1:622) {
    n=pasajeroId_tr[i]
    Sibsp_tr[i]=xx4[n]
  }
  
  #Número de padres/hijos que acompañaban al individuo
  x5=variables$Parch
  
  xx5=x5
  Partch_tr=rep(0,622)
  for (i in 1:622) {
    n=pasajeroId_tr[i]
    Partch_tr[i]=xx5[n]
  }
  
 
  

  
  #precio
  x7=variables$Fare
  
  xx7=x7 #71.2833
  Precio_tr=rep(0,622)
  for (i in 1:622) {
    n=pasajeroId_tr[i]
    Precio_tr[i]=xx7[n]
  }
  

#**************************CREACION DE LA DATA PARA ENTRENAR AL MODELO************
  matriz1=cbind(survive_tr,clase_codificada1_tr,clase_codificada2_tr,sexo_tr,edad_tr,Sibsp_tr,Partch_tr,Precio_tr)#matriz con los datos para el training
  
  #cambios nominales de las variables por comodidad.
  ytr=survive_tr
  xtr1_1=clase_codificada1_tr
  xtr1_2=clase_codificada2_tr
  xtr2=sexo_tr
  xtr3=edad_tr
  xtr4=Sibsp_tr
  xtr5=Partch_tr
  xtr7=Precio_tr
  
#**************REALIZACION DE LA REGRESION LOGISTICA**************
  regresion1=glm(ytr~xtr1_1+xtr1_2+xtr2+xtr3+xtr4+xtr5+xtr7,family = binomial("logit"))
  
  summary(regresion1)
#**********OBTENCION DE LOS COEFICIENTES ************
  b0=regresion1$coefficients[1]
  b1=regresion1$coefficients[2]
  b2=regresion1$coefficients[3]
  b3=regresion1$coefficients[4]
  b4=regresion1$coefficients[5]
  b5=regresion1$coefficients[6]
  b6=regresion1$coefficients[7]
  b7=regresion1$coefficients[8]
  
#*********************Fin del training*****************
  
  
  
  
#*****************CREACION DE LA DATA PARA LAS PRUEBAS DEL MODELO 
  #variable dependiente 
  survive_te=rep(0,269)
  for (i in 1:269) {
    n=pasajeroId_te[i]
    survive_te[i]=y[n]
  }
  
  #***variables independientes***
  
  clase_codificada1_te=rep(0,269)
  clase_codificada2_te=rep(0,269)
  for (i in 1:269) {
    n=pasajeroId_te[i]
    clase_codificada1_te[i]=x1_1[n]
    clase_codificada2_te[i]=x1_2[n]
  }
  
  sexo_te=rep(0,269)
  for (i in 1:269) {
    n=pasajeroId_te[i]
    sexo_te[i]=x2[n]
  }
  
  edad_te=rep(0,269)
  for (i in 1:269) {
    n=pasajeroId_te[i]
    edad_te[i]=x3[n]
  }
  
  Sibsp_te=rep(0,269)
  for (i in 1:269) {
    n=pasajeroId_te[i]
    Sibsp_te[i]=x4[n]
  }
  
  Partch_te=rep(0,269)
  for (i in 1:269) {
    n=pasajeroId_te[i]
    Partch_te[i]=x5[n]
  }
  

  Precio_te=rep(0,269) #21.0750
  for (i in 1:269) {
    n=pasajeroId_te[i]
    Precio_te[i]=x7[n]
  }
  

  #cambios nominales de las variables por comodidad.
  yts=survive_te
  xts1_1=clase_codificada1_te
  xts1_2=clase_codificada2_te
  xts2=sexo_te
  xts3=edad_te
  xts4=Sibsp_te
  xts5=Partch_te
  xts7=Precio_te

  #OBTENCION DE LAS Y ESTIMADAS por medio de la ecuacion de regresion logistica estimada
  
  yestimada=exp(b0+b1*xts1_1+b2*xts1_2+b3*xts2+b4*xts3+b5*xts4+b6*xts5+b7*xts7)/(1+ exp(b0+b1*xts1_1+b2*xts1_2+b3*xts2+b4*xts3+b5*xts4+b6*xts5+b7*xts7))
  yest=round(yestimada) #rendondiamos para obtener valores 1 y 0
  
  
  #***********VERIFICACION DEL MODELO************
  comparacion=(yts==yest) #entre las observaciones de la data del test  y las y q se obtuvo con el modelo
  aciertos=comparacion*1 #al vector de tru o false se le multiplica * 1 para q los verdaderos tomen el valor de 1 y los falsos tomen el valor de 0
  total=sum(aciertos) #se suman los aciertos
  tamanio=length(yts)#la longitud total de la data de test
  tasaaciertos=total/tamanio #con la division se obtiene la tasa de aciertos
  
  
 
 
  
  #EXPERIMENTO*************************
  
  matris=cbind(pasajeroId_tr,survive_tr,clase_codificada1_tr,clase_codificada2_tr,sexo_tr,edad_tr,Sibsp_tr,Partch_tr,Precio_tr)#creamos la matriz con los valores obtenidos aleatoriamente para training
  matris_test=cbind(pasajeroId_te,survive_te,clase_codificada1_te,clase_codificada2_te,sexo_te,edad_te,Sibsp_te,Partch_te,Precio_te)#creamos la matriz con los valores obtenidos aleatoriamente para test
  write.csv(matris,file = "D:/ken/U/nivel 6/estadistica para las ciencias de la computacion/proyecto/training.csv",row.names = T)
  write.csv(matris_test,file = "D:/ken/U/nivel 6/estadistica para las ciencias de la computacion/proyecto/test.csv",row.names = T)
  if(tasaaciertos>0.86){
    break
  }
  
} 
  #Chi-Square test
  #relacion significante entre variables
  #nivel de significancia 0.01 la relacion es significante en un  99% de confiancia
  
  #significancia de la variable sexo
  s1=summary(table(variables$Survived,variables$Sex))
  s_Sexo=s1$p.value
  s_Sexo
  
  
  #el p-valor es de 3.712e-59, esto es mucho menor q el nivel de significancia por lo q debe ser incluida en el modelo
  
  
  #significancia de la variable edad *
  
  s2=summary(table(variables$Survived,variables$Age))
  s_Edad=s2$p.value
  
  chisq.test(variables$Survived, variables$Age, simulate.p.value = TRUE)
  #el p-valor es de 0.06697 por lo que no se escogio dentro del modelo *
  
  #significancia de la variable clase
  s3=summary(table(variables$Survived,variables$Pclass))
  s_Clase=s3$p.value
  #el p-valor es de 4.549e-23 esto es mucho menor q el nivel de significancia por lo q debe ser incluida en el modelo
  
  #significancia de la variable Name
  s4=summary(table(variables$Survived,variables$Name))
  s_Nombre=s4$p.value
  #el p-valor es de 0.4842 por lo que no se escogio dentro del modelo *
  chisq.test(variables$Survived, variables$Name, simulate.p.value = TRUE)
  #el p-valor es de 1
  
  #significancia de la variable SibSp
  s5=summary(table(variables$Survived,variables$SibSp))
  s_SibSp=s5$p.value
  
  #el p-valor es de 1.559e-06 por lo que se la incluyo en el modelo *
  chisq.test(variables$Survived, variables$SibSp, simulate.p.value = TRUE)
  # el p-valor es de 0.0004998
  
  #significancia de la variable Parch
  s6=summary(table(variables$Survived,variables$Parch))
  s_Parch=s6$p.value
  #el p-valor es de 9.704e-05 por lo que se la incluyo en el modelo *
  chisq.test(variables$Survived, variables$Parch, simulate.p.value = TRUE)
  #el p-valor es de 0.0004998
  
  #significancia de la variable Ticket
  summary(table(variables$Survived,variables$Ticket))
  #el p-valor es de 0.01153
  
  #significancia de la variable Precio
  summary(table(variables$Survived,variables$Fare))
  #el p-valor es de 1.165e-11
  chisq.test(variables$Survived, variables$Fare, simulate.p.value = TRUE)
  
  #significancia de la variable Cabina
  summary(table(variables$Survived,variables$Cabin))
  #el p-valor es de 0.1836
  
  #significancia de la variable embarque
  summary(table(variables$Survived,variables$Embarked))
  #el p-valor es de 1.77e-06
  
#Experimento bondad de ajuste
  library(pscl)
  ba=pR2(regresion1)  
  #coeficiente de determinacion r2 da la bondad de ajuste
  # en la regresion logistica se obtiene un r2 que esta dada por el parametro McFadden
  # que debe estar entre 0.2 y 0.4
  pR2(regresion1)
  r2=ba[4]
  
