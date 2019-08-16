#laboratorio #1 Data Science
install.packages("ggplot2")
library(ggplot2)
library("psych")
library("ggplot2")
library("car")
library("Hmisc")
library("corrplot")
install.packages(c("psych","ggplot2","car","Hmisc","corrplot"))


sample <- read.csv(file="sample_submission.csv", header=TRUE)
test <- read.csv(file="test.csv", header=TRUE)
train <- read.csv(file="train.csv", header=TRUE)
summary(train)
qplot(train$LotFrontage, geom="histogram",main = "Histograma de distancia lineal de calle conectada a la propiedad") 
qplot(train$LotArea, geom="histogram",main = "Histograma de area en ft2 del lote") 
qplot(train$YearBuilt, geom="histogram",main = "Histograma del año de construccion") 
qplot(train$YearRemodAdd, geom="histogram",main = "Histograma del año de remodelacion ") 
qplot(train$MasVnrArea, geom="histogram",main = "Histograma de area en ft2 del masonry veneer") 
qplot(train$BsmtFinSF1, geom="histogram",main = "Histograma de ft2 terminados del tipo uno ") 
qplot(train$BsmtFinSF2, geom="histogram",main = "Histograma de ft2 terminados del tipo dos") 
qplot(train$BsmtUnfSF, geom="histogram",main = "Histograma de area ft2 no terminada")
qplot(train$TotalBsmtSF, geom="histogram",main = "Histograma de area ft2 terminada") 
qplot(train$BsmtFinSF1, geom="histogram") 
qplot(train$X1stFlrSF, geom="histogram",main = "Histograma de area en ft2 del primer piso") 
qplot(train$X2ndFlrSF, geom="histogram",main = "Histograma de area en ft2 del segundo piso") 
qplot(train$GrLivArea, geom="histogram",main = "Histograma de area en ft2 de living area debajo de grada") 

qplot(train$BsmtFullBath, geom="histogram",main = "Histograma de cantida de baños completos del sotano") 
qplot(train$BsmtHalfBath, geom="histogram",main = "Histograma de cantidad de medios baños del sotano") 
qplot(train$BedroomAbvGr, geom="histogram") 
qplot(train$KitchenAbvGr, geom="histogram") 

qplot(train$TotRmsAbvGrd, geom="histogram",main = "Histograma de cantidad de habitaciones debajo de grade") 
qplot(train$Fireplaces, geom="histogram",main = "Histograma de cantidad de chimeneas") 
qplot(train$GarageYrBlt, geom="histogram",main = "Histograma de año de fabricacion del garaje") 
qplot(train$GarageCars, geom="histogram",main = "Histograma de carros que caben en el garaje") 
qplot(train$GarageArea, geom="histogram",main = "Histograma de area en ft2 del garaje") 
qplot(train$WoodDeckSF, geom="histogram",main = "Histograma de area en ft2 de piso de madera") 
qplot(train$OpenPorchSF, geom="histogram",main = "Histograma de area en ft2 de portico abierto") 
qplot(train$EnclosedPorch, geom="histogram",main = "Histograma de area en ft2 de portico encerrado") 
qplot(train$X3SsnPorch, geom="histogram",main = "Histograma de area en ft2 de portico de 3 estaciones") 
qplot(train$ScreenPorch, geom="histogram",main = "Histograma de area en ft2 de portico con pantalla") 
qplot(train$PoolArea, geom="histogram",main = "Histograma de area en ft2 de piscina") 
qplot(train$MiscVal, geom="histogram",main = "Histograma de cantidad de datos miscelaneoos") 
qplot(train$YrSold, geom="histogram",main = "Histograma de años de venta") 
qplot(train$SalePrice, geom="histogram",main = "Histograma de precios de venta") 

##--------------------------ANALISIS DE CORRELACION DE VARIABLES

cuantitativas<-data.frame(
  
 "LotFrontage"=train$LotFrontage,
 "LotArea"= train$LotArea,
 "YearBuilt"=train$YearBuilt,
 "YearRemodAdd"=train$YearRemodAdd,
 "MasVnrArea"=train$MasVnrArea,
 "BsmtFinSF1"=train$BsmtFinSF1,
 "BsmtFinSF2"=train$BsmtFinSF2,
 "BsmtUnfSF"=train$BsmtUnfSF,
 "TotalBsmtSF"=train$TotalBsmtSF,
 "BsmtFinSF1"=train$BsmtFinSF1,
 "X1stFlrSF"=train$X1stFlrSF,
 "X2ndFlrSF"=train$X2ndFlrSF, 
 "GrLivArea"=train$GrLivArea,
 "BsmtFullBath"=train$BsmtFullBath ,
 "BsmtHalfBath"=train$BsmtHalfBath,
 "BedroomAbvGr"=train$BedroomAbvGr,
 "KitchenAbvGr"=train$KitchenAbvGr, 
 "TotRmsAbvGrd"=train$TotRmsAbvGrd,
 "Fireplaces"=train$Fireplaces,
 "GarageYrBlt"=train$GarageYrBlt,
 "GarageCars"=train$GarageCars,
 "GarageArea"=train$GarageArea,
 "WoodDeckSF"=train$WoodDeckSF,
 "OpenPorchSF"=train$OpenPorchSF ,
 "EnclosedPorch"=train$EnclosedPorch,
 "X3SsnPorch"=train$X3SsnPorch,
 "ScreenPorch"=train$ScreenPorch,
 "PoolArea"=train$PoolArea,
 "MiscVal"=train$MiscVal,
 "YrSold"=train$YrSold,
 "SalePrice"=train$SalePrice
)

cor(cuantitativas)


##Correlation matrix 
prueba <- na.omit(cuantitativas)

prueba <-   cor(prueba,method  ="pearson");
round (prueba, digits=2)
corrplot(prueba,tl.col = "black",tl.srt = 45)
corrplot(prueba, method ="shade",tl.col = "black",tl.srt = 45,order="AOE") ##ESTA ES LA QUE MEJOR SALE

col<- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
corrplot(cuantitativas.cor, method="shade",shade.col=NA, tl.col="black",tl.srt=45,col=col(200), addCoef.col="black",addcolorlabel="no")

##variables cualitativas

barplot(table(train$MoSold),main = "Cantidad por mes")
barplot(table(train$MSSubClass),main = "Cantidad por tipo de negocio")
barplot(table(train$MSZoning),main = "Cantidad por tipo de zona")
barplot(table(train$Street),main = "Cantidad por tipo de material de carretera de acceso  ")
barplot(table(train$Alley),main = "Cantidad por tipo de acceso")
barplot(table(train$LotShape),main = "Cantidad segun forma del lote")
barplot(table(train$LandContour),main = "Cantidad segun tipo de terreno")
barplot(table(train$Utilities),main = "Cantidad segun tipo de utilidades")
barplot(table(train$LotConfig),main = "Cantidad segun configuracion del lote")
barplot(table(train$LandSlope),main = "Cantidad segun tipo de cuesta")
barplot(table(train$Neighborhood),main = "Cantidad segun tipo de vecindario")
barplot(table(train$Condition1),main = "Cantidad segun proximidad en condicion")
barplot(table(train$Condition2),main = "Cantidad segun proximidad")
barplot(table(train$BldgType),main = "Cantidad segun tipo de vivienda")
barplot(table(train$HouseStyle),main = "Cantidad segun tipo de casa")
barplot(table(train$OverallQual),main = "Cantidad segun calidad de la casa")
barplot(table(train$OverallCond),main = "Cantidad segun condicion de la casa")
barplot(table(train$RoofStyle),main = "Cantidad segun tipo de techo")
barplot(table(train$RoofMatl),main = "Cantidad segun tipo de material de tehco")
barplot(table(train$Exterior1st),main = "Cantidad segun cobertura exterior de la vivienda")
barplot(table(train$Exterior2nd),main = "Cantidad segun segunda cobertuta exterior de la vivienda")
barplot(table(train$MasVnrType),main = "Cantidad segun masonry veneer")
barplot(table(train$ExterQual),main = "Cantidad segun calidad de materiales del exterior")
barplot(table(train$ExterCond),main = "Cantidad segun la condicion externa")
barplot(table(train$Foundation),main = "Cantidad segun el tipo de base")
barplot(table(train$BsmtQual),main = "Cantidad segun la altura del sotano")
barplot(table(train$BsmtCond),main = "Cantidad segun la condicion del sotan")
barplot(table(train$BsmtExposure),main = "Cantidad segun la exposicion del sotano")
barplot(table(train$BsmtFinType1),main = "Cantidad segun rating de sotano")
barplot(table(train$BsmtFinType2),main = "Cantidad segun rating (si hay multiples)")
barplot(table(train$Heating),main = "Cantidad seugn tipo de calefaccion")
barplot(table(train$HeatingQC),main = "Cantidad segun calidad y condicion de calefaccion")
barplot(table(train$CentralAir),main = "Cantidad segun presencia de aire acondicionado ")
barplot(table(train$Electrical),main = "Cantidad segun sistemas electricos")
barplot(table(train$KitchenQual),main = "Cantidad segun calidad de cocina")
barplot(table(train$Functional),main = "Cantidad segun funcionalidades")
barplot(table(train$FireplaceQu) ,main = "Cantidad segun calidad de chimeneas")
barplot(table(train$GarageType),main = "Cantidad segun tipo (ubicaicon) de garajes")
barplot(table(train$GarageFinish),main = "Cantidad segun materiales de interior de garaje")
barplot(table(train$GarageQual),main = "Cantidad segun calidad del garaje" )
barplot(table(train$GarageCond), main = "Cantidad segun la condicion del garaje")
barplot(table(train$PavedDrive),main = "Cantidad segun pavimentacion o no de la calle")
barplot(table(train$PoolQC),main = "Cantidad segun calidad de piscina")
barplot(table(train$Fence),main = "Cantidad segun calidad de cerca")
barplot(table(train$MiscFeature),main = "Cantidad segun calidad de miscelanea no cubierta")
barplot(table(train$MoSold),main = "Cantidad segun mes de venta")
barplot(table(train$SaleType),main = "Cantidad segun tipo de venta")
barplot(table(train$SaleCondition),main = "Cantidad segun condicion de venta")


#Clusters
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n???mero de clusters ???ptimo
library(factoextra) #Para hacer gr???ficos bonitos de clustering

#Clustering jerárquico
hc<-hclust(dist(train[,1:78])) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) 
groups<-cutree(hc,k=3)
datos$gruposHC<-groups


g1HC<-datos[datos$gruposHC==1,]
g2HC<-datos[datos$gruposHC==2,]
g3HC<-datos[datos$gruposHC==3,]


#Método de la silueta para clustering jerárquico
silch<-silhouette(groups,dist(train[,1:78]))
mean(silch[,3]) 

#Método de Ward para determinar el número correcto de clusteres con k-medias
#Para saber cual es el mejor numero de clusters
wss <- (nrow(train[,1:4])-1)*sum(apply(train[,1:78],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(train[,1:4], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Paquete para saber el mejor n???mero de clusters
nb <- NbClust(train[,1:4], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")

#Visualizaci???n de los clusters con factoextra
#Visualizaci???n de las k-medias
fviz_cluster(km, data = train[,1:78],geom = "point", ellipse.type = "norm")

#Visualizaci???n de cluster jer???rquico
hc.cut<-hcut(train[,1:78], k=3, hc_method = "complete")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
fviz_cluster(hc.cut, ellipse.type = "convex")


#Reglas de asociacion
install.packages("arules")
 install.packages("arulesViz")
library(arules)
 
# El mínimo nivel de soporte y confianza aceptados
reglas<- apriori(prueba, parameter = list(support = 0.2,
                                        confidence = 0.70,
                                        target = "rules"))



