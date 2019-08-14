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
qplot(train$LotFrontage, geom="histogram") 
qplot(train$LotArea, geom="histogram") 
qplot(train$YearBuilt, geom="histogram") 
qplot(train$YearRemodAdd, geom="histogram") 
qplot(train$MasVnrArea, geom="histogram") 
qplot(train$BsmtFinSF1, geom="histogram") 
qplot(train$BsmtFinSF2, geom="histogram") 
qplot(train$BsmtUnfSF, geom="histogram") 
qplot(train$TotalBsmtSF, geom="histogram") 
qplot(train$BsmtFinSF1, geom="histogram") 
qplot(train$X1stFlrSF, geom="histogram") 
qplot(train$X2ndFlrSF, geom="histogram") 
qplot(train$GrLivArea, geom="histogram") 

qplot(train$BsmtFullBath, geom="histogram") 
qplot(train$BsmtHalfBath, geom="histogram") 
qplot(train$BedroomAbvGr, geom="histogram") 
qplot(train$KitchenAbvGr, geom="histogram") 

qplot(train$TotRmsAbvGrd, geom="histogram") 
qplot(train$Fireplaces, geom="histogram") 
qplot(train$GarageYrBlt, geom="histogram") 
qplot(train$GarageCars, geom="histogram") 
qplot(train$GarageArea, geom="histogram") 
qplot(train$WoodDeckSF, geom="histogram") 
qplot(train$OpenPorchSF, geom="histogram") 
qplot(train$EnclosedPorch, geom="histogram") 
qplot(train$X3SsnPorch, geom="histogram") 
qplot(train$ScreenPorch, geom="histogram") 
qplot(train$PoolArea, geom="histogram") 
qplot(train$MiscVal, geom="histogram") 
qplot(train$YrSold, geom="histogram") 
qplot(train$SalePrice, geom="histogram") 

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

barplot(table(train$MoSold))

barplot(table(train$MoSold))
barplot(table(train$MSSubClass))
barplot(table(train$MSZoning))
barplot(table(train$Street))
barplot(table(train$Alley))
barplot(table(train$LotShape))
barplot(table(train$LandContour))
barplot(table(train$Utilities))
barplot(table(train$LotConfig))
barplot(table(train$LandSlope))
barplot(table(train$Neighborhood))
barplot(table(train$Condition1))
barplot(table(train$Condition2))
barplot(table(train$BldgType))
barplot(table(train$HouseStyle))
barplot(table(train$OverallQual))
barplot(table(train$OverallCond))
barplot(table(train$RoofStyle))
barplot(table(train$RoofMatl))
barplot(table(train$Exterior1st))
barplot(table(train$Exterior2nd))
barplot(table(train$MasVnrType))
barplot(table(train$ExterQual))
barplot(table(train$ExterCond))
barplot(table(train$Foundation))
barplot(table(train$BsmtQual))
barplot(table(train$BsmtCond))
barplot(table(train$BsmtExposure))
barplot(table(train$BsmtFinType1m))
barplot(table(train$BsmtFinType2))
barplot(table(train$Heating))
barplot(table(train$HeatingQC))
barplot(table(train$CentralAir))
barplot(table(train$Electrical))
barplot(table(train$KitchenQual))
barplot(table(train$Functional))
barplot(table(train$FireplaceQu))
barplot(table(train$GarageType))
barplot(table(train$GarageFinish))
barplot(table(train$GarageQual))
barplot(table(train$GarageCond))
barplot(table(train$PavedDrive))
barplot(table(train$PoolQC))
barplot(table(train$MiscFeature))
barplot(table(train$MoSold))
barplot(table(train$SaleType))
barplot(table(train$SaleCondition))





