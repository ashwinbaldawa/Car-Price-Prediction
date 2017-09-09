#Load the Essential libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(car)
library(mass)
library(stringr)

# Read the Carprice file into the R-Studio

carprice<-read.csv('CarPrice_Assignment.csv',stringsAsFactors = TRUE)
str(carprice)
View(carprice)

#Removing the unwanted columns
carprice$car_ID<-NULL
View(carprice)
#The CarName Attribute should be stored as Character

carprice$CarName<-as.character(carprice$CarName)
str(carprice)

#Check if any dupliate rows are present in the Data Frame

Car_dup<-unique(carprice$car_ID)
nrow(carprice)
View(Car_dup)

# There are no duplicate rows present in the dataset
# Checking for the NA Values if any

Car_NA<-sum(is.na(carprice))
View(Car_NA)

#There are no NA Values present in the Dataset
#Removing the outliers from the Dataset

quantile(carprice$horsepower,seq(0,1,0.01))

#The Horsepower Variable hass someoutliers,there is a jump in value at 98%
#Setting values > 184.00 to 184  

carprice$horsepower[which(carprice$horsepower>184.00)]<-184.00
View(carprice)

#Separating the Carname variables to Company Name/CarModel name

str(carprice)
y <- as.data.frame(str_split_fixed(carprice$CarName, " ", 2))
colnames(y) <- c("CompName", "CarModel")
carprice<-cbind(carprice,y)
carprice$CarName<-NULL
View(carprice)
str(carprice)

#Removing the Car Model name as it should not be considered as an independent variable

carprice$CarModel<-NULL
View(carprice)

#Cleaning the Incorrect names of the Car Companies:
#This is to avoid creation of extra column when the categorical data is converted to numeric

levels(carprice$CompName)[levels(carprice$CompName)=="porcshce"] <- "porsche"
levels(carprice$CompName)[levels(carprice$CompName)=="Nissan"]   <- "nissan"
levels(carprice$CompName)[levels(carprice$CompName)=="toyouta"]  <- "toyota"
levels(carprice$CompName)[levels(carprice$CompName)=="vokswagen"]<- "volkswagen"
levels(carprice$CompName)[levels(carprice$CompName)=="maxda"]    <- "mazda"
levels(carprice$CompName)[levels(carprice$CompName)=="vw"]    <- "volkswagen"

View(carprice)

#Converting Factor Variable FuelType to Numeric:

summary(levels(carprice$fueltype))
levels(carprice$fueltype)<-c(1,0)
carprice$fueltype<-as.numeric(levels(carprice$fueltype))[carprice$fueltype]

#Converting Factor Variable Aspiration to Numeric:

levels(carprice$aspiration)<-c(1,0)
carprice$aspiration<-as.numeric(levels(carprice$aspiration))[carprice$aspiration]

#converting Factor variable DoorNumber to Numeric:

levels(carprice$doornumber)<-c(1,0)
carprice$doornumber<-as.numeric(levels(carprice$doornumber))[carprice$doornumber]

#converting Factor variable Engine allocation to Numeric:

levels(carprice$enginelocation)<-c(1,0)
carprice$enginelocation<-as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

#Converting Factor Variable CarBody to Numeric:

dummy_1 <- data.frame(model.matrix( ~carbody, data = carprice))
dummy_1<-dummy_1[,-1]

#Combine the Dummy variable to the Main Matrix:

carprice<-cbind(carprice[,-5],dummy_1)

#Converting Factor Variable EngineType to Numeric:

dummy_2<-data.frame(model.matrix(~enginetype,data = carprice))
dummy_2<-dummy_2[,-1]

#Combine the Dummy variable EngineBody to the Main Matrix:

carprice<-cbind(carprice[,-12],dummy_2)

#Converting Factor Variable EngineType to Numeric:

dummy_3<-data.frame(model.matrix(~drivewheel,data = carprice))
dummy_3<-dummy_3[,-1]

#Combine the Dummy variable EngineBody to the Main Matrix:

carprice<-cbind(carprice[,-5],dummy_3)

#Converting Factor Variable CylinderNumber to Numeric:

dummy_4<-data.frame(model.matrix(~cylindernumber,data = carprice))
dummy_4<-dummy_4[,-1]

#Combine the Dummy variable CylinderNumber to the Main Matrix:

carprice<-cbind(carprice[,-11],dummy_4)

#Converting Factor Variable FuelSystem to Numeric:

dummy_5<-data.frame(model.matrix(~fuelsystem,data = carprice))
dummy_5<-dummy_5[,-1]

#Combine the Dummy variable FuelSystem to the Main Matrix:

carprice<-cbind(carprice[,-12],dummy_5)
View(carprice)
levels(carprice$CompName)

#Converting Factor Variable CarName to Numeric:

dummy_6<-data.frame(model.matrix(~CompName,data = carprice))
dummy_6<-dummy_6[,-1]

#Combine the Dummy variable for CarName to the Main Matrix:

carprice<-cbind(carprice[,-20],dummy_6)

#Building the Multiple linear regression model for the dataset:
#Set the Seed and divide the data into training and test data set

set.seed(100)

train_ind<-sample(1:nrow(carprice),0.7*nrow(carprice))

train_data<-carprice[train_ind,]
View(train_set)
test_set <-carprice[-train_ind,]

#Executing the First Multi Model:

model_1<-lm(price~.,data = train_data)
summary(model_1)

#The Summary of the above model has many insignificant variables

#Using the StepAIC function on the Training dataset to find

stepAIC(model_1,direction = "both")

model_2<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              curbweight + enginesize + boreratio + stroke + peakrpm + 
              citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginetypedohcv + enginetypel + enginetypeohcf + 
              enginetyperotor + drivewheelrwd + cylindernumberfive + cylindernumberthree + 
              fuelsystem2bbl + fuelsystemmpfi + CompNamebmw + CompNamebuick + 
              CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
              CompNamemercury + CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
              CompNamerenault + CompNamesaab + CompNametoyota + CompNamevolkswagen, 
              data = train_data)
summary(model_2)
vif(model_2)

#The Variables Enginesize,Car Width and Curbweight are highly collinear

c1<-data.frame(carprice$enginesize,carprice$carwidth,carprice$curbweight)
cor(c1)

#Removing Curbweight variable as it is has very high multicolinearity and as an independent variable it
#is heavily dependent Carwidth and Engine Size 

model_3<-lm(formula = price ~ aspiration + enginelocation 
              + boreratio + stroke + peakrpm + carwidth + enginesize +
              citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginetypedohcv + enginetypel + enginetypeohcf + 
              enginetyperotor + drivewheelrwd + cylindernumberfive + cylindernumberthree + 
              fuelsystem2bbl + fuelsystemmpfi + CompNamebmw + CompNamebuick + 
              CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
              CompNamemercury + CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
              CompNamerenault + CompNamesaab + CompNametoyota + CompNamevolkswagen, 
            data = train_data)
summary(model_3)
vif(model_3)

#Removing emfi as it has high VIF and p>0.05

model_4<-lm(formula = price ~ aspiration + enginelocation 
            + boreratio + stroke + peakrpm + carwidth + enginesize +
            +citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginetypedohcv + enginetypel + enginetypeohcf + 
              enginetyperotor + drivewheelrwd + cylindernumberfive + cylindernumberthree + 
              fuelsystem2bbl  + CompNamebmw + CompNamebuick + 
              CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
              CompNamemercury + CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
              CompNamerenault + CompNamesaab + CompNametoyota + CompNamevolkswagen, 
            data = train_data)
summary(model_4)
vif(model_4)

#citympg

model_5<-lm(formula = price ~ aspiration + enginelocation 
            + boreratio + stroke + peakrpm + carwidth + enginesize +
              + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginetypedohcv + enginetypel + enginetypeohcf + 
              enginetyperotor + drivewheelrwd + cylindernumberfive + cylindernumberthree + 
              fuelsystem2bbl  + CompNamebmw + CompNamebuick + 
              CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
              CompNamemercury + CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
              CompNamerenault + CompNamesaab + CompNametoyota + CompNamevolkswagen, 
            data = train_data)
summary(model_5)
vif(model_5)

#FuelSystem 2bbl

model_6<-lm(formula = price ~ aspiration + enginelocation 
            + boreratio + stroke + peakrpm + carwidth + enginesize +
              + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginetypedohcv + enginetypel + enginetypeohcf + 
              enginetyperotor + drivewheelrwd + cylindernumberfive + cylindernumberthree + 
               CompNamebmw + CompNamebuick + 
              CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
              CompNamemercury + CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
              CompNamerenault + CompNamesaab + CompNametoyota + CompNamevolkswagen, 
            data = train_data)
summary(model_6)
vif(model_6)

# Bore ratio

model_7<-lm(formula = price ~ aspiration + enginelocation 
            + stroke + peakrpm + carwidth + enginesize +
              + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginetypedohcv + enginetypel + enginetypeohcf + 
              enginetyperotor + drivewheelrwd + cylindernumberfive + cylindernumberthree + 
              CompNamebmw + CompNamebuick + 
              CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
              CompNamemercury + CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
              CompNamerenault + CompNamesaab + CompNametoyota + CompNamevolkswagen, 
            data = train_data)
summary(model_7)
vif(model_7)

#Cylinder Number Five

model_8<-lm(formula = price ~ aspiration + enginelocation 
            + stroke + peakrpm + carwidth + enginesize +
              + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginetypedohcv + enginetypel + enginetypeohcf + 
              enginetyperotor + drivewheelrwd  + cylindernumberthree + 
              CompNamebmw + CompNamebuick + 
              CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
              CompNamemercury + CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
              CompNamerenault + CompNamesaab + CompNametoyota + CompNamevolkswagen, 
            data = train_data)
summary(model_8)
vif(model_8)


#Companyname Saab

model_9<-lm(formula = price ~ aspiration + enginelocation 
            + stroke + peakrpm + carwidth + enginesize +
              + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginetypedohcv + enginetypel + enginetypeohcf + 
              enginetyperotor + drivewheelrwd  + cylindernumberthree + 
              CompNamebmw + CompNamebuick + 
              CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
              CompNamemercury + CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
              CompNamerenault + CompNametoyota + CompNamevolkswagen, 
            data = train_data)
summary(model_9)
vif(model_9)


#CompanyName Mercury

model_10<-lm(formula = price ~ aspiration + enginelocation 
            + stroke + peakrpm + carwidth + enginesize +
              + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginetypedohcv + enginetypel + enginetypeohcf + 
              enginetyperotor + drivewheelrwd  + cylindernumberthree + 
              CompNamebmw + CompNamebuick + 
              CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
              CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
              CompNamerenault + CompNametoyota + CompNamevolkswagen, 
            data = train_data)
summary(model_10)
vif(model_10)

#Removing Enginetypel

model_11<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + carwidth + enginesize +
               + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + cylindernumberthree + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNamerenault + CompNametoyota + CompNamevolkswagen, 
             data = train_data)
summary(model_11)
vif(model_11)


#Cylinder Numer Three

model_12<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + carwidth + enginesize +
               + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNamerenault + CompNametoyota + CompNamevolkswagen, 
             data = train_data)
summary(model_12)
vif(model_12)

#CarBody Sedan

model_13<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + carwidth + enginesize +
               + carbodyhardtop + carbodyhatchback  + 
               carbodywagon + enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNamerenault + CompNametoyota + CompNamevolkswagen, 
             data = train_data)
summary(model_13)
vif(model_13)

#Car Hatchback

model_14<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + carwidth + enginesize +
               + carbodyhardtop + carbodywagon  + 
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNamerenault + CompNametoyota + CompNamevolkswagen, 
             data = train_data)
summary(model_14)
vif(model_14)

#Carbodyhard top

model_15<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + carwidth + enginesize +
               +   carbodywagon +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNamerenault + CompNametoyota + CompNamevolkswagen, 
             data = train_data)
summary(model_15)
vif(model_15)

#Carbody Wagon

model_16<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + carbodywagon +
               CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNamerenault + CompNametoyota + CompNamevolkswagen, 
             data = train_data)
summary(model_16)
vif(model_16)

model_17<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNamerenault + CompNametoyota + CompNamevolkswagen, 
             data = train_data)
summary(model_17)
vif(model_17)

#Company Name Volkswagen

model_18<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNamerenault + CompNametoyota , 
             data = train_data)
summary(model_18)
vif(model_18)

#Company Name Renault

model_19<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge + CompNamehonda + CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
                CompNametoyota , 
             data = train_data)
summary(model_19)
vif(model_19)

#Company Name Honda

model_20<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge +  CompNamejaguar + CompNamemazda + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNametoyota , 
             data = train_data)
summary(model_20)
vif(model_20)

#Company Name Mazda

model_21<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor + drivewheelrwd  + 
               CompNamebmw + CompNamebuick + 
               CompNamedodge +  CompNamejaguar + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNametoyota , 
             data = train_data)
summary(model_21)
vif(model_21)

#Driver Wheel rwd

model_22<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor +  
               CompNamebmw + CompNamebuick + 
               CompNamedodge +  CompNamejaguar + 
               CompNamemitsubishi + CompNamenissan + CompNameplymouth + 
               CompNametoyota , 
             data = train_data)
summary(model_22)
vif(model_22)

#Company Name Nissan

model_23<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor +  
               CompNamebmw + CompNamebuick + 
               CompNamedodge +  CompNamejaguar + 
               CompNamemitsubishi +  CompNameplymouth + 
               CompNametoyota , 
             data = train_data)
summary(model_23)
vif(model_23)

#Toyota

model_24<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor +  
               CompNamebmw + CompNamebuick + 
               CompNamedodge +  CompNamejaguar + 
               CompNamemitsubishi +  CompNameplymouth
               , 
             data = train_data)
summary(model_24)
vif(model_24)

#dodge

model_25<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor +  
               CompNamebmw + CompNamebuick + 
              CompNamejaguar + 
               CompNamemitsubishi +  CompNameplymouth
             , 
             data = train_data)
summary(model_25)
vif(model_25)

#Plymouth

model_26<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypedohcv + enginetypeohcf + 
               enginetyperotor +  
               CompNamebmw + CompNamebuick + 
               CompNamejaguar + 
               CompNamemitsubishi 
             , 
             data = train_data)
summary(model_26)
vif(model_26)

#Engine pedo hcv

model_27<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
                enginetypeohcf + 
               enginetyperotor +  
               CompNamebmw + CompNamebuick + 
               CompNamejaguar + 
               CompNamemitsubishi 
             , 
             data = train_data)
summary(model_27)
vif(model_27)

#mitsubushi

model_28<-lm(formula = price ~ aspiration + enginelocation 
             + stroke + peakrpm + enginesize +
               enginetypeohcf + 
               enginetyperotor +  
               CompNamebmw + CompNamebuick + 
               CompNamejaguar 
               
             , 
             data = train_data)
summary(model_28)
vif(model_28)

View(test_set)

Predict_price<-predict(model_28,test_set[,-19])
test_set$test_price<-Predict_price


test_set$car_id<-seq(1,nrow(test_set))


View(test_set)
ggplot(test_set, aes(car_id,price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "days", breaks = seq(0,65,3), limits = c(0,65)) + 
  scale_y_continuous(name = "Views_show", breaks = seq(0,50000,2000), limits = c(0,50000)) + geom_line(aes(x=car_id, y=test_price, colour="red"))



#correlation

cor_out<-cor(test_set$price,test_set$test_price)
View(cor_out)

r_squ<-cor_out^2
View(r_squ)































































