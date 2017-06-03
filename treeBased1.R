#Proof Support Vector Machine

#Reference
#http://stackoverflow.com/questions/27125381/implement-multi-class-classification-using-svm-in-r
#http://stackoverflow.com/questions/34328105/how-to-build-multiclass-svm-in-r
#http://stackoverflow.com/questions/22009871/how-to-perform-multi-class-classification-using-svm-of-e1071-package-in-r
#https://www.youtube.com/watch?v=iu9rrCbSqgM

#Query Google
# https://www.google.com.co/search?q=multiclass+svm+R&source=lnms&sa=X&ved=0ahUKEwiNh4_vzNbTAhWBOCYKHQI0AKEQ_AUIBygA&biw=1360&bih=638&dpr=1

#Youtube video
# https://www.youtube.com/watch?v=Hu71tIm1iX0


library(rpart)
set.seed(1)

#Copy of original before edition
backupAuto <- auto

# Erase the fields NA that already keeping
auto<-na.omit(auto)

################### Change attributes categorical to number #################

auto$abtest <- as.character(auto$abtest)
auto[abtest == "control", abtest := "1"]
auto[abtest == "test", abtest := "2"]
auto$abtest <- as.factor(auto$abtest)
auto$abtest <- as.numeric(auto$abtest)

auto$vehicleType <- as.character(auto$vehicleType)
auto[vehicleType == "andere", vehicleType := "1"]
auto[vehicleType == "bus", vehicleType := "2"]
auto[vehicleType == "cabrio", vehicleType := "3"]
auto[vehicleType == "coupe", vehicleType := "4"]
auto[vehicleType == "kleinwagen", vehicleType := "5"]
auto[vehicleType == "kombi", vehicleType := "6"]
auto[vehicleType == "limousine", vehicleType := "7"]
auto[vehicleType == "suv", vehicleType := "8"]
auto$vehicleType <- as.factor(auto$vehicleType)
auto$vehicleType <- as.numeric(auto$vehicleType)

auto$gearbox <- as.character(auto$gearbox)
auto[gearbox == "automatik", gearbox := "1"]
auto[gearbox == "manuell", gearbox := "2"]
auto$gearbox <- as.factor(auto$gearbox)
auto$gearbox <- as.numeric(auto$gearbox)

auto$fuelType <- as.character(auto$fuelType)
auto[fuelType == "andere", fuelType := "1"]
auto[fuelType == "benzin", fuelType := "2"]
auto[fuelType == "cng", fuelType := "3"]
auto[fuelType == "diesel", fuelType := "4"]
auto[fuelType == "elektro", fuelType := "5"]
auto[fuelType == "hibrid", fuelType := "6"]
auto[fuelType == "lpg", fuelType := "7"]
auto$fuelType <- as.factor(auto$fuelType)
auto$fuelType <- as.numeric(auto$fuelType)

auto$notRepairedDamage <- as.character(auto$notRepairedDamage)
auto[notRepairedDamage == "ja", notRepairedDamage := "1"]
auto[notRepairedDamage == "nein", notRepairedDamage := "2"]
auto$notRepairedDamage <- as.factor(auto$notRepairedDamage)
auto$notRepairedDamage <- as.numeric(auto$notRepairedDamage)


auto$brand <- as.character(auto$brand)
auto[brand == "alfa_romeo", brand := "1"]
auto[brand == "audi", brand := "2"]
auto[brand == "bmw", brand := "3"]
auto[brand == "chevrolet", brand := "4"]
auto[brand == "chrysler", brand := "5"]
auto[brand == "citroen", brand := "6"]
auto[brand == "dacia", brand := "7"]
auto[brand == "daewoo", brand := "8"]
auto[brand == "daihatsu", brand := "9"]
auto[brand == "fiat", brand := "10"]
auto[brand == "ford", brand := "11"]
auto[brand == "honda", brand := "12"]
auto[brand == "hyundai", brand := "13"]
auto[brand == "jaguar", brand := "14"]
auto[brand == "jeep", brand := "15"]
auto[brand == "kia", brand := "16"]
auto[brand == "lada", brand := "17"]
auto[brand == "lancia", brand := "18"]
auto[brand == "land_rover", brand := "19"]
auto[brand == "mazda", brand := "20"]
auto[brand == "mercedez_benz", brand := "21"]
auto[brand == "mini", brand := "22"]
auto[brand == "mitsubichi", brand := "23"]
auto[brand == "nissan", brand := "24"]
auto[brand == "opel", brand := "25"]
auto[brand == "peugeot", brand := "26"]
auto[brand == "porsche", brand := "27"]
auto[brand == "renault", brand := "28"]
auto[brand == "rover", brand := "29"]
auto[brand == "saab", brand := "30"]
auto[brand == "seat", brand := "31"]
auto[brand == "skoda", brand := "32"]
auto[brand == "smart", brand := "33"]
auto[brand == "sonstige_auto", brand := "34"]
auto[brand == "subaru", brand := "35"]
auto[brand == "suzuki", brand := "36"]
auto[brand == "toyota", brand := "37"]
auto[brand == "trabant", brand := "38"]
auto[brand == "volkswagen", brand := "39"]
auto[brand == "volvo", brand := "40"]
auto$brand <- as.factor(auto$brand)
auto$brand <- as.numeric(auto$brand)


auto$sellingTime[auto$sellingTime>=0&auto$sellingTime<=3]<-1
auto$sellingTime[auto$sellingTime>3&auto$sellingTime<=11]<-2
auto$sellingTime[auto$sellingTime>11]<-3
auto$sellingTime<-as.factor(auto$sellingTime)
table(auto$sellingTime)



# auto$sellingTime[auto$sellingTime==0]<-0
# auto$sellingTime[auto$sellingTime>=1&auto$sellingTime<=2]<-1
# auto$sellingTime[auto$sellingTime>2&auto$sellingTime<=5]<-2
# auto$sellingTime[auto$sellingTime>5&auto$sellingTime<=10]<-3
# auto$sellingTime[auto$sellingTime>10&auto$sellingTime<=17]<-4
# auto$sellingTime[auto$sellingTime>17]<-5
# auto$sellingTime<-as.factor(auto$sellingTime)
# table(auto$sellingTime)


# auto$sellingTime[auto$sellingTime==0]<-0
# auto$sellingTime[auto$sellingTime>=1&auto$sellingTime<=2]<-1
# auto$sellingTime[auto$sellingTime>2&auto$sellingTime<=5]<-2
# auto$sellingTime[auto$sellingTime>5&auto$sellingTime<=8]<-3
# auto$sellingTime[auto$sellingTime>8&auto$sellingTime<=13]<-4
# auto$sellingTime[auto$sellingTime>13&auto$sellingTime<=19]<-5
# auto$sellingTime[auto$sellingTime>19]<-6
# auto$sellingTime<-as.factor(auto$sellingTime)
# table(auto$sellingTime)

############################ CHoose a subconjunto #######################

auto$dateCrawled <- NULL #Delete
auto$name <- NULL #Delete
auto$dateCreated <- NULL #Delete
auto$lastSeen <- NULL #Delete
auto$model <- NULL #Delete
auto$postalCode <- NULL # Delete

################# Transform some variables to numeric ####################

auto$price <- as.numeric(auto$price)
auto$yearOfRegistration <- as.numeric(auto$yearOfRegistration)
auto$kilometer <- as.numeric(auto$kilometer)
auto$monthOfRegistration <- as.numeric(auto$monthOfRegistration)
auto$powerPS <- as.numeric(auto$powerPS)

#BackUp after edition
autoEdit <- auto

##################### Trabajar con una muestra representative #############
require(caTools)
muestra <- sample.split(auto$sellingTime, SplitRatio = 0.999999)
sampleWork <- auto[muestra==TRUE,]
table(sampleWork$sellingTime)

########## Create the samples of train and test for tree model ############

require(caTools)
index_car <- sample.split(sampleWork$sellingTime, SplitRatio=.7)
trainset_car <- sampleWork[index_car==TRUE,]
table(trainset_car$sellingTime)
testset_car <- sampleWork[index_car==FALSE,]
table(testset_car$sellingTime)

############################### PCA in auto ###############################
pca.train <- trainset_car[,-12]
pca.test <- testset_car[,-12]
prin_comp <- prcomp(pca.train, scale. = T)

biplot(prin_comp, scale = 0)

prin_comp$rotation[1:11,1:4]

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)

prop_varex[1:11]

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")




############################## Arbol 1 Clasification ########################
library(rpart)
myf <- sellingTime ~ kilometer  + yearOfRegistration + brand+ price

dtm1 <- rpart(sellingTime~., data=trainset_car, 
              method="class", 
              control=rpart.control(minsplit=10, minbucket=10, cp=0.00001))
dtm1$cptable
plotcp(dtm1)
library(rpart.plot)
rpart.plot(dtm1)

p1<- predict(dtm1, testset_car, type="class")
table(unlist(testset_car[,12]),p1)

# prune the tree 
dtm1P<- prune(dtm1, cp=dtm1$cptable[which.min(dtm1$cptable[,"xerror"]),"CP"])
rpart.plot(dtm1P)

p1p<- predict(dtm1P, testset_car, type="class")
table(unlist(testset_car[,12]),p1p)

############################## Arbol 1 Regresion ############################
library(rpart)

dtmR1 <- rpart(sellingTime~., data=trainset_car, 
              method="anova", 
              control=rpart.control(minsplit=10, minbucket=10, cp=0.0005))
dtmR1$cptable
library(rpart.plot)
rpart.plot(dtmR1)

pR1<- predict(dtmR1, testset_car, type="class")
table(unlist(testset_car[,12]),pR1)

# prune the tree 
dtm1P<- prune(dtm1, cp=dtm1$cptable[which.min(dtm1$cptable[,"xerror"]),"CP"])
rpart.plot(dtm1P)

p1p<- predict(dtm1P, testset_car, type="class")
table(unlist(testset_car[,12]),p1p)



######################### Arbol 2 ####################################

library(rpart)
dtm2 <- rpart(sellingTime~., data=trainset_car, 
              method="class", 
              control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
dtm2$cptable
library(rpart.plot)
rpart.plot(dtm2)

p2<- predict(dtm2, testset_car, type="class")
table(unlist(testset_car[,12]),p2)

# prune the tree 
dtm2P<- prune(dtm2, cp=dtm2$cptable[which.min(dtm2$cptable[,"xerror"]),"CP"])
rpart.plot(dtm2P)

p2p<- predict(dtm2P, testset_car, type="class")
table(unlist(testset_car[,12]),p2p)


########################### Arbol 3 ##################################

library(rpart)
dtm3 <- rpart(sellingTime~., data=trainset_car, 
              method="class", 
              control=rpart.control(minsplit=1, minbucket=1, cp=0.005))
dtm3$cptable
library(rpart.plot)
rpart.plot(dtm3)

p3<- predict(dtm3, testset_car, type="class")
table(unlist(testset_car[,12]),p3)

# prune the tree 
dtm3P<- prune(dtm3, cp=dtm3$cptable[which.min(dtm3$cptable[,"xerror"]),"CP"])
rpart.plot(dtm3P)

p3p<- predict(dtm3P, testset_car, type="class")
table(unlist(testset_car[,12]),p3p)

######################## Cross validation ##############################
require(caret)
flds <- createFolds(auto$sellingTime, k = 5, list = TRUE, returnTrain = FALSE)
names(flds)[1] <- "train"

for(i in 1:5){
  testset_car<-auto[ flds[[i]], ]
  trainset_car<-auto[ -flds[[i]], ]
  
  dtmC <- rpart(sellingTime~., data=trainset_car, 
              method="class", 
              control=rpart.control(minsplit=1, minbucket=1, cp=0.01))

  rpart.plot(dtmC)
  
  pC<- predict(dtmC, testset_car, type="class")
  print(paste0("Cross Validation Experiment: ", i))
  print(knitr::kable(table(unlist(testset_car[,12]),pC)))
  
}



#Experiments





#Referemce
# http://stackoverflow.com/questions/27125381/implement-multi-class-classification-using-svm-in-r
# https://stackoverflow.com/questions/20993073/the-result-of-rpart-is-just-with-1-root
# https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/predict.rpart.html
# https://www.r-bloggers.com/cross-validation-for-predictive-analytics-using-r/
# https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
# https://stackoverflow.com/questions/20993073/the-result-of-rpart-is-just-with-1-root
# https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/
# http://www.statmethods.net/advstats/cart.html
# https://stackoverflow.com/questions/16453625/package-domc-not-available-for-r-version-3-0-0-warning-in-install-packages
# https://topepo.github.io/caret/parallel-processing.html
# https://github.com/Danko-Lab/Rgtsvm
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
