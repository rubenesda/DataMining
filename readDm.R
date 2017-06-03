#Reading Dataset
library(data.table)
library(ggplot2)
library(lubridate)
library(corrplot)

auto <- fread("autos.csv",stringsAsFactors = T)

#write.csv(auto, file = "MyData.csv",row.names=FALSE)

##################
#Cleansing Dataset
##################


auto$dateCrawled <- ymd_hms(auto$dateCrawled) # Format time
auto$dateCreated <- ymd_hms(auto$dateCreated)
auto$lastSeen <- ymd_hms(auto$lastSeen)

#######################
#Creation new variables
#######################

#Age time
#auto$age <- as.integer(year(today()) - auto$yearOfRegistration)
#Selling time
auto$sellingTime <- as.integer(as.Date(auto$lastSeen) - as.Date(auto$dateCreated))

auto <- auto[order(auto$sellingTime),]
#write.csv(auto, file = "MyData2.csv",row.names=FALSE)


auto$nrOfPictures <- NULL #Delete useless columns because all values are zeros
auto$seller <- NULL #They are verly little the difference of 0.0008% significance
auto$offerType <- NULL #Very little the difference  of 0.003% of significance

##################
# More Cleansing Dataset
##################

auto <- auto[price<150000&price>100] # Price between 60 and 150000 Euros
auto <- auto[vehicleType != ""]
auto <- auto[yearOfRegistration>=1900&yearOfRegistration<2017]
auto[gearbox == "", gearbox := NA]
auto <- auto[powerPS>0&powerPS<5000]
auto[model == "", model := NA]
auto[monthOfRegistration == 0, monthOfRegistration := NA]
auto[fuelType == "", fuelType := NA]
auto[notRepairedDamage == "", notRepairedDamage := NA]
# nom <- strsplit(as.character(auto$name),split = "_")
# auto$model <- as.factor(sapply(nom,"[[",1))

#Summary
#summary(auto)


###############
#Visualization
###############

#summary(auto$yearOfRegistration)
ggplot(aes(x="yearOfRegistration", y=yearOfRegistration), data = auto) +
  geom_boxplot()

ggplot(aes(x=vehicleType, y=yearOfRegistration), data = auto) +
  geom_boxplot() +
  ylim(1975, 2016)

#Price

ggplot(auto,aes(price))+
  stat_density(fill="blue")+scale_x_log10(labels = scales::dollar_format(suffix = "€", prefix = ""))+
  labs(title="Vehicle Price",subtitle="In Euros")

ggplot(auto, aes(x=price)) +
  geom_bar(fill='darkgreen', color='black') +
  scale_fill_brewer(type= 'div') +
  labs(x= 'Mounts', y= 'number of price') +
  ggtitle('Price Frequency Diagram')

#Vehicle Type
ggplot(auto, aes(x=vehicleType)) +
  geom_bar(fill='blue', color='black') +
  scale_fill_brewer(type= 'div') +
  labs(x= 'Vehicle Type', y= 'number of cars') +
  ggtitle('Vehicle Type Frequency Diagram')

#Engine Power of each vehicle
ggplot(auto, aes(x=auto$powerPS)) +
  geom_bar(fill='blue', color='black') +
  labs(x= 'engine power', y= 'number of cars') +
  ggtitle('Histogram of Engine Power (PowerPS)')+xlim(0,500)


ggplot(auto, aes(auto$powerPS)) +
  geom_histogram(fill= ('blue'), color='black', binwidth=15) +
  labs(x= 'engine power', y= 'number of cars') +
  ggtitle('Histogram of Engine Power (PowerPS)')+xlim(-1,500)

#Gearbox
ggplot(aes(x= gearbox), data=subset(auto, !is.na(gearbox))) +
  geom_bar(color='black', fill='orange') +
  labs(x= 'Gearbox', y='Number of Cars', title='Gearbox')

#Kilometer
ggplot(aes(auto$kilometer), data=auto) +
  geom_bar(color='black', fill='orange')+
  scale_y_log10()+
labs(x= 'Kilometer', y='Number of Cars', title='Kilometer Histogram')

#Month of Registration
ggplot(aes(x=table(auto$monthOfRegistration))) +
  geom_bar(fill='orange', color='black') +
  labs(x= 'Months ', y='Number of Cars', title= 'Month of registration Frequency Diagram')

#Fuel Type
ggplot(aes(x=fuelType), data=subset(auto, !is.na(fuelType))) +
  geom_bar(aes(fill=fuelType), color='black') +
  labs(x= 'Fuel Type', y='Number of Cars', title= 'Fuel Type Frequency Diagram')


#Model (Complicate of visualize)
ggplot(aes(x= model), data=subset(auto, !is.na(model))) +
  geom_bar(color='black', fill='orange') +
  labs(x= 'Model', y='Number of Cars', title='Models Frequency Diagram')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Brand
ggplot(aes(x= brand), data=subset(auto, !is.na(brand))) +
  geom_bar(color='black', fill='orange') +
  labs(x= 'Brands', y='Number of Cars', title='Brands Frequency Diagram')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#NotRepairedDamage
ggplot(data=subset(auto, !is.na(notRepairedDamage)), aes(x=notRepairedDamage)) +
  geom_bar(fill='blue', color='black') +
  scale_fill_brewer(type= 'div') +
  labs(x= 'Repaired', y= 'number of cars') +
  ggtitle('Repaired Damage Frequency Diagram')

#Abtest
ggplot(data=subset(auto, !is.na(abtest)), aes(x=abtest)) +
  geom_bar(fill='orange', color='black') +
  scale_fill_brewer(type= 'div') +
  labs(x= 'Abtest', y= 'number of cars') +
  ggtitle('Abtest Frequency Diagram')

#Selling Time
ggplot(data=auto, aes(auto$sellingTime)) +
  geom_histogram(breaks=seq(0, 35, by = 1),
                 col="black",
                 fill="orange"
                 ) +
  labs(title="Selling Time Frequency diagram") +
  labs(x="Selling Time (Days)", y="Count")

##############
#Correlations
##############

auto_corr <- subset(auto, !is.na(powerPS))[,.(price,powerPS,kilometer,as.numeric(age),as.numeric(sellingTime),as.numeric(gearbox))]
colnames(auto_corr) <- c("Price","Engine Power\n(PowerPS)","Kilometer","Age","Selling\nTime","GearBox")
corrplot.mixed(cor(auto_corr))


# References first source
# https://www.kaggle.com/donyoe/d/orgesleka/used-cars-database/exploring-used-cars-database/notebook
# https://www.kaggle.com/timucinanuslu/d/orgesleka/used-cars-database/data-crunchers/notebook
# https://www.kaggle.com/orgesleka/used-cars-database/discussion

# References codes in R
# https://www.r-bloggers.com/installing-r-packages/
# http://stackoverflow.com/questions/16736197/counting-occurences-in-column-and-create-variable-in-r
# http://rprogramming.net/write-csv-in-r/
# https://www.rdocumentation.org/packages/caTools/versions/1.17.1/topics/sample.split
# http://machinomics.blogspot.com.co/2012/03/multiclass-svm-with-e1071_20.html









