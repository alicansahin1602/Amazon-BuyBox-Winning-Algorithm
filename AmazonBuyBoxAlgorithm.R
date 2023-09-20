library(dplyr)
library(car)
library(ggplot2)
setwd("/Users/alicansahin/Desktop/BOUN/4.sınıf/2.Dönem/IE 544/Project")
trainData = as.data.frame(readRDS("amz_train.rds"))
summary(trainData)

trainData$day = substr(trainData[,"epoc"],1,10)
sellers = unique(trainData$sid)
products = unique(trainData$pid)
#1.a

numberofProduct = length(unique(trainData$pid)) #number of unique products
numberofSeller = length(unique(trainData$sid)) #number of unique sellers

totalProduct = trainData[,c("pid","day")] %>% group_by(day) %>% group_by(day) %>% summarise(TotalProduct = n())
totalProduct$day = unique(substr(trainData[,"epoc"],1,10))
print(totalProduct,n = 23)
dates = unique(substr(trainData[,"epoc"],1,10))
products = unique(trainData$pid)
productMatrix = matrix(nrow = length(products),ncol = length(dates))

for(i in 1:length(products)){
  for(j in 1:length(dates)){
    productMatrix[i,j] = dim(trainData[trainData$pid == products[i] & trainData$day == dates[j],])[1]
  }
}
productMatrix
rownames(productMatrix) = products
colnames(productMatrix) = dates
productMatrix

totalSellerMatrix = matrix(ncol = 1,nrow = length(dates))
for(i in 1:length(dates)){
  totalSellerMatrix[i,1] = length(unique(trainData[trainData$day == dates[i],]$sid))
}
rownames(totalSellerMatrix) = dates
colnames(totalSellerMatrix) = c("Number of Sellers")
totalSellerMatrix



#1.b

unique(trainData[trainData$pid == "B002ZV0OJO",]$bbox_price)
options(pillar.sigfig = 4)
averageMatrix = matrix(nrow = length(products),ncol = 3)


trainData[,c("pid","price")] %>% group_by(pid) %>% summarise(maxPrice = max(price))
trainData[,c("pid","bbox_price")] %>% group_by(pid) %>% summarise(maxBboxPrice = max(bbox_price))
trainData[,c("pid","shipping")] %>% group_by(pid) %>% summarise(maxShippingPrice = max(shipping))

trainData[,c("pid","price")] %>% group_by(pid) %>% summarise(minPrice = min(price))
trainData[,c("pid","bbox_price")] %>% group_by(pid) %>% summarise(minBboxPrice = min(bbox_price))
trainData[,c("pid","shipping")] %>% group_by(pid) %>% summarise(minShippingPrice = min(shipping))

trainData[,c("pid","price")] %>% group_by(pid) %>% summarise(averagePrice = mean(price))
trainData[,c("pid","bbox_price")] %>% group_by(pid) %>% summarise(averageBboxPrice = mean(bbox_price))
trainData[,c("pid","shipping")] %>% group_by(pid) %>% summarise(averageShippingPrice = mean(shipping))




#1.c
head(trainData,5)


productGroup = trainData[,c("pid","sid_rating","sid_pos_fb","sid_rating_cnt","pid_rating","pid_rating_cnt","bbox")] %>% group_by(pid)



ggplot(productGroup,aes(bbox,sid_rating)) + 
  geom_point() + facet_grid(.~pid)
  
ggplot(productGroup,aes(bbox,sid_pos_fb)) + 
  geom_point() + facet_grid(.~pid)
  
ggplot(productGroup,aes(bbox,sid_rating_cnt)) + 
  geom_point() + facet_grid(.~pid)

ggplot(productGroup,aes(bbox,pid_rating)) + 
  geom_point() + facet_grid(.~pid)

ggplot(productGroup,aes(bbox,pid_rating_cnt)) + 
  geom_point() + facet_grid(.~pid)




#1.d
percantageBBOXTotal = matrix(nrow = numberofSeller,ncol = 1)
colnames(percantageBBOX) = "Percantage"
rownames(percantageBBOX) = unique(trainData$sid)

mean(as.numeric(trainData[trainData$sid == "amazon",]$bbox == "success"))*100 #total % of amazon success

totalSaleProd = trainData[trainData$sid == "amazon",c("pid","bbox")] %>% group_by(pid) %>% summarise(totalSale = n())
totalSuccesProd = trainData[trainData$sid == "amazon" & trainData$bbox == "success",c("pid","bbox")] %>% group_by(pid) %>% summarise(totalSucces = n())

successPercProd = merge(totalSaleProd,totalSuccesProd,all = T)
successPercProd$Perc = (successPercProd$totalSucces / successPercProd$totalSale)*100
successPercProd

trainData$week = week(trainData$day)
trainData

totalSaleWeek = trainData[trainData$sid == "amazon",c("week","bbox")] %>% group_by(week) %>% summarise(totalSale = n())
totalSuccesWeek = trainData[trainData$sid == "amazon" & trainData$bbox == "success",c("week","bbox")] %>% group_by(week) %>% summarise(totalSucces = n())

successPercWeek = merge(totalSaleWeek,totalSuccesWeek,all = T)
successPercWeek
successPercWeek$Perc = (successPercWeek$totalSucces / successPercWeek$totalSale)*100
successPercWeek

#1.e
sellerInfo = trainData[,c("sid","is_prime","is_fba","page","rank","bbox")] %>% group_by(sid)
sellerInfo = sellerInfo[!duplicated(sellerInfo),]
sellerInfo

productInfo = trainData[,c("pid","is_prime","is_fba","page","rank","bbox")] %>% group_by(pid)
productInfo = productInfo[!duplicated(productInfo),]
productInfo


mosaicplot(bbox~is_prime,sellerInfo)
mosaicplot(bbox~is_prime,productInfo)
mosaicplot(bbox~is_fba,sellerInfo)
mosaicplot(bbox~is_fba,productInfo)
mosaicplot(bbox~page,sellerInfo)
mosaicplot(bbox~page,productInfo)
mosaicplot(bbox~rank,sellerInfo)
mosaicplot(bbox~rank,productInfo)


#1f
head(trainData,5)


#sid_rating/price compared to other sellers within the same day.
#pid_rating/price
#sdi_pos_fb with 7.5 threshold
#sid_rating * sid_pos_fb 
#best price or not = is it the lowest price compared to other sellers with same time and same product





head(trainData,5)

trainDataCopy = trainData

trainDataCopy$PoS = trainDataCopy$sid_rating/trainDataCopy$price
trainDataCopy$sid_pos_fbIndicator = as.numeric(trainDataCopy$sid_pos_fb >= 7.5)
trainDataCopy$sRatingPos = trainDataCopy$sid_rating * trainDataCopy$sid_pos_fb
trainDataCopy$PidOP = trainDataCopy$pid_rating/trainDataCopy$price

ggplot(trainDataCopy,aes(bbox,PoS)) +
  geom_point()
  
ggplot(trainDataCopy,aes(bbox,sid_pos_fbIndicator)) +
  geom_point()


ggplot(trainDataCopy,aes(bbox,sRatingPos)) +
  geom_point()

ggplot(trainDataCopy,aes(bbox,PidOP)) +
  geom_point()



ggplot(trainDataCopy,aes(bbox,sid_rating)) + #below 4 does not win
  geom_point()



trainDataCopy[trainDataCopy$pid == "B00VSIT5UE" & as.character(trainDataCopy$epoc) == "2015-08-27 13:03:45",] #an example where another seller beats amazon. Even if the sid_rating_count is low it beats

trainDataCopy$IsBestPrice = c(rep(0,dim(trainDataCopy)[1]))


library(data.table)
trainDataCopy = data.table(trainDataCopy)
pidEpocGroup = trainDataCopy[,.SD[which.min(price)],by=c("pid","epoc")]
pidEpocGroup$IsBestPrice = rep(1,length(pidEpocGroup$IsBestPrice))


head(trainDataCopy,10)
pidEpocGroup
trainDataCopy = trainDataCopy[pidEpocGroup, on=.(pid,epoc,sid), c("IsBestPrice"):=.(i.IsBestPrice)][]
length(trainDataCopy[trainDataCopy$IsBestPrice==1,]$IsBestPrice)

mean(trainDataCopy[trainDataCopy$bbox == "success",]$IsBestPrice)*100 #Percantage of winning bbox with best price.


head(trainDataCopy,5)
trainDataCopy[trainDataCopy$pid == "B002ZV0OJO" & as.character(trainDataCopy$epoc) == "2015-08-11 09:04:13",]

trainData = data.frame(trainDataCopy)
head(trainData,5)



#1.g

#price => ratio
#sid_rating => interval 
#sid_rating_cnt => interval
#shipping => ratio
#page => ordinal
#rank => ordinal
#pid_rating => interval
#pid_rating_cnt => interval
#is_fba => nominal
#is_prime => nominsl
#bbox_price => ratio

#1.h
ggplot(trainDataCopy,aes(bbox,PoS)) +
  geom_point()

ggplot(trainDataCopy,aes(bbox,sRatingPos)) +
  geom_point()

trainDataCopy[trainDataCopy$sRatingPos <= 30 & trainDataCopy$bbox =="success",] #maybe outlier

ggplot(trainDataCopy,aes(bbox,sid_pos_fb)) + 
  geom_point()

trainDataCopy[trainDataCopy$sid_pos_fb == 7.5 & trainDataCopy$bbox =="success",] #maybe outlier
 #maybe outlier

is.na(trainDataCopy)

trainData[!complete.cases(trainData),] #there is nan values corresponding to shipping prices. This values always creates failure. Therefore, it can be misleading and count as outliers.



#2
#Score-Based = hc,tabu
#Constrain-Based = gs,iamb,mmpc,si.hiton.pc
#Mixed = mmhc,rsmax2
library(bnlearn)
head(trainData,2)
#Take-out : pid,epoc,sid,page,rank,bbox_sid,bbox_price,day,PoS,sRatingPoS,week
dataForModel =trainData[,-c(1,2,3,9,10,15,16,18,19,21,23)]
dataForModel$sid_pos_fbIndicator = factor(dataForModel$sid_pos_fbIndicator)
dataForModel$IsBestPrice = factor(dataForModel$IsBestPrice)

dataForModel = na.omit(dataForModel)
head(dataForModel)
summary(dataForModel)

model1 = hc(dataForModel)
score(model1,dataForModel) #1.526.142
graphviz.plot(a)



model12 = tabu(dataForModel)
score(model12,dataForModel)
graphviz.plot(aprime)

dataForModel = dataForModel[,-c(1)] #price out
model2 = hc(dataForModel)
score(model2,dataForModel) #693.226
graphviz.plot(model2)






dataForModel$IsShipping = factor(as.numeric(dataForModel$shipping >0))
head(dataForModel)
dataForModel = dataForModel[,-4] #shipping out

model4 = hc(dataForModelPrime)
score(model4,dataForModelPrime) #764.292
graphviz.plot(model4)
summary(dataForModelPrime)

dataForModel = dataForModel[,-c(2)] #sid_pos_fb out
model5 = hc(dataForModel)
score(model5,dataForModel)#310.744
graphviz.plot(model5)

head(dataForModel)
dataForModel$sid_rating = factor(as.numeric(dataForModel$sid_rating >= 4))
model6 = hc(dataForModel)
score(model6,dataForModel) #319.140

dataForModel$IsAmazon = factor(as.numeric(dataForModel$sid_rating_cnt == 100))
model7 = hc(dataForModel)
score(model7,dataForModel) #323.016

dataForModel = dataForModel[,-2] #remove sid_rating_cnt
model8 = hc(dataForModel)
score(model8,dataForModel) #169.399
head(dataForModel)
graphviz.plot(model8,layout = "fdp")



#Constraint Based
wlist = data.frame(from = c("IsAmazon"),to = c("bbox"))
model1c = gs(dataForModel,undirected = F)
model1c
graphviz.plot(model1c)

model2c = iamb(dataForModel,undirected = F)
model2c
graphviz.plot(model2c)

model3c = mmpc(dataForModel,undirected = F,whitelist = wlist)
model3c
graphviz.plot(model3c)

model4c = si.hiton.pc(dataForModel,undirected = F,whitelist = wlist)
model4c
graphviz.plot(model4c)

#Hybrid

model1h = mmhc(dataForModel)
model1h
graphviz.plot(model1h)
score(model1h,dataForModel) #-304.765

model2h = rsmax2(dataForModel)
graphviz.plot(model2h)
score(model2h,dataForModel) #-304.765



set.seed(1)
bn.cv(dataForModel, bn = "hc") #1.299538
set.seed(1)
bn.cv(d2, bn = "gs") #1.85762
set.seed(1)
bn.cv(dataForModel, bn = "iamb") #2.096448
set.seed(1)
bn.cv(dataForModel, bn = "mmpc",algorithm.args = list(whitelist = wlist)) 
set.seed(1)
bn.cv(dataForModel, bn = "si.hiton.pc",algorithm.args = list(whitelist = wlist))
set.seed(1)
bn.cv(dataForModel, bn = "mmhc") #1.89195
set.seed(1)
bn.cv(dataForModel, bn = "rsmax2") #1.883792

#Model Selection => Model8

# Bootstraping
arcs = boot.strength(dataForModel, R=50,algorithm = "hc",cpdag=F)
arcs<-arcs[(arcs$strength > 0.8) & (arcs$direction >= 0.5), ]
graphviz.plot(averaged.network(arcs),layout="fdp")
score(averaged.network(arcs),dataForModel) #173.190

#Parameter Learning
parameters = bn.fit(model8,dataForModel)


cpquery(parameters,event = (bbox == "success"),evidence = (IsAmazon == 1),method = "ls",n=10^6) #0.9256728


parameters2 = bn.fit(averaged.network(arcs),dataForModel)
cpquery(parameters2,event = (bbox == "success"),evidence = (IsAmazon == 1),method = "ls",n=10^6) #0.9246152

#Prediction

testData = as.data.frame(readRDS("amz_test.rds"))

head(testData,2)

head(dataForModel,2)

testData$PoS = testData$sid_rating/testData$price
testData$sid_pos_fbIndicator = as.numeric(testData$sid_pos_fb >= 7.5)
testData$sRatingPos = testData$sid_rating * testData$sid_pos_fb
testData$IsBestPrice = c(rep(0,dim(testData)[1]))
library(data.table)
testData = data.table(testData)
pidEpocGroup = testData[,.SD[which.min(price)],by=c("pid","epoc")]
pidEpocGroup$IsBestPrice = rep(1,length(pidEpocGroup$IsBestPrice))
testData = testData[pidEpocGroup, on=.(pid,epoc,sid), c("IsBestPrice"):=.(i.IsBestPrice)][]
length(testData[testData$IsBestPrice==1,]$IsBestPrice)
head(testData,2)

testData =testData[,-c(1,2,3,4,6,9,10,15,17)]
testData$sid_pos_fbIndicator = factor(testData$sid_pos_fbIndicator)
testData$IsBestPrice = factor(testData$IsBestPrice)

testData = na.omit(testData)
testData$IsShipping = factor(as.numeric(testData$shipping >0))
#testData = testData[,-c(2)]
testData$sid_rating = factor(as.numeric(testData$sid_rating >= 4))
testData$IsAmazon = factor(as.numeric(testData$sid_rating_cnt == 100))
testData = testData[,-c(2,3)]
head(dataForModel,2)
head(testData,2)
summary(testData)



testDataFull = as.data.frame(readRDS("amz_test_full.rds"))

head(testDataFull[testData$IsAmazon == 1,],2)
head(testData[testData$IsAmazon == 1,],3)

succesOrFailure = numeric()
for(i in 1:dim(testData)){
  probSuccess = cpquery(parameters,event = (bbox == "success"),evidence = (sid_rating == testData[i,1]& is_fba == testData[i,4]& is_prime == testData[i,5]& sid_pos_fbIndicator == testData[i,6]& IsBestPrice == testData[i,7]& IsShipping == testData[i,8] & IsAmazon == testData[i,9]),method = "ls",n=10^6) 
  if (probSuccess >= 0.5){
    succesOrFailure[i] = "success"
  }else{
    succesOrFailure[i] = "failure"
  }
}


testDataFull$prediction = succesOrFailure
testDataFull$SameorNot = rep(0,dim(testDataFull)[1])
for(i in 1:dim(testData)[1]){
  if (testDataFull[i,]$bbox == testDataFull[i,]$prediction){
    testDataFull[i,]$SameorNot = 1
  }
}
sum(testDataFull$SameorNot)/dim(testDataFull)[1] #93.31% of the predictions are correct
sum(testDataFull[testDataFull$sid == "amazon",]$SameorNot)/dim(testDataFull[testDataFull$sid == "amazon",])[1] #60% finding true for amazon
sum(testDataFull[testDataFull$bbox_sid == "amazon",]$SameorNot)/dim(testDataFull[testDataFull$bbox_sid == "amazon",])[1] #99.34 % finding whether the amazon will win or not.
sum(testDataFull[testDataFull$sid != "amazon" & testDataFull$bbox == "success",]$SameorNot)/dim(testDataFull[testDataFull$sid != "amazon" & testDataFull$bbox == "success",])[1] #11.53% of prediction of non-amazon sellers.



