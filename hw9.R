# HW 9
library(readr)
library(dplyr)
library(stringr)
library(timeDate)
library(lubridate)
library(highcharter)
library(ngram)
library(graphics)
library(ggplot2)
# Q1
path = list.files("/Users/Apple/Documents/TaraFiles/University/term 8/Data Analysis/week_9/class_data/stock_dfs/",
                      full.names = T)
companyNames = list.files("/Users/Apple/Documents/TaraFiles/University/term 8/Data Analysis/week_9/class_data/stock_dfs/") %>% 
  str_replace(".csv","")
section = read_csv("/Users/Apple/Documents/TaraFiles/University/term 8/Data Analysis/week_9/class_data/constituents.csv")

StockData = read_csv(path[1]) %>% mutate(company = companyNames[1])
for (i in 2:length(path)){
  x = read_csv(path[i]) %>% mutate(company = companyNames[i])
  StockData <- rbind(StockData,x)                
}

#1 year
year_1= StockData["Date"]+365
names(year_1)="DateEnd"
year_1$company = StockData$company
year_1$DateStart = StockData$Date
year_1$priceStart = StockData$Close

year_1_full = left_join(StockData %>% select(DateEnd=Date,company,priceEnd=Close),year_1,by=c("DateEnd","company"))

year_1_full %>% mutate(percentOFreturn = (priceEnd-priceStart)/priceStart) %>% 
  arrange(-percentOFreturn)%>% group_by(company) %>% top_n(1,percentOFreturn) %>%
  arrange(-percentOFreturn)->year_1_summarised


year_1_section=na.omit(right_join(year_1_summarised,section,by=c("company"="Symbol")))%>%arrange(-percentOFreturn)
head(year_1_section)

hchart(year_1_section[1:10,], "column", hcaes(x = company, y = percentOFreturn, group = Sector))

# 2 year
year_2= StockData["Date"]+365*2
names(year_2)="DateEnd"
year_2$company = StockData$company
year_2$DateStart = StockData$Date
year_2$priceStart = StockData$Close

year_2_full = left_join(StockData %>% select(DateEnd=Date,company,priceEnd=Close),year_2,by=c("DateEnd","company"))

year_2_full %>% mutate(percentOFreturn = (priceEnd-priceStart)/priceStart) %>% 
  arrange(-percentOFreturn)%>% group_by(company) %>% top_n(1,percentOFreturn) %>%
  arrange(-percentOFreturn)->year_2_summarised

year_2_section=na.omit(right_join(year_2_summarised,section,by=c("company"="Symbol")))
head(year_2_section)

hchart(year_2_section[1:10,], "column", hcaes(x = company, y = percentOFreturn, group = Sector))


# 5 year
year_5= StockData["Date"]+365*5+1
names(year_5)="DateEnd"
year_5$company = StockData$company
year_5$DateStart = StockData$Date
year_5$priceStart = StockData$Close

year_5_full = left_join(StockData %>% select(DateEnd=Date,company,priceEnd=Close),year_5,by=c("DateEnd","company"))

year_5_full %>% mutate(percentOFreturn = (priceEnd-priceStart)/priceStart) %>% 
  arrange(-percentOFreturn)%>% group_by(company) %>% top_n(1,percentOFreturn) %>%
  arrange(-percentOFreturn)->year_5_summarised

year_5_section=na.omit(right_join(year_5_summarised,section,by=c("company"="Symbol")))
head(year_5_section)

hchart(year_5_section[1:10,], "column", hcaes(x = company, y = percentOFreturn, group = Sector))





### Q2

tomorrow_1= StockData["Date"]+1
names(tomorrow_1)="DateEnd"
tomorrow_1$company = StockData$company
tomorrow_1$DateStart = StockData$Date
tomorrow_1$priceStart = StockData$Close

tomorrow_1_full = left_join(StockData %>% select(DateEnd=Date,company,priceEnd=Close),tomorrow_1,by=c("DateEnd","company"))
tomorrow_1_full%>%filter(day(DateEnd)==14)%>%na.omit()%>%
  mutate(difference=(priceEnd-priceStart)/priceStart)->tomorrow_1_full

hchart(density(tomorrow_1_full$difference), type = "area", name = "difference")%>%
  hc_title(text = "13 !") %>%  hc_add_theme(hc_theme_sandsignika())


t.test(tomorrow_1_full$difference,alternative = c("greater")) 
t.test(tomorrow_1_full$difference,alternative = c("less")) 

# Q3

StockData%>%mutate(maxChange=High-Low)%>%group_by(Date)%>%
  summarise(total_trade=sum(maxChange))%>% 
  arrange(-total_trade)%>%.[1:2,]


# Q4

AAPL = read_csv("/Users/Apple/Documents/TaraFiles/University/term 8/Data Analysis/week_9/class_data/stock_dfs/AAPL.csv")
open_apple = AAPL$Open

K_list = 2:10
errors = array(K_list)

for (k in K_list) {
  my_data = as.data.frame(open_apple)
  for (i in 1:k) {
    name = paste("day", i, sep = '')
    old_names = colnames(my_data)
    my_data %>% cbind(lead(open_apple, n = i)) -> my_data
    colnames(my_data) = c(old_names, name)
  }
  fmla <- as.formula(paste("open_apple ~ ", paste(colnames(my_data)[-1], collapse= "+")))
  fit=lm(fmla,my_data)
  errors[k] = summary(fit)$sigma
  
}
K_list[which(errors == min(errors[-1]))]


# Q5
nam=companyNames[1]
pcaData = read_csv(path[1])%>%select(Date,Open)
colnames(pcaData)=c("Date",nam)
for (i in 2:length(companyNames)) {
  nam=companyNames[i]
  x = read_csv(path[i])%>%select(Date,Open)
  colnames(x)=c("Date",nam)
  pcaData=left_join(pcaData,x,by="Date")%>%na.omit()
}
pcaData=pcaData[,2:126]
pca = prcomp(pcaData, center=T, scale.=T)
#round(pca$sdev^2, 2)
pcs = data.frame(pca$x)
eigv = round(pca$sdev^2/sum(pca$sdev^2)*100, 2)
eigv = data.frame(c(1:125 ),eigv)
names(eigv) = c('PCs','Variance')
plot(eigv[1:40,],type = "b",col = "darkblue",lwd = 2);grid()
PCA = pca$sdev^2
names(PCA) = paste0('PC', eigv$PCs)
qcc::pareto.chart(PCA[1:40])
percent=sum(eigv[1:3,2])/sum(eigv[,2])
abline(h=percent,col="red")


# Q6

dataQ6=right_join(StockData%>%select(Date,Open,company),
           section%>%select(company=Symbol,Sector),by=c("company"))
dataQ6%>%select(Sector,Date,Open)%>%group_by(Sector,Date)%>%
  summarise(meanOpen=mean(Open,na.rm = T))->Q6result

sectorNames=unique(section$Sector)
nam=sectorNames[1]
pcaDataQ6 = Q6result%>%filter(Sector==nam)%>%select(Date,meanOpen)
pcaDataQ6=pcaDataQ6[,2:3]
colnames(pcaDataQ6)=c("Date",nam)
for (i in 2:length(sectorNames)) {
  nam=sectorNames[i]
  x = Q6result%>%filter(Sector==nam)%>%select(Date,meanOpen)
  x=x[,2:3]
  colnames(x)=c("Date",nam)
  pcaDataQ6=left_join(pcaDataQ6,x,by="Date")%>%na.omit()
}


Indexes = read_csv("/Users/Apple/Documents/TaraFiles/University/term 8/Data Analysis/week_9/class_data/indexes.csv")
right_join(pcaDataQ6,Indexes,by="Date")%>%na.omit()->pcaDataQ6
pcaDataQ6=pcaDataQ6[,2:21]
pca6 = prcomp(pcaDataQ6, center=T, scale.=T)
pcs6 = data.frame(pca6$x)
eigv6 = round(pca6$sdev^2/sum(pca6$sdev^2)*100, 2)
eigv6 = data.frame(c(1:20 ),eigv6)
names(eigv6) = c('PCs','Variance')
plot(eigv6,type = "b",col = "darkblue",lwd = 2);grid()
PCA6 = pca6$sdev^2
names(PCA6) = paste0('PC', eigv6$PCs)
qcc::pareto.chart(PCA6)

library(ggbiplot)

ggbiplot(pca6, obs.scale = 1, var.scale = 1)




# Q7

AAPL[,2:7]->AAPL_7
numTrain=round(0.8*nrow(AAPL_7))
pcaAPPL = prcomp(AAPL_7[1:numTrain,], center=T, scale.=T)
pcsAPPL = data.frame(pcaAPPL$x)
eigvAPPL = round(pcaAPPL$sdev^2/sum(pcaAPPL$sdev^2)*100, 2)
eigvAPPL = data.frame(c(1:6 ),eigvAPPL)
names(eigvAPPL) = c('PCs','Variance')
plot(eigvAPPL,type = "b",col = "darkblue",lwd = 2);grid()
PCA_APPL = pcaAPPL$sdev^2
names(PCA_APPL) = paste0('PC', eigvAPPL$PCs)
qcc::pareto.chart(PCA_APPL)

data_apple = data.frame(feature = pcaAPPL$x[1:(numTrain-1),1],label = AAPL_7$Open[2:numTrain])
model = lm(label~feature,data=data_apple)
summary(model)

data_apple$predict = predict(model,data_apple)
ggplot(data = data_apple) + geom_point(aes(x = label , y = predict),
                                       size = 1 , color = "darkolivegreen3") +
  geom_smooth(aes(x = label , y = predict), formula=y~x, colour = "slateblue4",
              fill = "aquamarine") +
  geom_abline(slope = 1, intercept = 0, color = "firebrick1",size =1)


errors_7= summary(model)$sigma
errors_7

# Q8

d=diff(Indexes$SP500)
d=d[which(d>-50)]
d=d[which(d<50)]
hchart(density(d) , type = "area", name = "difference")%>%
  hc_title(text = "SP500 profit density plot") %>%  hc_add_theme(hc_theme_sandsignika())




# 9

library(ggbiplot)
library("EBImage")
pic = flip(readImage("/Users/Apple/Documents/TaraFiles/University/term 8/Data Analysis/week_9/hw_09/stock.jpg"))
red.weigth   = .2989; green.weigth = .587; blue.weigth  = 0.114
img = red.weigth * imageData(pic)[,,1] +
  green.weigth * imageData(pic)[,,2] + blue.weigth  * imageData(pic)[,,3]
image(img, col = grey(seq(0, 1, length = 256)))
pca.img = prcomp(img, scale=TRUE)
# Let's plot the cumulative variance of all 349 components
plot(summary(pca.img)$importance[3,], type="l",
     ylab="%variance explained", xlab="nth component (decreasing order)")
# to capture 99% of the variance, we need the first 32 components
abline(h=0.99,col="red");abline(v = 110,col="red",lty=3)
chosen.components = 1:110
feature.vector = pca.img$rotation[,chosen.components]
compact.data = t(feature.vector) %*% t(img)
#dim(compact.data)
approx.img = t(feature.vector %*% compact.data) 
#dim(approx.img)
imageData(pic)[,,3]=approx.img
#
plot(flip(pic))
title("initial picture")

image(approx.img, col = grey(seq(0, 1, length = 256)))
title(paste(110 ,' of PCA columns in compacted picture',sep=''))


chosen.components = 1:50
feature.vector = pca.img$rotation[,chosen.components]
compact.data = t(feature.vector) %*% t(img)
approx.img = t(feature.vector %*% compact.data) 
image(approx.img, col = grey(seq(0, 1, length = 256)))
title(paste(50 ,' of PCA columns in compacted picture',sep=''))

l=(1:100)*3
size_9=as.data.frame(as.integer(object.size(pic)))
colnames(size_9)="size"
for (i in l) {
  chosen.components = 1:i
  feature.vector = pca.img$rotation[,chosen.components]
  compact.data = t(feature.vector) %*% t(img)
  size_i=as.data.frame(as.integer(object.size(feature.vector))+as.integer(object.size(compact.data)))
  colnames(size_i)="size"
  size_9=rbind(size_9,size_i)
}

size_9=size_9[-1,]
size_9=cbind(size_9,as.data.frame(l))
names(size_9)=c("size","number_of_PC")

ggplot(data = size_9) + geom_point(aes(x = number_of_PC , y = size),
                                       size = 1 , color = "darkolivegreen3") 

hchart(size_9, "column", hcaes(x = number_of_PC , y = size))




