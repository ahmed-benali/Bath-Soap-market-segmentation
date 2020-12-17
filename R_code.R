library(dummies)
library(class)
library(klaR)
library(SDMTools)
library(readxl)
library(factoextra) 
library(ggplot2)
library(cluster)
library(clValid)
library(fpc)


#Reading the the data set from the Excel File
BathSoap <- read_excel("C:\\Users\\araba\\Desktop\\BathSoap.xls",sheet = "DM_Sheet", skip = 1)


#......................Purchase Beahviour Data Preprocessing..............................#


# susceptibility to 3 types of discounts
discounts <- BathSoap[20:22]

#get the max prop-volume %
maxprop<- c()
for (i in 1:nrow(BathSoap)){
  maxprop[i] <- max(BathSoap[i,36:46])
}

#get the prop number corresponding to the the max prop-volume %
maxpropnum<- c() 
for (i in 1:nrow(BathSoap)){
  if(maxprop==BathSoap[i,36]){maxpropnum[i] <-5}
  else if(maxprop==BathSoap[i,37]){maxpropnum[i] <-6}
  else if(maxprop==BathSoap[i,38]){maxpropnum[i] <-7}
  else if(maxprop==BathSoap[i,39]){maxpropnum[i] <-8}
  else if(maxprop==BathSoap[i,40]){maxpropnum[i] <-9}
  else if(maxprop==BathSoap[i,41]){maxpropnum[i] <-10}
  else if(maxprop==BathSoap[i,42]){maxpropnum[i] <-11}
  else if(maxprop==BathSoap[i,43]){maxpropnum[i] <-12}
  else if(maxprop==BathSoap[i,44]){maxpropnum[i] <-13}
  else if(maxprop==BathSoap[i,45]){maxpropnum[i] <-14}
  else {maxpropnum[i] <-15}
}

#Get the Percentage if a customer is "loyal" to a distinct Brand 
max.brand<- c()
for (i in 1:nrow(BathSoap)){
  max.brand[i] <- max(BathSoap[i,23:30])
}
loyalty<-max.brand



#''''''''''''''''''''''''''''''''''Purchase Behaviour''''''''''''''''''''''''''#
purchase.behaviour <-cbind(BathSoap[,c(12:16,19,31)],loyalty)


#''''''''''''''''''''''''''''''''''Basis For Purchase''''''''''''''''''''''''''#
basis.purchase <- cbind(maxprop,maxpropnum,discounts,BathSoap[,32:35])


#'''''''''''''''''''''purchase beahviour and basis for purchasing scaling''''''''''''''#
both <- cbind(BathSoap[,c(12,13,15,16,31)],loyalty,basis.purchase)




##### SCALING ####### 
scaled.behaviour <- scale(purchase.behaviour)
scaled.basis <- scale(basis.purchase)
scaled.both <- scale(both)



################### 1)clustering Households with k-means:

# a. purchase behaviour kmeans wss plot
fviz_nbclust(scaled.behaviour, kmeans, method = "wss" )

# b. basis for purchase kmeans wss plot
fviz_nbclust(scaled.basis, kmeans, method = "wss")

# c. both purchase beahviour and basis for purchase wss plot
fviz_nbclust(scaled.both, kmeans, method = "wss")

#### ======> k=4 is the best value of k using the method of wss line Elbow


#comparing the kmeansclustering with the three different datasets with k = 4

km1 <- kmeans(scaled.behaviour, 4)
km1$tot.withinss


km2<- kmeans(scaled.basis,4)
km2$tot.withinss

km3 <- kmeans(scaled.both,4)
km3$tot.withinss



table(km1$cluster)
table(km2$cluster)
table(km3$cluster)

#plotting the 4 different clusters

plotcluster(scaled.behaviour, km1$cluster,main ="purchase behaviour clustering ")
clusplot(scaled.behaviour, km1$cluster ,main ="purchase behaviour clustering " )

# Gap between clusters
dunn(dist(scaled.behaviour,method = "euclidean"), km1$cluster , method = "euclidean")
dunn(dist(scaled.basis,method = "euclidean"), km2$cluster , method = "euclidean")
dunn(dist(scaled.both,method = "euclidean"), km3$cluster , method = "euclidean")


#'''''''''''''''''''''''''' Demographic characteristics Dummy variables Transformation ''''''''''''''''''''''''''#

data <- BathSoap

#SEC transformation
dum.SEC <- dummy(data$SEC)
colnames(dum.SEC)<-c("class_A","class_B","class_C","class_D/E")


#Eating Habit transformation
dum.FEH <- dummy(data$FEH)
dum.FEH <- dum.FEH[,-1]
colnames(dum.FEH)<-c("VEG","VEG&EGG","NON_VEG")


##Language  transformation
dum.MT<- dummy(data$MT)
dum.MT <- dum.MT[,-1]
colnames(dum.MT)<-c("MT.English","MT.Gujarati","MT.Hindi","MT.Kannada","MT.Konkani",
                    "MT.Malayalam","MT.Marathi","MT.Punjabi","MT.Rajasthani","MT.Sindhi","MT.Tamil",
                    "MT.Telugu","MT.Urdu","MT.others")


#sex transformation 
dum.SEX<- dummy(data$SEX)
dum.SEX <- dum.SEX[,-1]
colnames(dum.SEX)<-c("Male","Female")


#Age transformation 
dum.AGE<- dummy(data$AGE)
colnames(dum.AGE)<-c("<24","25=>34","35=>44",">45")


#EDU transformation 
dum.EDU<- dummy(data$EDU)
dum.EDU <- dum.EDU[,-1]
colnames(dum.EDU) <-c("ilt","ltr","4y","5-9","10-12","s.col","grad","s.grad","G&P.sch")

#CHILD transformation 
dum.CHILD<- dummy(data$CHILD)
dum.CHILD <- dum.CHILD[,-5]
colnames(dum.CHILD)<-c(" below6","[7,14]","both","none")

#Cable television transformation 
dum.CS<- dummy(data$CS)
dum.CS <- dum.CS[,2]

##############################Characteristics  of the clusters###################################


#Demographics Dataset
Demographics <- cbind(dum.SEC, dum.FEH, dum.MT, dum.SEX, data[,6], dum.EDU, data[,8], 
                      dum.CHILD, dum.CS, data[,11])


#Demographics, Basis for Purchase and Brand Loyalty Dataset
seg.data <- cbind(Demographics,both)

#Descriptives of the 4 clusters
a<-aggregate(seg.data, by=list(km1$cluster) , FUN=mean )
a
as.matrix(a$maxpropnum)
as.matrix(a$maxprop)

#Affluence index along the 4 different segments
boxplot(BathSoap$`Affluence Index`~ km3$cluster , main="Affluence index for each cluster")

#naming the groups
a$Group.1<-c("cluster1","cluster2","cluster3","cluster4")

#Social Economic Class along the 4 different segments
table<-t(as.matrix(a[,2:5]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,2:5]),col = 1:4, main ="SEC")

# Eating Habit along the 4 different segments
table<-t(as.matrix(a[,6:8]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,6:8]),col = 1:3, main ="FEH")

#Language along the 4 different segments
table<-t(as.matrix(a[,9:22]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,9:22]),col = 1:14, main ="MT")

#SEX along the 4 different segments
table<-t(as.matrix(a[,23:24]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,23:24]),col = 1:2, main ="SEX")

#Age group along the 4 different segments
table<-t(as.matrix(a[,25]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,25]),col = 2, main ="AGE")

# Education level along the 4 different segments
table<-t(as.matrix(a[,26:34]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,26:34]),col = 1:9, main ="EDU")

#Houshold size along the 4 different segments
table<-t(as.matrix(a[,35]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,35]),col = 3, main ="HS")


# number of children along the 4 different segments
table<-t(as.matrix(a[,36:39]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,36:39]),col = 1:4, main ="CHILD")

# Having cable TV or not along the 4 different segments
table<-t(as.matrix(a[,40]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,40]),col = 2, main ="CS")

#Affluence index along the 4 different segments
boxplot(BathSoap$`Affluence Index`~ km3$cluster , main="Affluence index for each cluster")
table<-as.matrix(b[,37:47])
table

#Brand Loyalty along the 4 different segments 
table<-t(as.matrix(a[,46:47]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,46:47]),col = 1:2, main ="loyalty")


#basis for puchase along the 4 different segments
table<-t(as.matrix(a[,c(48,53,54,55,56)]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(a[,c(48,53,54,55,56)]),col = 1:5, main ="basis of purchase")


#selling proposition along the 4 different segments

b<-aggregate(BathSoap, by=list(km1$cluster) , FUN=mean )
table<-as.matrix(b[,37:47])
table<-t(as.matrix(b[,37:47]))
colnames(table)<-a$Group.1
barplot(table,legend=colnames(b[,37:47]),col = 1:11, args.legend = list(x = 5.4, bty='l'),xpd = TRUE,main ="selling proposition")
boxplot(BathSoap$`Affluence Index`~ km3$cluster , main="Affluence index for each cluster")
table<-as.matrix(b[,37:47])
table



##############Predictive Model########################## 


#the training and the validation sample
set.seed(123)
cluster_data <- cbind(both,km1$cluster)
training<-sample(1:nrow(cluster_data),floor(0.6*nrow(cluster_data)))
train<-cluster_data[training,]
test<-cluster_data[-training,]


#building the predictive model
set.seed(123)

train.class=cluster_data$`km1$cluster`[training]
test.class=cluster_data$`km1$cluster`[-training]
pred=knn(train,test,train.class,k=23)



#checking reliability of the model
table(pred,test.class)
mean(pred==test.class)

#customers that direct mailing would be "success"
BathSoap[cluster_data$`km1$cluster`==3,]
