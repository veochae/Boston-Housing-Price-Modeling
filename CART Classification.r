options(scipen=999)

load("C:/Users/ttarasansombat1/Documents/QTM3 - Copy/MyRStudio/Rdata/realtor.spring.2020.rda")

#rephraming the dependent variable
mydata=realtor.spring.2020
realtor.spring.2020=NULL

str(mydata)

#this code is creating the AREA of the master bedroom
mydata$masterbedroom.area=mydata$masterbedroom.length*mydata$masterbedroom.width

#this code is getting the living area of the house without the masterbedroom
mydata$areanomaster=mydata$living.area-mydata$masterbedroom.area

#data clensing
summary(mydata)
str(mydata)

  #we are getting rid of overview
mydata$overview=NULL

  #type are in factors
mydata$type=as.factor(mydata$type)

  #rand is a dummy variable, therefore NULLed
mydata$rand=NULL

  #kitchen area
mydata$kitchen.area=mydata$kitchen.length*mydata$kitchen.width

  #changing the year.built into a category of a range
  #there is an underlying code that prevents us from turning factors into as.numeric.
  #Thus, I changed the year.built into a character, then changed it into numeric values, stored in year_cat
  #then, the cut function is creating a range: you can read it as between 1725 and 1750 and between 1750 and 1800 and so on
  #Then, the created range has been turned into a categorical variable (as.factor)
  #If you want to use the original year.built, you can just undo the NULL :)
mydata$year.built=as.numeric(as.character(mydata$year.built))
mydata$year_cat=cut(mydata$year.built,br=c(1725,1735,1745,1755,1765,1775,1785,1795,1805,1815,1825,1835,1845,1855,1865,1875,1885,1895,1905,1915,1925,1935,1945,1955,1965,1975,1985,1995,2005,2015,2019))
mydata$year_cat=as.factor(mydata$year_cat)
mydata$year.built=NULL
str(mydata)
#getting rid of datarows with sold price of N/A
mydata$sold.price[is.na(mydata$sold.price)]=0
mydata=subset(mydata,mydata$sold.price>0)

#replacing the N/A to 0 for the variable column "lot size"
mydata$lot.area[is.na(mydata$lot.area)] = "0"

#getting rid of repeated variables
mydata$masterbedroom.length=NULL
mydata$masterbedroom.width=NULL
mydata$kitchen.length=NULL
mydata$kitchen.width=NULL

#importing the zipcode data
z=read.csv("C:/Users/ttarasansombat1/Documents/QTM3 - Copy/MyRStudio/RawData/rural-demo.csv")
z$ZIPCODE=paste0("0",z$Zip)
ruralcode=data.frame(z$ZIPCODE,z$RuralCode)
summary(mydata)
names(ruralcode)[1]="zip.code"
names(ruralcode)[2]="rural.code"

str(ruralcode)
  #left-joining mydata and ruralcode by zipcode
joindedata=merge(mydata,ruralcode,by="zip.code", all.x=T)
  #setting my data equal to the joindedata
mydata=joindedata
joindedata=NULL
ruralcode=NULL
z=NULL

str(mydata)

  #Getting rid of the Zipcode
mydata$zip.code=NULL
str(mydata)
mydata$myresponse=mydata$sold.price
mydata$sold.price=NULL
str(mydata)
mydata$lot.area=as.numeric(mydata$lot.area)
str(mydata)

mydata$response_factor=cut(mydata$myresponse,br=c(159999,378000,487000,596000,814000,1468000,2122000,2776000,3430000,4084000,4738000,5392000,6046000))
mydata$myresponse=NULL

summary(mydata$response_factor)
#CART
library(tree)
library(gmodels)
library(ftsa)

#setup

tree_type="C"

#Enter the minimum number of items in each leaf

min_leaf_size=30

#Enter the minimum deviance for a node to be considered for a further split to avoid overfitting

min_deviance=0

numpredictors=dim(mydata)[2]-1

numfac=0

for (i in 1:numpredictors) {
  if ((is.factor(mydata[,i]))){
    numfac=numfac+1} 
}

#End finding the number of categorical predictors 

nobs=dim(mydata)[1]



if (tree_type=="R") {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Regression Tree
  
  train_size=floor(0.8*nobs)
  test_size=nobs-train_size
  
} else {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Classification Tree
  
  prop = prop.table(table(mydata$myresponse_factor))
  length.vector = round(nobs*0.8*prop)
  train_size=sum(length.vector)
  test_size=nobs-train_size
  class.names = as.data.frame(prop)[,1]
  numb.class = length(class.names)}


resample=1
RNGkind(sample.kind = "Rounding")
set.seed(1) #sets the seed for random sampling

while (resample==1) {
  
  
  if (tree_type=="C") {
    
    train_index = c()
    
    for(i in 1:numb.class){
      index_temp = which(mydata$myresponse_factor==class.names[i])
      train_index_temp = sample(index_temp, length.vector[i], replace = F)
      train_index = c(train_index, train_index_temp)
    }} else {
      train_index=sample(nobs,train_size, replace=F)
    }
  
  mydata_train=mydata[train_index,] #randomly select the data for training set using the row numbers generated above
  mydata_test=mydata[-train_index,]#everything not in the training set should go into testing set
  
  right_fac=0 #denotes the number of factors with "right" distributions (i.e. - the unique levels match across mydata, test, and train data sets)
  
  
  for (i in 1:numpredictors) {
    if (is.factor(mydata_train[,i])) {
      if (sum(as.vector(unique(mydata_test[,i])) %in% as.vector(unique(mydata_train[,i])))==length(unique(mydata_test[,i])))
        right_fac=right_fac+1
    }
  }
  
  if (right_fac==numfac) (resample=0) else (resample=1)
  
}

dim(mydata_test) #confirms that testing data has only 20% of observations
dim(mydata_train) #confirms that training data has 80% of observations


#END DATA BREAKDOWN FOR HOLDOUT METHOD


#Start growing the reference tree
full_tree=tree(myresponse_factor ~ .,split="deviance",mindev=min_deviance, mincut=min_leaf_size,data=mydata_train)
#End growing the reference tree


#START 10-FOLD CROSS-VALIDATION TO FIND THE SIZE THAT THE FULL TREE WILL BE REDUCED TO

b_list=rep(1,100)

for (i in 1:100){
  
  set.seed(i)
  cv_tree=cv.tree(full_tree,K=10)
  cv_tree$size
  cv_tree$dev
  bestsize=min(cv_tree$size[cv_tree$dev==min(cv_tree$dev)])
  b_list[i]=bestsize
  #plot(cv_tree, type="p")
  
}

mytable=as.data.frame(table(b_list))
mytable_s=mytable[order(mytable$Freq),]
final_tree_size=as.numeric(paste(mytable_s[dim(mytable_s)[1],1]))
#END K-FOLD CROSS-VALIDATION TO FIND THE SIZE THAT THE FULL TREE WILL BE REDUCED TO


#START REDUCING THE FULL TREE TO OPTIMAL SIZE AND PLOTTING

bestcut=prune.tree(full_tree,best=final_tree_size)
plot(bestcut, type=c("uniform"))
text(bestcut, cex=0.6, digits = max(nchar(mydata$myresponse_factor))+3)

if (tree_type=="R"){
  print(bestcut, digits=max(nchar(mydata$myresponse_factor))+3)} else
    print(bestcut)



#END REDUCING THE FULL TREE TO OPTIMAL SIZE AND PLOTTING


#START PREDICTING THE RESPONSE IN THE TESTING SET (20 % SUBSET)

predicted=predict(bestcut,newdata=mydata_test, type="vector")

if (tree_type=="R") {
  id=as.numeric(names(predicted))
  temp_tbl=cbind(id,as.data.frame(predicted))
  mydata_test2=cbind(as.numeric(rownames(mydata_test)),mydata_test)
  colnames(mydata_test2)[1]="id"
  pred_table=merge(temp_tbl, mydata_test2, by.x="id", all.x=T)
  predicted=pred_table$predicted
  pred_table$predicted=NULL
  final_table=cbind(pred_table,predicted)
  final_table$id=NULL } else {
    
    predicted=as.data.frame(predicted)
    
    new.col = c()
    
    for(i in 1:(dim(predicted)[1])){
      
      find.max=which(predicted[i,]==max(predicted[i,]))
      
      #If there is a tie, assign a class randomly
      if (length(find.max)>1) {
        
        find.max=sample(find.max,1, replace=F)
        
      }
      
      new.col = c(new.col, names(predicted)[find.max])
    }
    
    
    predicted$predicted=new.col
    
    id=as.numeric(rownames(predicted))
    newdat=as.data.frame(id)
    newdat$predicted=predicted$predicted
    mydata_test2=cbind(as.numeric(rownames(mydata_test)),mydata_test)
    colnames(mydata_test2)[1]="id"
    final_table=merge(mydata_test2,newdat,by.x="id", all.x=T)}


#Measuring predictive accuracy below

if (tree_type=="R") {
  
  print(paste("MAE: ",error(forecast=final_table$predicted, true=final_table$myresponse_factor, method="mae"), sep=""))#returns the MAE of prediction
  print(paste("MAPE: ", error(forecast=final_table$predicted, true=final_table$myresponse_factor, method="mape"), sep=""))#returns the MAPE of prediction
  
  
} else {
  print("Confusion Matrix Is:")
  CrossTable(final_table$myresponse_factor,final_table$predicted,prop.chisq=F,prop.t=F) }

#END PREDICTING THE RESPONSE IN THE TESTING SET (20 % SUBSET)

#############################################################################################
##############################THIS IS THE END OF THE MACRO###################################
#############################################################################################



#START OF DATA IMPORT
mydata$sold.price=mydata$response_factor
hist(mydata$kitchen.area)
plot(mydata$kitchen.area,mydata$myresponse)
mydata$myresponse=mydata$sold.price
pairs(~sold.price+num.beds+num.baths,data=mydata,main="Matrix Plot of Sold Price, Number of Beds, and Number of Baths")
pairs(~myresponse+living.area+kitchen.area+lot.area+masterbedroom.area,data=mydata, main="Matrix Plot of Sold Price, Living Area, Kitchen Area, Lot Area, and Masterbedroom Area")

mydata$num.beds=as.factor(mydata$num.beds)
boxplot(myresponse~num.beds, data=mydata, outline=F)
mydata$num.baths=as.factor(mydata$num.baths)
boxplot(myresponse~num.baths, data=mydata, outline=F)
median(mydata$myresponse[mydata$type=="Single Family Home"])
median(mydata$myresponse[mydata$type=="Condo/Townhome/Row Home/Co-Op"])
