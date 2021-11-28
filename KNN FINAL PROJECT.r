options(scipen=999)
library(gmodels)
library(class)
K=100

mydata=read.csv("C:/Users/cchoi2/Downloads/datawithwordcount (1).csv")
mydata$overview=NULL
str(mydata)

mydata$type=NULL
#mydata$num.beds=NULL
#mydata$num.baths=NULL
mydata$living.area=NULL
mydata$lot.area=NULL
mydata$rand=NULL
mydata$masterbedroom.area=NULL
mydata$kitchen.area=NULL
mydata$year_cat=NULL
mydata$rural.code=NULL
mydata$pet.friendly=NULL
mydata$convenient=NULL
#mydata$downtown=NULL
mydata$University=NULL
mydata$Starbucks=NULL
mydata$food=NULL
mydata$restaurant=NULL
mydata$shopping=NULL
mydata$fitness=NULL
mydata$train=NULL
mydata$subway=NULL
mydata$bus=NULL
mydata$airport=NULL
mydata$T.station=NULL
#mydata$fireplace=NULL
mydata$laundry.room=NULL
mydata$balcony=NULL
mydata$natural.light=NULL
mydata$hardwood.floor=NULL
#mydata$garage=NULL
mydata$high.ceiling=NULL
mydata$walk.in.closet=NULL
mydata$rare=NULL
mydata$affordable=NULL
mydata$spectscular=NULL
mydata$stunning.view=NULL
mydata$newly.renovated=NULL
mydata$luxurious=NULL
str(mydata)
summary(mydata$myresponse)
mydata$myresponse_factor <- cut(x = mydata$myresponse, breaks = c(159999, 700000,1000000, 2000000, 6700001))

levels(mydata$myresponse_factor) <- c("1", "2", "3","4")
mydata$myresponse_factor
str(mydata)
mydata$year.built=NULL
str(mydata)
mydata$rural.code=NULL


mydata$masterbedroom.area=NULL
mydata$masterbedroom.length=NULL
mydata$masterbedroom.width=NULL
mydata$living.area=NULL
mydata$kitchen.length=NULL
mydata$kitchen.width=NULL
mydata$myresponse=NULL
str(mydata)
summary(mydata)

raw_for_export=mydata


#START OF VARIABLE STANDARDIZATION

col_nums=c(1,7); #Substitute (1,2,3,4) with the possitions at which columns that


cols_for_standard=as.matrix(mydata[,col_nums])
standardized=scale(cols_for_standard)
all_col_nums=c(1:length(names(mydata)))
remaining_cols=as.vector(all_col_nums[is.na(pmatch(all_col_nums, col_nums))])
remaining_data=subset(mydata,select=remaining_cols)
mydata=cbind(remaining_data, standardized)

#END OF VARIABLE STANDARDIZATION



#START DATA BREAKDOWN FOR HOLDOUT METHOD

nobs=dim(mydata)[1]
RNGkind(sample.kind = "Rounding")
set.seed(1) #sets the seed for random sampling

prop = prop.table(table(mydata$myresponse_factor))
length.vector = round(0.8*nobs*prop)
train_size=sum(length.vector)
test_size=nobs-train_size
class.names = as.data.frame(prop)[,1]
numb.class = length(class.names)
train_index = c()

for(i in 1:numb.class){
  index_temp = which(mydata$myresponse_factor==class.names[i])
  train_index_temp = sample(index_temp, length.vector[i], replace = F)
  train_index = c(train_index, train_index_temp)
}


train=mydata[train_index,] #randomly select the data for training set using the row numbers generated above
test=mydata[-train_index,]#everything not in the training set should go into testing set

y_train=train$myresponse_factor
y_test=as.data.frame(test$myresponse_factor)


pred_train=train
pred_test=test
pred_train$myresponse_factor=NULL
pred_test$myresponse_factor=NULL


dim(pred_train) #confirms that training data has only 80% of observations
dim(pred_test) #confirms that testing data has 20% of observations

#END DATA BREAKDOWN FOR HOLDOUT METHOD


#START ONE-LEAVE-OUT CROSS VALIDATION TO FIND THE 'BEST' k VALUE  

cl=train$myresponse_factor
percent_correct=c()

for (i in 1:K){
  set.seed(1)  
  one.run=knn.cv(pred_train, cl, k=i)
  one.run.df=data.frame(one.run,cl)
  corr.cl.rate=sum(one.run.df$one.run==one.run.df$cl)/dim(pred_train)[1]
  percent_correct=c(percent_correct, corr.cl.rate)}

best.k=which.max(percent_correct)
plot(percent_correct, type="l", main="Correct Classification Rate Vs k", xlab="Value of k", ylab="Correct Classification Rate")
axis(at=best.k, side=1)
abline(v=best.k, lty=3)

#END ONE-LEAVE-OUT CROSS VALIDATION TO FIND THE 'BEST' k VALUE  



percent_correct=100*sum(as.character(nearest_final)==as.character(y_test[,1]))/dim(y_test)[1]

#END FINAL KNN RUN FOR THE BEST VALUE OF K


#START: ACCURACY FOR PREDICTING THE TESTING SET 
print(paste("Percentage of Correct Classifications for 'Best' KNN is:",percent_correct, "percent")) 

colnames(y_test)="myresponse"
table_for_export=cbind(raw_for_export[-train_index,],nearest_final)
table_for_export$knn_classification=table_for_export$nearest_final
table_for_export$nearest_final=NULL


#Getting the confusion matrix below  
Confusion_Matrix = CrossTable(table_for_export$myresponse_factor, table_for_export$knn_classification,dnn=c("True Class","Predicted Class"), prop.chisq=F,prop.t=F, prop.c=F, prop.r=F)

#END: ACCURACY FOR PREDICTING THE TESTING SET  
