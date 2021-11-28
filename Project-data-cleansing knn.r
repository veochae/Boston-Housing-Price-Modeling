options(scipen=999)
library(gmodels)
library(class)

load("C:/Users/cchoi2/Downloads/realtor.spring.2020.rda")

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

  #type, number of bath and beds are in factors
#mydata$type=as.factor(mydata$type)

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
'
mydata$year.built=as.numeric(as.character(mydata$year.built))
mydata$year_cat=cut(mydata$year.built,br=c(1725,1735,1745,1755,1765,1775,1785,1795,1805,1815,1825,1835,1845,1855,1865,1875,1885,1895,1905,1915,1925,1935,1945,1955,1965,1975,1985,1995,2005,2015,2019))
mydata$year_cat=as.factor(mydata$year_cat)
mydata$year.built=NULL'

#getting rid of datarows with sold price of N/A
mydata$sold.price[is.na(mydata$sold.price)]=0
mydata=subset(mydata,mydata$sold.price>0)

#replacing the N/A to 0 for the variable column "lot size"
mydata$lot.area[is.na(mydata$lot.area)] = "0"

#importing the zipcode data
z=read.csv("C:/Users/cchoi2/Downloads/rural-demo.csv")
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
library(gmodels)
library(class)
K=100
mydata$myresponse=mydata$sold.price
mydata$sold.price=NULL
str(mydata)


summary(mydata$myresponse)
mydata$myresponse_factor <- cut(x = mydata$myresponse, breaks = c(160000, 596000, 1032000, 1468000, 1904000, 2340000, 2776000, 3212000, 3648000, 4084000, 4520000, 4956000, 5392000, 5828000, 6264000, 6700000))

levels(mydata$myresponse_factor) <- c("1", "2", "3", "4", "5","6","7","8","9","10","11","12","13","14","15")
mydata$myresponse_factor
str(mydata)
mydata$year.built=as.numeric(mydata$year.built)
str(mydata)
mydata$rural.code=NULL
mydata$lot.area=as.numeric(mydata$lot.area)

mydata$type=as.numeric(mydata$type)
mydata$masterbedroom.area=NULL
mydata$masterbedroom.length=NULL
mydata$masterbedroom.width=NULL
mydata$living.area=NULL
mydata$kitchen.length=NULL
mydata$kitchen.width=NULL
mydata$myresponse=NULL
str(mydata)
summary(mydata)

#DO NOT MODIFY THE NEXT LINE OF CODE
raw_for_export=mydata


#Use the following statement to standardize the numeric predictors which are in need of
#standardization. You need to carefully list the numbers of the columns that need to be 
#standardized.Be careful NOT to standardize columns that are categorical or the column corresponding to "myresponse".

#START OF VARIABLE STANDARDIZATION

col_nums=c(1,7); #Substitute (1,2,3,4) with the possitions at which columns that
#are in need of standardization appear in the updated "mydata"
#dataframe. You can find out the numbers of columns by running
#the str(mydata) command.In this example, columns listed 
#as the 1st, 2nd, 3rd, and 4th will be standardized.
#If you know that the columns in need of standardization are 
#located starting from column #1 and ending at column #N
#Then you can replace the 'c(1,2,3,4)' with 'seq(1,N)' for more efficient coding

#############################################################################################
#####################DO NOT MODIFY THE LINES BELOW UNTIL WHERE IT SAYS#######################
#############################"END OF VARIABLE STANDARDIZATION"###############################


cols_for_standard=as.matrix(mydata[,col_nums])
standardized=scale(cols_for_standard)
all_col_nums=c(1:length(names(mydata)))
remaining_cols=as.vector(all_col_nums[is.na(pmatch(all_col_nums, col_nums))])
remaining_data=subset(mydata,select=remaining_cols)
mydata=cbind(remaining_data, standardized)

#END OF VARIABLE STANDARDIZATION


#############################################################################################
#####################################ATTENTION###############################################
#############################################################################################

#######################IF THE ABOVE MODIFICATIONS ARE MADE CORRECTLY,########################
####AT THIS POINT "MYDATA" DATA FRAME SHOULD CONTAIN ONLY THE PREDICTORS AND THE OUTCOME.#### 
####IN CASE IT CONTAINS ANYTHING MORE OR LESS, THE CODE BELOW WILL NOT FUNCTION PROPERLY.####
#############################################################################################


str(mydata)

#############################################################################################
########################DO NOT MODIFY LINES BELOW UNTIL WHERE IT SAYS########################
##############"END ONE-LEAVE-OUT CROSS VALIDATION TO FIND THE 'BEST' k VALUE  "##############


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



#############################################################################################
############################SPECIFICATION OF THE FINAL KNN###################################
#############################################################################################

#You will need to pick the value for "k" that resulted in the highest pecentage of correct
#classifications from the previous step. That value is the one corresponding to the highest
#percentage of correct classifications, as shown with a dotted vertical line
#on the "Correct Classification Rate Vs k" plot. That value of "k" needs to be subsequently 
#passed to function "knn" below. What the "knn" call below will then do, will be to
#run "knn" with that pre-specified value of "k" to predict the testing set, and produce
#predictive accuracy of the model when applied to the testing data.

#START FINAL KNN RUN FOR THE BEST VALUE OF K

set.seed(1)
nearest_final=knn(train=pred_train, test=pred_test, cl=y_train, k=22)

#############################################################################################
############################DO NOT MODIFY BEYOND THIS POINT##################################
#############################################################################################


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

