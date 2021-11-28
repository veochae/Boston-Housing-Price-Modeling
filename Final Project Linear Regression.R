options(scipen=999)
library(ftsa)

str(mydata)

#This code is used to randomly generate train and test data sets from the oldies orignial.
RNGkind(sample.kind = "Rounding")
set.seed(1) 

train_index=sample(550,440,replace=F)

mydata_train=mydata[train_index,]
mydata_test=mydata[-train_index,]
dim(mydata_train)
dim(mydata_test)

#this is the linear regression model equation 
#did not use the masterbedroom area and living area becasue they are correlated variable with the areanomaster
mydata_training_model = lm(sold.price~rural.code+
                             type+
                             num.baths+
                             kitchen.area+
                             areanomaster+
                             year_cat,
                           data = mydata_train)
summary(mydata_training_model)

#This is the test predictive model for soldprice.
Predicted_test=predict(mydata_training_model, mydata_test)

#This is code used for calculating MAE and MAPE of the Predictive Model above.
error(forecast=Predicted_test, true=mydata_test$sold.price, method="mae")
error(forecast=Predicted_test, true=mydata_test$sold.price, method="mape")