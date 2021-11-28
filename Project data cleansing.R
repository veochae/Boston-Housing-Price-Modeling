options(scipen=999)

load("C:/Users/dchae2/Downloads/realtor.spring.2020.rda")

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
mydata$year_cat=cut(mydata$year.built,br=c(1725,1872,2019))
mydata$year_cat=as.factor(mydata$year_cat)
mydata$year.built=NULL

#getting rid of datarows with sold price of N/A
mydata$sold.price[is.na(mydata$sold.price)]=0
mydata=subset(mydata,mydata$sold.price>0)

#replacing the N/A to 0 for the variable column "lot size"
mydata$lot.area[is.na(mydata$lot.area)] = "0"

#importing the zipcode data
z=read.csv("C:/Users/dchae2/Downloads/rural-demo.csv")
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
