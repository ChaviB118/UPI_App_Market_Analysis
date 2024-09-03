#Loading data set:

data <- read.csv("C:/Users/Dell/Documents/Statistics/Data Analysis/Data set/UPI/UPI cleaned dataset.csv")
#Understanding data set:

str(data)
summary(data)

#Data Visualization:

plot(data)

#Extracting required data for analysis:

data1 <- data[,c(2,3,4,5)]
View(data1)

#Understanding extracted data set:

str(data1)
summary(data1)


#Visualizing the extracted data:

plot(data1)

#Data Analysis:

attach(data1)

#Multiple correlation:
#Correlation Matrix: using Pearson, Spearman and Kendall correlation

cor(data1,method = "pearson")
cor(data1,method = "spearman")
cor(data1,method = "kendall")


#Fitting linear regression models:
#Y ~ a + b*X

#(1) <- Y = Total Volume and X <- Total Value

model <- lm(Volume..Mn..~Value..Cr.)
model
#a = 2.380212, b =  0.005543
summary(model)
#Adjusted R-squared:  98.99%
anova(model)
#Probability of F statistic is very small(less than 5%)leading to rejection of null hypothesis.
#Rejecting H0: b = 0

#(2)<- Y = Total Volume by Customers and X <- Total Value by Customers

model1 <- lm(Volume..Mn..by.Customers.~Value..Cr..by.Customers.)
model1
#a = 1.640995 , b =  0.005528
summary(model1)
#Adjusted R-squared:  99.12%
anova(model1)
#Probability of F statistic is very small(less than 5%)leading to rejection of null hypothesis.
#Rejecting H0: b = 0

#(3) <- Y = Total Volume and X <- Total Volume by Customers

model2 <- lm(Volume..Mn..~Volume..Mn..by.Customers.)
model2
# a = 1.950 and b = 1.006
summary(model2)
#Adjusted R-squared:  99.88% 
anova(model2)
#Probability of F statistic is very small(less than 5%)leading to rejection of null hypothesis.
#Rejecting H0: b = 0

#(4) <- Y = Total Value and X <- Total Value by Customers

model3 <- lm(Value..Cr.~Value..Cr..by.Customers.)
model3
# a = 225.752  and b = 1.003
summary(model3)
#Adjusted R-squared:  99.94%
anova(model3)
#Probability of F statistic is very small(less than 5%)leading to rejection of null hypothesis.
#Rejecting H0: b = 0

#Fitting a Multiple linear regression model of best fit:
#Using Forward Selection Method to get the model of best fit to the data.
#It is selected based on Adjusted R square (Adj R sq) value:

m0 <- lm(Volume..Mn..~1) #Null Model
summary(m0)

m1 <- update(m0,.~.+Value..Cr.)
summary(m1)
#Adj R sq <- 98.99%

m2 <- update(m1,.~.+Volume..Mn..by.Customers.)
summary(m2)
#Adj R sq <- 99.94% (Increased)

m3 <- update(m2,.~.+Value..Cr..by.Customers.)
summary(m3)
#Adj R sq <- 99.97% (Increased)

#Adding Interaction between exploratory variables:

m4 <- update(m3,.~.+Value..Cr.:Volume..Mn..by.Customers.)
summary(m4)
#Adj R sq <- 99.97% (No Change)

m5 <- update(m3,.~.+Value..Cr.:Value..Cr..by.Customers.)
summary(m5)
#Adj R sq <- 99.97% (No Change)

m6 <- update(m3,.~.+Volume..Mn..by.Customers. :Value..Cr..by.Customers.)
summary(m6) 
#Adj R sq <- 99.97% (No Change)

m7 <- lm(Volume..Mn..~ Value..Cr.*Volume..Mn..by.Customers.*Value..Cr..by.Customers.)
#Saturated model
summary(m7)

#We can see that adding interaction terms to the model is not increasing the Adj R sq value at all.
#Hence we do not add any interaction terms to the model.
#Therefore we can see that m3 is the model of best fit with the Adj R sq values of 99.97%
#Multiple Linear Regression Model of Best Fit is: Y ~ a + b1X1 + b2X2 + b3X3 
#Y = Total Volume, X1 = Total Value, X2 = Volume by customers, X3 = Value by customers

detach(data1)
