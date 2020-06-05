#Part1 
##Data Visualization: Yuntian Yang
# Semester Project 
# Avocado Data from Kaggle
# Visualization - Yuntian Yang
# Scatterplot, boxplot, density plot, and their outliers, corrplot and heatmap

library(ggplot2)

getwd()

avocado <- read.csv('semester project/avocado.csv', header = TRUE)
head(avocado)
avocado$X <- NULL
head(avocado)
colnames(avocado)

avocado$year = as.factor(avocado$year)
avocado$Date = as.Date(avocado$Date)
avocado$month  = factor(months(avocado$Date), levels = month.name)

# conventional vs organic , average price, year

# Boxplot: Average price vs type by year
options(repr.plot.width= 7, repr.plot.height=5)
ggplot(avocado, aes(type, AveragePrice))+
  geom_boxplot(aes(colour = year))+
  labs(colour = "Year", x = "Type", y ="Average Price",
       title = "Box Plot - Average price of avocado per year by avocado type")

# barplot: year by type
ggplot(avocado) +
  geom_bar(mapping = aes(x = year, fill = type))

# scatterplot: price against time by type
ggplot(avocado, aes(x = Date, y = AveragePrice, color = type))+
  geom_point()+
  geom_smooth()+
  labs(colour = "Type", x = 'Date', y = 'Price', 
       title = 'Average Price Over Time and by type')

# density/distribution:
x <- avocado$AveragePrice
h<-hist(x, breaks=12, col="red", xlab="Average Price",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)


d <- density(avocado$AveragePrice)
plot(d, main="Kernel Density of Average Price", , xlab="Average Price")
polygon(d, col="red", border="blue")


# heatmap/correlation plot
library(corrplot)

df$X = NULL
colnames(df)[12]
for (i in c(2:9, 11)){print(typeof(df[, i])) }

colnames(df[, c(2:9, 12)])
C <- cor(df[, c(2:9, 12)])
corrplot(C, method = "number")

apply(avocado[, 4:6], 2, mean)

ggplot(data = avocado) +
  geom_point(mapping = aes(x = AveragePrice, y = Total.Volume, color=type))

###############################################
#Truc
#Part2: Histogram, building linear regression model
#To build a linear model, we first check if there is a relationship 
#between our predictors (date&type) and the response (avg price)
#convert date to continuous variable
avocado$Date = as.numeric(avocado$Date) 
#Histogram for Average Price for a Single Avocado
ggplot(avocado, aes(x = AveragePrice)) + geom_histogram(aes(y =..density..), breaks = seq(0, 3, by = 0.05),  colour = "black", fill = "pink") + stat_function(fun = dnorm, args = list(mean = mean(avocado$AveragePrice), sd = sd(avocado$AveragePrice))) + ggtitle("Average Price for a Single Avocado") 
#Descriptive statistics for Average Price for a Single Avocado
summary(avocado)
sd(avocado$AveragePrice) #=> get 0.40267766
#Histogram for Average Price for a Single Avocado by type
#to show it's more precise if we divide data by type
ggplot(avocado, aes(x = AveragePrice, fill = type)) +  geom_histogram(binwidth = .05, alpha =.5, position = "identity") +labs(x = "Average Price", y = "Frequency")
#Create subsets: organic and conventional avocados
conventional = subset(avocado, subset = type=="conventional")
organic = subset(avocado, subset = type == "organic")
#Visualizing data
#Histogram for Conventional avocados under the normal distribution
ggplot(conventional, aes(x = AveragePrice)) + 
  geom_histogram(aes(y =..density..), breaks = seq(0, 3, by = 0.05), colour = "black", fill = "lightsalmon1") +stat_function(fun = dnorm, args = list(mean = mean(conventional$AveragePrice), sd = sd(conventional$AveragePrice))) + labs(x= "Avg Price of Conventional")
#Histogram for Organic avocados under the normal distribution
ggplot(organic, aes(x = AveragePrice)) + geom_histogram(aes(y =..density..), breaks = seq(0, 3, by = 0.05), colour = "black", fill = "darkslategray2") +stat_function(fun = dnorm, args = list(mean = mean(organic$AveragePrice), sd = sd(organic$AveragePrice))) + labs(x= "Avg Price of Organic")
#Do 2-sample 2-tailed test to prove that two sample means are not equal
ttest.type <- t.test(conventional$AveragePrice, organic$AveragePrice, var.equal = TRUE)
ttest.type
#data:  conventional$AveragePrice and organic$AveragePrice
#t = -105.59, df = 18247, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.5051658 -0.4867522
#sample estimates:
#  mean of x mean of y 
#1.158040  1.653999 
#Fit regression model and use it to check date and type are significant variables
fit = lm(AveragePrice~ Date+type, data = avocado)
fit
#Coefficients:
#(Intercept)         Date  typeorganic  
#1.055927     0.001201     0.495966 
summary(fit)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.056e+00  5.179e-03  203.87   <2e-16 ***
#  Date        1.201e-03  4.731e-05   25.39   <2e-16 ***
#  typeorganic 4.960e-01  4.616e-03  107.44   <2e-16 ***
#Residual standard error: 0.3118 on 18246 degrees of freedom
#Multiple R-squared:  0.4004,	Adjusted R-squared:  0.4004 
#F-statistic:  6093 on 2 and 18246 DF,  p-value: < 2.2e-16 
######################################################

#Jasmeen
### Following Truc's log
avocado_2$Date = as.numeric(avocado$Date) 
fit = lm(AveragePrice~ Date+type, data = avocado)
summary(fit)
# returns the summary statistic of linear model for the whole dataset 

# All of the p-values are well below standard alpha level of 0.05 and means 
# that the coefficients are significant.

# When p Value is less than significance level (< 0.05), we can safely reject 
# the null hypothesis that the coefficient β of the predictor is zero. 

# This means that when the p-value is less than our standard significance level of 0.05, we can 
# reject the Null Hypothesis that the coefficients of the predictor are 0.

### Using Hana’s model for training data set, we get:
avocadoO = avocado[avocado$type == "organic",]
avocadoC = avocado[avocado$type == "conventional",]
#create the training(development) and tet(validation) data samples
#from original data.
set.seed(20000) #setting the seed to reproduce results of random sampling.
trainingRowIndex = sample(1:nrow(avocadoO),0.8*nrow(avocadoO))
trainingData = avocadoO[trainingRowIndex,] #model training data
testData = avocadoO[-trainingRowIndex,] #test data

#developing modele on the training data and use it to predict the
#average price on test data
lmModO = lm(AveragePrice~Date, data=trainingData)
#Summary on linear model of Organic Avocado
summary(lmModO)
# Returns the summary statistic for Organic type linear model
# Following the same steps for Conventional we have the following:
set.seed(20000) #setting the seed to reproduce results of random sampling.
trainingRowIndex = sample(1:nrow(avocadoC),0.8*nrow(avocadoC))
#r training data
trainingData = avocadoC[trainingRowIndex,] #model training data
testData = avocadoC[-trainingRowIndex,] #test data

#developing modele on the training data and use it to predict the
#average price on test data
lmModC= lm(AveragePrice~Date, data=trainingData)
#Summary of Conventional avocado
summary(lmModC)


######################################################

#Qian
### Following Truc's log
avocado_2$Date = as.numeric(avocado$Date) 
fit = lm(AveragePrice~ Date+type, data = avocado)
summary(fit)
# returns the summary statistic of linear model for the whole dataset 
### Using Hana’s model for training data set, we get:
avocadoO = avocado[avocado$type == "organic",]
avocadoC = avocado[avocado$type == "conventional",]
fit1 = lm(AveragePrice~ Date+type, data = avocadoO)
summary(fit1)
fit2 = lm(AveragePrice~ Date+type, data = avocadoO)
summary(fit2)
# returns the summary statistic of linear model for the Organic and Conventional  dataset 
### Get Std. Error
modelSummary1 <- summary(fit1)  # capture model summary as an object
modelSummary2 <- summary(fit2)  
modelCoeffs1 <- modelSummary1$coefficients
modelCoeffs2 <- modelSummary2$coefficients  # model coefficients
std1.error <- modelCoeffs1["Date", "Std. Error"]  # get std.error for Date
std2.error <- modelCoeffs2["Date", "Std. Error"] 
### Get F-value
### F_statistic <- summary(fit)$fstatistic
Get AIC,BIC
AIC(fit)  # AIC(Akaike’s information criterion)
BIC(fit) #BIC(Bayesian information criterion)


######################################################
#Hana 
#Predicting linear models
#K fold cross validation
#Build dataset into 80:20 sample (training:test)
#Build the mode on the 80% sample and then use the model to predict
#dependent variables on test data//
avocado = read.csv("avocado.csv")
avocado$Date = as.numeric(avocado$Date)
#dividing data into conventional vs organic
avocadoO = avocado[avocado$type == "organic",]
avocadoC = avocado[avocado$type == "conventional",]

##############ORGANIC AVOCADO##############
#create the trianing(development) and tet(validation) data samples
#from original data.
set.seed(20000) #setting the seed to reproduce results of random sampling.
trainingRowIndex = sample(1:nrow(avocadoO),0.8*nrow(avocadoO))
#r training data
trainingData = avocadoO[trainingRowIndex,] #model training data
testData = avocadoO[-trainingRowIndex,] #test data

#developing modele on the training data and use it to predict the
#average price on test data
lmMod = lm(AveragePrice~Date, data=trainingData)
pricePred = predict(lmMod, testData)

#Caclulating prediction accuracy and error aates
actuals_preds = data.frame(cbind(actuals=testData$AveragePrice,
                                 predicteds=pricePred))
correlation_accuracy = cor(actuals_preds) #12%
head(actuals_preds)
#calculate min max accuracy and MAPE
min_max_accuracy = mean(apply(actuals_preds, 1, min)
                        /apply(actuals_preds,1,max))
#84.8% minmax accraucy
mape = mean(abs((actuals_preds$predicteds - actuals_preds$actuals))
            /actuals_preds$actuals)
#18.8%, mean absolute percentage deviation.

#k-Fold Cross Validation
library(DAAG)
cvResults = suppressWarnings(CVlm(data=avocadoO, form.lm=AveragePrice
                                  ~ Date, m=3,dots=FALSE, seed=29,
                                  legend.pos="topleft", printit= TRUE,
                                  main="kFold CV for Organic Avocado."));

##############CONVENTIONAL AVOCADO##############
set.seed(20000) #setting the seed to reproduce results of random sampling.
trainingRowIndex = sample(1:nrow(avocadoC),0.8*nrow(avocadoC))
#r training data
trainingData = avocadoC[trainingRowIndex,] #model training data
testData = avocadoC[-trainingRowIndex,] #test data

#developing modele on the training data and use it to predict the
#average price on test data
lmMod = lm(AveragePrice~Date, data=trainingData)
pricePred = predict(lmMod, testData)

#Caclulating prediction accuracy and error aates
actuals_preds = data.frame(cbind(actuals=testData$AveragePrice,
                                 predicteds=pricePred))
correlation_accuracy = cor(actuals_preds) #35.2%
head(actuals_preds)
#calculate min max accuracy and MAPE
min_max_accuracy = mean(apply(actuals_preds, 1, min)
                        /apply(actuals_preds,1,max))
#88% minmax accraucy
mape = mean(abs((actuals_preds$predicteds - actuals_preds$actuals))
            /actuals_preds$actuals)
cvResults = suppressWarnings(CVlm(data=avocadoC, form.lm=AveragePrice
                                  ~ Date, m=3,dots=FALSE, seed=29,
                                  legend.pos="topleft", printit= TRUE,
                                  main="kFold CV for Conventional Avocado."));



avocadoO = avocado[avocado$type == "organic",]
apply(avocadoO,2,class)
boxplot(avocado$AveragePrice[avocadoO$region == "Albany"])
boxplot(avocado$AveragePrice[avocadoO$region == "Houston"])
boxplot(avocado$AveragePrice[avocadoO$region == "Indianapolis"])
boxplot(avocado$AveragePrice~avocado$region)
p=ggplot(data = avocadoO, aes(xlab=region,ylab=AveragePrice))
p+boxplot()

function1 = function(a,r){
  res = mean(a$AveragePrice[a$region == r])
  #output : averge price of region
  return(res)
}

#average price of houston is different from albany
t.test(avocado$AveragePrice[avocado$region == "Houston"],avocado$AveragePrice[avocado$region == "Albany"])



