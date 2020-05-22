#Group Project 
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
