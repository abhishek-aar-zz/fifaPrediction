fifa = read.csv("1_cleaned_data.csv")
options(scipen = 999)
library("ggplot2")

##CORRELATION
#SCATTER PLOT BETWEEN LOG(VALUE) VS OVERALL
print(ggplot(fifa, aes(x=Overall, y =unlist(lapply(lapply(fifa$Value, log), as.numeric))))+ylab("Value") + geom_jitter()+geom_smooth(method = lm))

#PEARSON CORRELATION TEST
print(cor.test(unlist(lapply(lapply(fifa$Value, log), as.numeric)), fifa$Overall, method="pearson"))

##REGRESSION

#CREATING THE TRAINING AND TEST DATA
set.seed(100)
trainingRows<-sample(1:nrow(fifa), 0.8*nrow(fifa))
trainingData<-fifa[trainingRows, ]
testData<-fifa[-trainingRows, ]

#FIT THE MODEL ON TRAINING DATA AND PREDICT ON TEST DATA
model = lm(log(Value)~Overall, data = trainingData)
predictedData = predict(model,testData)
predictedData = exp(predictedData)


#REVIEW DIAGNOSTIC MEASURES
summary(model)

##CALCULATE PREDICTION ACCURACY AND ERROR RATES
diff = data.frame(cbind(actuals=testData$Value, predicteds=predictedData))

#CORERLATION ACCURACY
correlation_accuracy<-cor(diff)
print(correlation_accuracy)

#MIN-MAX ACCURACY CALCULATION
min_max_accuracy <-mean(apply(diff, 1, min)/apply(diff,1,max))
print(min_max_accuracy)

#MEAN-ABSOLUTE-PERCENTAGE-ERROR (MAPE) CALCULATION
mape <- mean(abs((diff$predicteds - diff$actuals))/diff$actuals) 








