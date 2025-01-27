data = read.csv("~/STAT5P87/Lectures/Test1/prostate-data.csv")
data = data[,c(1, 4, 6, 9)]

# Make training-testing sizes
data_size = dim(data)[1]
training_size = ceiling(data_size * 0.75)
testing_size = data_size - training_size

# Pick subsets
set.seed(0)
training_index = sample(c(1:data_size), size = training_size, replace = FALSE)
training = data[training_index,]
testing = data[-training_index,]

trainingY = training$lpsa

# subset selection k=3
model1 = lm(lpsa ~ .,data=training)
yhat1 = predict(model1, data = testing_data)

mean((trainingY - yhat1)^2)
# 0.534

# k=2
model2 = lm(lpsa ~ lcavol + lbph,data=training)
yhat2 = predict(model2, data = testing_data)

mean((trainingY - yhat2)^2)
# 0.545

model3 = lm(lpsa ~ lcavol + lcp,data=training)
yhat3 = predict(model3, data=testing)

mean((trainingY - yhat3)^2)
# 0.569

model4 = lm(lpsa ~ lcp + lbph,data=training)
yhat4 = predict(model4, data = testing_data)

mean((trainingY - yhat4)^2)
# 0.892

# k=1
model4 = lm(lpsa ~ lcavol,data=training)
yhat4 = predict(model4, data = testing_data)

mean((trainingY - yhat4)^2)
# 0.580

model5 = lm(lpsa ~ lcp,data=training)
yhat5 = predict(model5, data = testing_data)

mean((trainingY - yhat5)^2)
# 0.910

model6 = lm(lpsa ~ lbph,data=training)
yhat6 = predict(model6, data = testing_data)

mean((trainingY - yhat6)^2)
# 1.261












