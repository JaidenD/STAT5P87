require(rpart)

data = read.csv("prostate-data.csv")

# part a
fit = rpart(lcavol ~ age + lpsa + lcp, data = data, 
            control = rpart.control(maxdepth = 3))

preds = predict(fit, newdata = data)

mse = mean((preds - data$lcavol)^2)
mse # mse = 0.4318909

# part b
plot(fit)
text(fit, cex = 1)
fit

# Pruning the "left" node
temp = mydata[mydata$lcavol < 0.262,]
n = dim(temp)[1]
y = temp$lpsa

Rnode = sum((y - mean(y))^2)

yHat = matrix(0, nrow = n)
yHat[temp$lpsa < 2.303] = 0.279
yHat[temp$lcavol >= 2.303] = 1.435

Rbranch = sum((y - yHat)^2)
(Rnode - Rbranch) / 1 # -16.94624



# Pruning the "right" node
temp = mydata[mydata$lcavol >= 0.262,]
n = dim(temp)[1]
y = temp$lpsa

Rnode = sum((y - mean(y))^2)

yHat = matrix(0, nrow = n)
yHat[temp$lcp < 2.14] = 2.147
yHat[temp$lcp >= 2.14] = 3.038

Rbranch = sum((y - yHat)^2)
(Rnode - Rbranch) / 1 # -8.127647

# The left node has a smaller alpha hence gets pruned
# The left node has input lpsa with threshold <2.303

# part c
# TODO havent learned this, itll be done next week.




