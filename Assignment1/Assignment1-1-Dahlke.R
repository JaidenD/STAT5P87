data = read.csv("~/STAT5P87/Assignment1/a1-q1-data.csv")

# Rename variable 'x' to 'x1'
data$variable[data$variable == 'x'] <- 'x1'

# Remove all rows corresponding to observation 2
is_two = (data$value == 2)
data = data[-c(which(is_two==TRUE)),]

# Add rows to the data frame for a new observation 4 (x1 = 3, y = 2)
new_data = data.frame(c(4,4), c('x1','y'), c(3,2))
names(new_data) = c('observation', 'variable', 'value')

data = rbind(data,new_data)

# Swap rows so variable order is y,x1,y,x1,...
data[c(5, 6), ] = data[c(6, 5), ]

# Add rows to the data frame for a new variable x2 
#(x2(observation = 1) = 3, x2(observation = 3) = 1, x2(observation = 4) = 5)
newrow1 = data.frame(observation = 2, variable = "x2", value = "3")
newrow2 = data.frame(observation = 3, variable = "x2", value = "1")
newrow3 = data.frame(observation = 4, variable = "x2", value = "5")

data = rbind(data[1:2,], newrow1, data[3:4,], newrow2, data[5:6,], newrow3)

`value-squared` = as.numeric(data$value)^2
data = cbind(data,`value-squared`)


