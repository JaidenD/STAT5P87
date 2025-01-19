data = read.csv("~/Assignment1/a1-q1-data.csv")

# Rename variable 'x' to 'x1'
data$variable[data$variable == 'x'] <- 'x1'

# Remove all rows corresponding to observation 2
is_two = (data$value == 2)
data = data[-c(which(is_two==TRUE)),]

# Add rows to the data frame for a new observation 4 (x1 = 3, y = 2)
new_data = data.frame(c(4,4), c('x1','y'), c(3,2))
names(new_data) = c('observation', 'variable', 'value')
new_data

data = rbind(data,new_data)
data

# Add rows to the data frame for a new variable x2 
#(x2(observation = 1) = 3, x2(observation = 3) = 1, x2(observation = 4) = 5)
is_observation_two = (data$observation == 2)
is_observation_three = (data$observation == 3)
is_observation_four = (data$observation == 4)

x_2 = 3*is_observation_two + 1*is_observation_three + 5*is_observation_four
# I'm not sure if this is correct.
data$x_2 = x_2
data

# Create  a  new  column  named  ‘value-squared’  containing  the  squaredy,x1  andx2values for each observation









