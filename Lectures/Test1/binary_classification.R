data = read.csv('~/STAT5P87/Lectures/Test1/simple-classification-data.csv')

set.seed(0)

p = 2 # Binary classification
data_size = dim(data)[1]

# 50-50 training testing
training_split = 0.5
testing_split = 1 - training_split


training_index = sample(c(1:n), size = n*training_split, replace = FALSE)

# split data
training_data = data[training_index,]




















































