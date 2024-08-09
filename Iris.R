# IRIS FLOWER CLASSIFICATION

Iris <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\Codsoft\\IRIS.csv")
View(Iris)

# Data Visualization using plot()
plot(Iris, col="blue")

# Scatter plot of the variables sepal width vs. sepal length

plot(Iris$sepal_width, Iris$sepal_length,lwd=2,col="red",xlab="sepal width",ylab="sepal length")

# Histogram

hist(Iris$sepal_width, col="red")
hist(Iris$sepal_length, col="orange")
hist(Iris$petal_length, col="blue")
hist(Iris$petal_width, col="green")


library(caret)

# Using boxplots for visualization of the data

attach(Iris)
par(mfrow=c(2,2))
boxplot(sepal_length~species, data=Iris, col="yellow")
boxplot(sepal_width~species, data=Iris, col="green")
boxplot(petal_length~species, data=Iris, col="red")
boxplot(petal_width~species, data=Iris, col="blue")

# Checking for missing values in the data
sum(is.na(Iris))

# Fixing the seed number
set.seed(10)

# Splitting training set and test set of the data

TrainIndex <- createDataPartition(Iris$species, p=0.8, list=F)
Trainset <- Iris[TrainIndex, ]
Testset <- Iris[-TrainIndex, ]

# scatter plots of training set and test set

plot(Trainset)
plot(Testset)

# Using SVM model (polynomial kernel)

# Build Training model

library(kernlab)
Model <- train(species~., data = Trainset, method="svmPoly",  na.action=na.omit,
                preProcess=c("scale","center"), trControl=trainControl(method="none"), 
                tuneGrid=data.frame(degree=1,scale=1,C=1)
)

# Build cv model

Model.cv <- train(species~., data = Trainset, method="svmPoly",  na.action=na.omit,
                  preProcess=c("scale","center"), trControl=trainControl(method="cv",number=10), 
                  tuneGrid=data.frame(degree=1,scale=1,C=1)
)

# Apply model for prediction

# Apply model to make prediction on Training set
Model.train <- predict(Model, Trainset)

# Apply model to make prediction on Testing set
Model.test <- predict(Model.cv, Testset)

# Perform cross-validation
Model.cv <- predict(Model.cv, Trainset)

# Model performance (Displays confusion matrix and statistics)
Model.train.confusion <- confusionMatrix(as.factor(Model.train), as.factor(Trainset$species))
Model.test.confusion <- confusionMatrix(as.factor(Model.test), as.factor(Testset$species))
Model.cv.confusion <- confusionMatrix(as.factor(Model.cv),as.factor(Trainset$species))

print(Model.train.confusion)
print(Model.test.confusion)
print(Model.cv.confusion)













































