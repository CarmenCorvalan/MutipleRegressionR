# Multicore processing

library(doMC)
registerDoMC(cores=4)

# Install Packages

library("pacman")
pacman::p_load("e1071", "lattice", "ggplot2", "caret", "corrplot", "gbm", "dplyr", "kernlab", "pROC")

# Import & Rename Data

Existing <- existingproductattributes2017
summary(Existing)
duplicated(Existing)

##### Playing with data ####
#Predictions on important variables
data2 <- filter(
  Existing, ProductType %in% c(
    "PC","Laptop","Smartphone","Netbook"
  )
)
data2
ggplot(
  data2, aes(
    x=ProductType, 
    y=Volume, 
    size=x4StarReviews, 
    color=PositiveServiceReview
  )
)+
  geom_point()+
  ylim(0,1500)

####Data Cleaning####
# Remove Warranty
data3 <- Existing[!duplicated(Existing[,c("ProductType","PositiveServiceReview", 
                                          "ProductDepth", "ShippingWeight", 
                                          "Volume", "x4StarReviews"
)
]
),
]
data3

# Remove outliers
outliers <- boxplot(Existing$Volume)$out
data3[which(Existing$Volume %in% outliers),]
data3 <- data3[-which(data3$Volume %in% outliers),]
data3

# Remove 5starReviews and BestSellerRank
drops <- c("x5StarReviews","BestSellersRank")
data3 <- data3[, !(names(data3) %in% drops)]
data3


####Dummies and Correlation####
#Create Dummy Variables
#What's happening here?
newDataF <- dummyVars(" ~ .", data = data3)
data4 <- data.frame(predict(newDataF, newdata = data3))
data4
#Correlation Matrix
corrData <- cor(data4)
corrData
corrplot(corrData, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = FALSE)

####Linear Model####
# Load Train and Test datasets
# Identify feature and response variable(s) and values must be numeric
set.seed(77)
inTraining <- createDataPartition(
  data4$Volume,
  p = 0.75,
  list = FALSE
)
trainSet <- data4[inTraining,]
testSet <- data4[-inTraining,]

# Train the model using the training sets and check score

control2 <- trainControl(method="repeatedcv",
                         number=20, 
                         repeats = 5,
                         predictionBounds=c(0,NA),
                         )

SVMFit <- train(Volume ~0+., 
                data=trainSet, 
                method="svmLinear2", 
                metric="RMSE", 
                trControl=control2)
# display results
print(SVMFit) 
summary(SVMFit)

Volume.pred2<-predict(SVMFit, testSet)
Volume.pred2 <- as.integer(Volume.pred2)
Volume.pred2




