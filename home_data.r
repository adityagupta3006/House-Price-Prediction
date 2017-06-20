library(lattice) 
library(corrplot)
graphics.off      
cat("\014")      # Clears screen
rm(list=ls())		 # Clears environment
setwd(getwd())
mydataset <- as.data.frame(read.csv(file="home_data.csv", header=T, sep=","))	# Reading CSV File

#-------------------------------------Data Cleaning Function--------------------------------------------------------------------------
clean <- function(x){	# Create a function for dataset cleaning
  suppressWarnings(as.numeric( gsub('[^a-zA-Z0-9.]', '', x)))	# Remove all unneccessary symbols and convert into numeric	
}
mydataset[] <- sapply(mydataset, clean) # Call function using sapply on dataset
mydatasetcla <- subset( mydataset, select = -c(id, date ) )	# Removing House id and date
#-------------------------------------------------------------------------------------------------------------------------------------

#splom(mydatasetcla) # Creating ScatterPlots
for (i in 2:ncol(mydatasetcla)){
  plot(mydatasetcla[,i], mydatasetcla$price, ylab="price", xlab = colnames(mydatasetcla)[i])
}

cor(mydatasetcla)[1,]	# Correlation matrix for Price

#Plotting correlation matrix
corrplot(cor(mydatasetcla), method="number")

#Spliting into training and test data
ind<-sample(2,nrow(mydatasetcla),replace=TRUE,prob=c(0.85,0.15))
training_data<-mydatasetcla[ind==1,]
test_data<-	mydatasetcla[ind==2,]

#Creating a linear model
linearModel <- lm(formula = price ~., data = training_data)
summary(linearModel)


#Updating Linear Model to eliminate unncessary variables
linearModel <- update(linearModel, .~.-sqft_basement-floors-sqft_lot-sqft_lot15
                      -sqft_living15-yr_renovated)
#residuals(linearModel)	# Prints residuals of linearModel
summary(linearModel)


pred <- predict(linearModel,test_data )	# Prediction of prices using linear model

actuals_preds <- data.frame(cbind(actuals=test_data$price, predicteds=pred))
correlation_accuracy <- cor(actuals_preds)  

#actuals_preds
correlation_accuracy
plot(pred, test_data$price)
plot(residuals(linearModel))
plot(linearModel)
