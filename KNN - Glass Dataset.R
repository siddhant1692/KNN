

glass_r <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\KNN\\glass_r.csv')

# table of diagnosis
table(glass_r$Type)
# table or proportions with more informative labels
round(prop.table(table(glass_r$Type)) * 100, digits = 1)
# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# normalize the wbcd data
glass_n <- as.data.frame(lapply(glass_r[1:9], normalize))
glass_nl<-cbind(glass_n,glass_r$Type)
# create training and test data
glass_train <- glass_n[1:150, ]
glass_test <- glass_n[151:214, ]
# create labels for training and test data
glass_train_labels <- glass_r[1:150, 10]
glass_test_labels <- glass_r[151:214, 10]
#---- Training a model on the data ----
# load the "class" library
library(class)
glass_test_pred <- knn(train = glass_train, test = glass_test,
                      cl = glass_train_labels, k=20)
##--------Evaluating model performance ----
# load the "gmodels" library
library(gmodels)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)
#############################################################
## Improving model performance ----
# use the scale() function to z-score standardize a data frame
glass_z <- as.data.frame(scale(glass_r[-10]))
# create training and test datasets
glass_train <- glass_z[1:150, ]
glass_test <- glass_z[151:214, ]
# re-classify test cases
glass_test_pred <- knn(train = glass_train, test = glass_test,
                      cl = glass_train_labels, k=21)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)
# try several different values of k
glass_train <- glass_n[1:150, ]
glass_test <- glass_n[151:214, ]


glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=1)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=5)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=11)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=15)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=27)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

