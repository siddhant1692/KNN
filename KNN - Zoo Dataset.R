
Zoo_r <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\KNN\\Zoo_r.csv')

# table of diagnosis
table(Zoo_r$type)
# table or proportions with more informative labels
round(prop.table(table(Zoo_r$type)) * 100, digits = 1)
# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# normalize the wbcd data
zoo_n <- as.data.frame(lapply(Zoo_r[2:17], normalize))
zoo_nl<-cbind(zoo_n,Zoo_r$type)
# create training and test data
zoo_train <- zoo_n[1:66, ]
zoo_test <- zoo_n[67:101, ]
# create labels for training and test data
zoo_train_labels <- Zoo_r[1:66, 18]
zoo_test_labels <- Zoo_r[67:101, 18]
#---- Training a model on the data ----
# load the "class" library
library(class)
zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                      cl = zoo_train_labels, k=20)
##--------Evaluating model performance ----
# load the "gmodels" library
library(gmodels)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)
#############################################################
## Improving model performance ----
# use the scale() function to z-score standardize a data frame
zoo_z <- as.data.frame(scale(Zoo_r[2:17]))
# create training and test datasets
zoo_train <- zoo_z[1:66, ]
zoo_test <- zoo_z[67:101, ]
# re-classify test cases
zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                      cl = zoo_train_labels, k=21)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)
# try several different values of k
zoo_train <- zoo_n[1:66, ]
zoo_test <- zoo_n[67:101, ]


zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=1)
CrossTable(x = zoo_test_labels, y = zoo_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=4)
CrossTable(x = zoo_test_labels, y = zoo_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=5)
CrossTable(x = zoo_test_labels, y = zoo_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=11)
CrossTable(x = zoo_test_labels, y = zoo_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=15)
CrossTable(x = zoo_test_labels, y = zoo_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=21)
CrossTable(x = zoo_test_labels, y = zoo_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=27)
CrossTable(x = zoo_test_labels, y = zoo_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

