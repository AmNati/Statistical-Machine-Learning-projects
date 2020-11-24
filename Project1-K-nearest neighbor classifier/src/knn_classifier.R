library("tidyverse")  # For working with data set
library(keras)        # For one-hot encoding: to_categorical

# Read in data sets
train <- read.table("data/zip.train.gz")
test <- read.table("data/zip.test.gz")

# Split into x, y
x.train <- train[, -1]
y.train <- train[, 1]

x.test <- test[, -1]
y.test <- test[, 1]



# Hot encoding
to_categorical <- function(y) {
  classes <- sort(unique(y))
  
  k <- length(classes)
  n <- length(y)
  
  y.cat <- matrix(0, nrow = n, ncol = k)
  
  for(i in 1:n) {
    class <- y[i] == classes
    y.cat[i, class] <- 1
  }
  colnames(y.cat) <- classes
  y.cat
}


# Turn y into categorical response
y.train <- y.train %>% to_categorical()
y.test <- y.test %>% to_categorical()

class_labels <- 0:9
named_class_labels <- c("zero", "one", "two", "three", "four", 
                        "five", "six", "seven", "eight", "nine")


# Returns the label index for each prediction
knn <- function(x_train, y_train, x_test, y_test, k = 1) {
  
                            # observation matrix dimensions
                            n <- nrow(x_train)
                            p <- ncol(x_train)
                            
                            # basic checks to ensure dimensional conformability
                            stopifnot(n == nrow(y_train))
                            stopifnot(p == ncol(x_test))
                            stopifnot(nrow(x_test) == nrow(y_test))
                            stopifnot(ncol(y_train) == ncol(y_test))
                            stopifnot(k > 0)
                            
  
      pred_func <- function(z) {
                            z_star <- replicate(n, z) %>% t()
                            
                            # Calculated L2 distance
                            D <- x_train - z_star
                            L2 <- rowSums(D^2)
                            
                            # Get indices of k nearest neighbors
                            k_nearest <- order(L2) %>% head(k)
                            
                            # Make prediction
                            if (k == 1) {
                              knn_response <- y_train[k_nearest, ] %>% as.matrix %>% t()
                              knn_mean <- knn_response
                            } else {
                              knn_response <- y_train[k_nearest, ]
                              
                              knn_mean <- colMeans(knn_response)
                            }
                    return(which.max(knn_mean))
                              }
  
            apply(x_test, 1, pred_func)
}


# Generating prediction
results <- knn(x_train = x.train, y_train = y.train.cat,
               x_test = x.test, y_test = y.test.cat,
               k = 5)

# Calculate error rate
correct <- class_labels[results] == y.test
incorrect <- !correct

mean(incorrect)


# Visualizing Images of classified Digits
print_image <- function(index, x, label) {
              rotate <- function(x) t(apply(x, 2, rev))
              matrix(x[index, ], nrow = 16, byrow = T) %>%
                rotate %>%
                image(col = gray.colors(n = 8, 1, 0), xlab = label, 
                      xaxt = 'n', yaxt = 'n')
}

# Sample Images of correctly classified digits
for(i in sample(which(correct), 12, F)) {
                title <- paste0("Pred: ", named_class_labels[results[i]], 
                                " | Obs: ", named_class_labels[which.max(y.test.cat[i,])])
                print_image(i, x.test, title)
}