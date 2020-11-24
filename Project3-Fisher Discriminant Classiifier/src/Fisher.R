
# Libraries
pacman::p_load(tidyverse, xfun, kableExtra)


# Data

train <- read.table("data/zip.train.gz") %>% as.matrix

digits <- numbers_to_words(0:9)
# Defined functions


# Compute the mean of each variable in a data set
mu_mat <- function(x) {
  n <- nrow(x)
  1/n * t(x) %*% matrix(1, n, 1)
}

# Compute the variance-covariance matrix of a data set
cov_mat <- function(x) {
  n <- nrow(x)
  x_bar <- mu_mat(x)
  1/n * t(x) %*% (diag(1,n) - 1/n*matrix(1,n,1) %*% matrix(1,1,n)) %*% x
}

# Rotate 2D data by some angle (radians)
rotate <- function(x, theta) {
  m <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),
              nrow = 2, byrow = T)
  t(m %*% t(x))
}

# Projection of X on w
proj <- function(X, w) {
  as.numeric(t(w) %*% w)^-1 * (X %*% w) %*% t(w)
}

# Bisection method for finding roots
root_bisect <- function(fun, ..., lower, upper, tol = 1e-8, max_n = 100) {
  if (lower > upper) {
    a <- upper
    b <- lower
  } else {
    a <- lower
    b <- upper
  }
  
  for (i in 1:max_n) {
    FA <- fun(a, ...)
    p  <- a + (b - a) / 2
    FP <- fun(p, ...)
    if (FP == 0 | (b-a)/2 < tol) {
      return(p)
    }
    if (FA * FP > 0) {
      a  <- p
      FA <- FP
    } else {
      b <- p
    }
  }
  return(NA_real_)
}


# Task 1



digit_mat <- vector("list", 10)  # Initialize a list to store matrix by digit

for (k in 0:9) {
  digit_mat[[k+1]] <- train[train[,1] == k, -1]
}

names(digit_mat) <- digits


# Mean features of each digit
  

mean_mat <- matrix(NA_real_, 256, 10, 
                   dimnames = list(
                     str_pad(as.character(1:256), 3, "left", "0"),
                     digits))

for (k in digits) {
  mean_mat[,k] <- mu_mat(digit_mat[[k]])
}


# First and last 5 features of the digits


m <-5  # How many to print from the top and bottom

rbind(head(mean_mat, m), tail(mean_mat, m)) %>%
  kable(digits = 2) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  add_header_above(c(" " = 1, "Handwritten Digit" = 10))


# covariance matrix of features for each digit
  
  

cov_arr <- array(NA_real_, dim = c(10, 256, 256))

dimnames(cov_arr) <- list(digits, NULL, NULL)


for (k in digits) {
  cov_arr[k,,] <- cov_mat(digit_mat[[k]])
}



cov_det <- vector("numeric", 10)
names(cov_det) <- digits

for (k in digits) {
  cov_det[k] <- det(cov_arr[k,,])
}



cov_det %>% t() %>%
  kable(caption = "Determinant for each digit covariance matrix") %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(1:10, width = "3em")




# Task 2

# Here, we implemented the Fisher discriminant criterion via simulation. Two multivariate classes were simulated from a gaussian distribution, and then used a rotation matrix to transform the data to form an elliptical shape

# ata Simulation**

# Number of observations per class
n <- 1000

# Red Class
red.mu <- c(10, 20)
red.sd <- c(3, 5)

red <- matrix(c(rnorm(n, 0, red.sd[1]),
                rnorm(n, 0, red.sd[2])), 
              nrow = n, byrow = F) %>%
             rotate(-pi/6)

red[,1] <- red[,1] + red.mu[1]
red[,2] <- red[,2] + red.mu[2]

# mean and covariance matrix
red.cov <- cov_mat(red)
red.mu <- mu_mat(red)

red.tbl <- tibble(u = red[,1],
                  v = red[,2],
                  class = "red")

# Green Class 
green.mu <- c(5, 7)
green.sd <- c(3, 7)

green <- matrix(c(rnorm(n, 0, green.sd[1]),
                  rnorm(n, 0, green.sd[2])), 
                nrow = n, byrow = F) %>%
                        rotate(pi/5)

green[,1] <- green[,1] + green.mu[1]
green[,2] <- green[,2] + green.mu[2]

#mean and covariance matrix
green.cov <- cov_mat(green)
green.mu <- mu_mat(green)


green.tbl <- tibble(u = green[,1],
                    v = green[,2],
bind_rows(red.tbl, green.tbl)) %>%
  mutate_at(vars(class), as.factor) %>%
  ggplot(aes(x = u, y = v, color = class)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("green", "red")) +
  labs(title = "Two classes with known mean and covariance")

# Optimal separating hyperplanes

# Fisher criterion
cov_sum <- red.cov + green.cov
w <- solve(cov_sum) %*% (red.mu - green.mu)
w <- w / as.numeric(sqrt(t(w) %*% w))  # Normalize the vector


# Projection of each class on w

proj_red_w <- proj(red, w)

proj_green_w <- proj(green, w)

prw.mu <- mean(proj_red_w[,1])
prw.sd <- sd(proj_red_w[,1])

pgw.mu <- mean(proj_green_w[,1])
pgw.sd <- sd(proj_green_w[,1])

c <- root_bisect(function(x){dnorm(x, prw.mu, prw.sd) - dnorm(x, pgw.mu, pgw.sd)},
                 lower = prw.mu, upper = pgw.mu)

c.prime <- c*(w[1]^2 + w[2]^2) / (w[1] * w[2])


p1 <- bind_rows(red.tbl, green.tbl) %>%
  mutate_at(vars(class), as.factor) %>%
  ggplot(aes(x = u, y = v, color = class)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = -w[1] / w[2], intercept = c.prime,
              linetype = "dashed") +
  geom_abline(slope = w[2] / w[1], intercept = 0) +
  scale_color_manual(values = c("green", "red")) +
  coord_fixed()



p2 <- bind_rows(tibble(proj = proj_red_w[,1], class = "red"), 
                tibble(proj = proj_green_w[,1], class = "green")) %>%
  mutate_at(vars(class), as.factor) %>%
  ggplot(aes(x = proj, y = ..density.., fill = class, color = class)) +
  geom_histogram(alpha = 0.3, position = "identity", bins = 35) +
  geom_vline(xintercept = c, linetype = "dashed") + 
  scale_fill_manual(values = c("green", "red")) +
  scale_color_manual(values = c("green", "red"))




gridExtra::grid.arrange(p1, p2, nrow = 1, 
                        top = "Fisher Criterion and Optimal Projection\n")


## Comparing Linear regression and Fisher criterion Decision boundaries


lin.tbl <- bind_rows(tibble(X1 = green[,1],
                            X2 = green[,2],
                            Y  = 0),
                     tibble(X1 = red[,1],
                            X2 = red[,2],
                            Y  = 1))

model_fit <- glm(Y ~ X1 + X2, data = lin.tbl, family = "binomial")

B <- model_fit$coefficients

logi <- data.frame(intercept = -B[1]/B[3],
                   slope     = -B[2]/B[3],
                   Boundary      = "Logistic")

fish <- data.frame(intercept = c.prime,
                   slope     = -w[1] / w[2],
                   Boundary      = "Fisher")

lin_fit <- function(X, y) {
  X <- cbind(matrix(1, nrow(X), 1), X)
  
  solve((t(X) %*% X)) %*% (t(X) %*% y)
}

X.lin <- select(lin.tbl, X1, X2) %>% as.matrix()
y.lin <- lin.tbl$Y %>% as.matrix()
z <- lin_fit(X.lin, y.lin)
linear <- data.frame(intercept = (1 - 2*z[1]) / (2*z[3]),
                     slope     = -z[2] / z[3],
                     Boundary  = "Linear")



bind_rows(red.tbl, green.tbl) %>%
  mutate_at(vars(class), as.factor) %>%
  ggplot(aes(x = u, y = v, color = class)) +
  geom_point(alpha = 0.3) +
  geom_abline(data = fish, 
              aes(slope = slope, intercept = intercept, linetype = Boundary)) +
  geom_abline(data = linear,
              aes(slope = slope, intercept = intercept, linetype = Boundary)) +
  scale_color_manual(name = "Class", values = c("green", "red")) +
  labs(title = "Decision Boundary",
       subtitle = "Linear Regression v. Fisher Criterion")

