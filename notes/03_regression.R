x1 <- c(1, 4, 6)
x2 <- c(7, 7, 2)
cbind(x1, x2)
rbind(x1, x2)

x <- matrix(data = c(1, 4, 6, 7, 7, 2), ncol=2, nrow=3)
x

2*x

t(x)

y <- matrix(c(5, 6, 7, 2), ncol=2, nrow=2)
y

x %*% y
y %*% x

diag(5)

x %*% diag(2)

diag(1:3)

y %*% solve(y)

n <- 10
x <- rnorm(n)
a <- rep(1, n)

1/n * t(a) %*% x
mean(x)

r <- x - mean(x)
sum(r^2)

t(r) %*% r
t(r)

crossprod(r)/(n-1)
var(x)

y <- trees$Volume
X <- cbind(1, trees$Girth, trees$Height)
X


solve(t(X) %*% X) %*% t(X) %*% y

solve(crossprod(X)) %*% crossprod(X, y)

fit <- lm(Volume ~ Girth + Height, data = trees)
fit$coefficients

x3 <- trees$Girth + 2 * trees$Height
x3

fit <- lm(Volume ~ Girth + Height, data = trees)
fit

X <- cbind(1, trees$Girth, trees$Height)
head(X)

library(Matrix)
rankMatrix(X)
solve(crossprod(X))

betahat <- solve(crossprod(X)) %*% crossprod(X, y)

yhat <- X %*% betahat

plot(y ~ yhat)

res <- y - yhat
mean(res)
n <- length(y)
p <- ncol(X)

s2 <- as.numeric(crossprod(res)/(n-p))

stderr <- sqrt(diag(s2 * solve(crossprod(X))))
betahat

summary(fit)

betahat/stderr

res0 <- y - mean(y)
(crossprod(res0) - crossprod(res))/2 / (crossprod(res)/(n-3))

x3 <- rnorm(31)

fit <- lm(Volume ~ Girth + Height + x3, data=trees)
summary(fit)
fitR <- lm(Volume ~ Girth, data=trees)

anova(fitR, fit)


n <- 50; M <- 500
x <- seq(1, M, len=n)
X <- cbind(1, x, x^2, x^3)
beta <- matrix(c(1,1,1,1), nrow=4, ncol=1)
beta


y <- X%*%beta+rnorm(n,sd=1)

crossprod(X)

X <- cbind(1, trees$Girth, trees$Height)
y <- trees$Volume

betahat

QR <- qr(X)
Q <- qr.Q(QR)
R <- qr.R(QR)
crossprod(Q)
backsolve(R, crossprod(Q, y))
betahat

data(mtcars)
head(mtcars)

x <- factor(mtcars$cyl)

X <- model.matrix(~ x)
head(X)
head(mtcars[,1:2])

fit <- lm(mpg ~ factor(cyl), data=mtcars)
summary(fit)

X <- cbind(1, model.matrix(~ x - 1))
head(X)

lm.fit(X, mtcars$mpg)$coefficients

fit <- lm(mpg ~ factor(cyl) * wt, data=mtcars)
summary(fit)

fit <- lm(mpg ~ wt, data=mtcars)
summary(fit)

fit <- lm(wt ~ mpg, data=mtcars)
summary(fit)


















