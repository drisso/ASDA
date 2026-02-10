mydiff <- function(a, b = 2) {
    ret <- a - b
    return(ret)
}

loglik <- function(lambda, x = e100) {
    sum(dpois(x, lambda, log = TRUE))
}

lambdas = seq(0.05, 0.95, length = 100)


loglikbin <- function(p, x = 10, n = 120) {
    dbinom(x, size = n, prob = p, log=TRUE)
}

ps <- seq(0.01, 0.99, length = 1000)

loglik <- vapply(ps, loglikbin, numeric(1))

df <- data.frame(loglik, ps)
ggplot(df, aes(x = ps, y = loglik)) +
    geom_line()

ps[which.max(loglik)]
