load("~/Downloads/wrangled_data_with_var_for_plots.rda")

library(tidyverse)

nyts_data |> filter(year == 2015) -> nyts_2015

nyts_2015 |>
    summarize(ms = mean(ecig_current & Sex == "male", na.rm=TRUE),
              fs = mean(ecig_current & Sex == "female", na.rm=TRUE),
              mn = mean(!ecig_current & Sex == "male", na.rm=TRUE),
              fn = mean(!ecig_current & Sex == "female", na.rm=TRUE),
              odds_ratio1 = (ms/fs)/(mn/fn),
              odds_ratio2 = (ms/mn)/(fs/fn)
    ) |> as.data.frame()

fit <- glm(ecig_current ~ Sex, family=binomial, data=nyts_2015)
summary(fit)

fit <- glm(ecig_current ~ Sex + factor(year) + non_ecig_ever,
           family=binomial, data = nyts_data)
summary(fit)

