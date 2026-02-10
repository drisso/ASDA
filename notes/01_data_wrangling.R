1:100 %>%
    mean() %>%
    rnorm(n=2, mean=., sd=2) %>%
    density() %>%
    plot() 

gapminder |>
    filter(continent == "Europe") |>
    filter(year == 1997) |>
    arrange(desc(gdpPercap)) |>
    head(n=5)

gapminder |>
    mutate(totalGdp = pop*gdpPercap) |>
    filter(country == "Japan") |> 
    filter(year == 1962) |>
    as.data.frame()

gapminder |>
    filter(year == 2007) |>
    group_by(continent) |>
    summarize(averageExp = mean(lifeExp))

gapminder |>
    filter(year <= 1969 & year >= 1960) |>
    group_by(continent, year) |>
    summarize(averageExp = mean(lifeExp))

theme_set(theme_minimal())

gapminder |>
    group_by(continent, year) |>
    summarize(avgExp = mean(lifeExp),
              min = min(lifeExp),
              max = max(lifeExp)) |>
    ggplot(aes(x = year, y = avgExp, 
               group = continent,
               color = continent)) +
    geom_line() +
    geom_ribbon(aes(ymin = min, ymax = max, 
                    fill = continent,
                    alpha = 0.2)) +
    facet_wrap(~continent)

gapminder |>
    ggplot(aes(x = year, y = lifeExp,
               group = country,
               color = continent)) +
    geom_line() +
    facet_wrap(~continent)

gapminder |>
    ggplot(aes(y = log(pop))) +
    geom_boxplot(col = "red", fill="blue")

theme_set(theme_minimal())
p <- gapminder |>
    ggplot(aes(x = gdpPercap, y = lifeExp)) +
    geom_point()
p
p + scale_x_log10()

install.packages("ggrepel")
library(ggrepel)
gapminder |>
    filter(year == 1962) |>
    ggplot(aes(x = gdpPercap, y = lifeExp, 
               color = continent)) +
    geom_point(aes(size = pop, alpha = 0.5)) +
    scale_x_log10() +
    ggtitle("Data from 1962") +
    geom_label_repel(aes(label = country)) +
    scale_color_manual(values = continent_colors)

library(plotly)
p <- gapminder |>
    filter(year == 1962) |>
    ggplot(aes(x = gdpPercap, y = lifeExp, 
               color = continent,
               label = country)) +
    geom_point(aes(size = pop)) +
    scale_x_log10() +
    ggtitle("Data from 1962")

ggplotly(p, tooltip = "country")

gapminder |>
    ggplot(aes(x = lifeExp)) +
    geom_histogram(col = "red")

gapminder |>
    ggplot(aes(x = lifeExp)) +
    geom_density()

gapminder |>
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_boxplot() 

gapminder |>
    ggplot(aes(y = continent, x = lifeExp)) +
    geom_boxplot() 

gapminder |>
    ggplot(aes(x = continent, y = lifeExp)) +
    geom_violin(fill = "red")

library(ggridges)
gapminder |>
    ggplot(aes(y = continent, x = lifeExp)) +
    geom_density_ridges()

gapminder |>
    ggplot(aes(x = lifeExp,
               y = after_stat(density))) +
    geom_histogram() +
    geom_density()

gapminder |>
    ggplot(aes(x = factor(year), y = lifeExp)) +
    geom_boxplot() +
    facet_wrap(~continent)

gapminder |>
    ggplot(aes(x = lifeExp, y = continent,
               fill = continent)) +
    geom_density_ridges() +
    scale_fill_manual(values = continent_colors)

gapminder |>
    ggplot(aes(x = gdpPercap, y = lifeExp)) +
    geom_point() +
    geom_density_2d() +
    scale_x_log10()

gapminder |>
    ggplot(aes(x = gdpPercap, y = lifeExp)) +
    geom_hex() +
    scale_x_log10()

gapminder |>
    filter(year == 2007) |>
    ggplot(aes(x = gdpPercap, y = lifeExp,
               size = pop,
               color = country,
               label = country)) +
    geom_point() +
    scale_x_log10() +
    facet_wrap(~continent) +
    theme(legend.position = "none") +
    geom_text_repel(max.overlaps = 20)

github.com/rstudio/EDAWR

data <- read_csv("~/Downloads/cases.csv")
data

data_long <- pivot_longer(data, names_to = "year",
             values_to = "cases", cols = 2:4)

data_long

pivot_wider(data_long, names_from = "year",
            values_from = "cases")

write_csv(data_long, file = "cases_long.csv")

songs <- read_csv("~/Downloads/songs.csv")
artists <- read_csv("~/Downloads/artists.csv")

songs
artists

inner_join(songs, artists, by = "name")
full_join(songs, artists, by = "name")

left_join(songs, artists, by = "name")
right_join(songs, artists, by = "name")


