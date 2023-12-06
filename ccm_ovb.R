library(rEDM)
library(dplyr)
library(tidyr)
library(ggplot2)

len <- 1000
dat <- tibble(time = 1:len,
              x1 = diffinv(rnorm(len-1)), # time series
              x2 = rnorm(len, -1*lag(x1)*0.5, 1),
              y = 0.5) |>
  mutate(y = rnorm(len, lag(y) + x1 + x2 
                   # +lag(x1,2) - lag(x2,2) +
                   #   lag(x1,3) + lag(x2,3)
                   ,
                   2)) |>
  select(-x1) |>
  as.data.frame()

# some plots

dat |>
  pivot_longer(x1:y) |>
  ggplot(aes(x=time, y = value, color = name)) +
  geom_line() +
  facet_wrap(vars(name))

ggplot(dat, aes(x = x2, y = y, color = time)) + geom_line()

# test

simplex_out_x <- Simplex(dataFrame = dat, 
                         lib = "1 700", 
                         pred = "701 1000", 
                         columns = "x2", 
                         target = "y",
                         E = 3)


ComputeError(simplex_out_x$Observations,
             simplex_out_x$Predictions)

# check embedding dimension
rho_e <- EmbedDimension(dataFrame = dat, 
                        lib = "1 900", 
                        pred = "901 1000", 
                        columns = "x2", 
                        target = "y")
