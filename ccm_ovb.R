library(rEDM)
library(dplyr)
library(tidyr)
library(ggplot2)
data(paramecium_didinium)

#dat <- paramecium_didinium
len <- 1000
dat <- tibble(time = 1:len,
              x1 = runif(len),
              x2 = rnorm(len, -1*lag(x1)*0.5, 1),
              y = 0.5) |>
  mutate(y = rnorm(len, lag(y) + x1 + x2 
                   # +lag(x1,2) - lag(x2,2) +
                   #   lag(x1,3) + lag(x2,3)
                   ,
                   0)) |>
  select(-x1) |>
  as.data.frame()

dat |>
  pivot_longer(x2:y) |>
  ggplot(aes(x=time, y = value, color = name)) +
  geom_line() +
  facet_wrap(vars(name))

ggplot(dat, aes(x = x2, y = y, color = time)) + geom_line()

#look at embedding dimensions

n <- NROW(dat)
concat_xy <- c(dat$x2, dat$y)
lib_x <- c(1,length(concat_xy)/2)
lib_y <- c(length(concat_xy)/2+1, length(concat_xy))

simplex_out_x <- simplex(concat_xy, lib = lib_x, pred = lib_x, silent = TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]
copred_x_to_y <- simplex(concat_xy, lib = lib_x, pred = lib_y, E = best_E_x)

simplex_out_y <- simplex(concat_xy, lib = lib_y, pred = lib_y, silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

copred_y_to_x <- simplex(concat_xy, lib = lib_y, pred = lib_x, E = best_E_y)


groups <- c("prediction of x (from x)", 
            "coprediction of x (from y)", 
            "prediction of y (from y)", 
            "coprediction of y (from x)")
to_plot <- data.frame(label = factor(groups, levels = groups), 
                      rbind(simplex_out_x[which.max(simplex_out_x$rho), ], 
                            copred_y_to_x, 
                            simplex_out_y[which.max(simplex_out_y$rho), ], 
                            copred_x_to_y)
)


ggplot(to_plot, aes(x = label, y = rho)) + 
  geom_col() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#### lags
vars <- names(dat)[2:3] # c("paramecium", "didinium")

# generate all combinations of lib_column, target_column, tp
params <- expand.grid(lib_column = vars, 
                      target_column = vars, 
                      tp = -10:10)

# throw out cases where lib == target
params <- params[params$lib_column != params$target_column, ]

# E = 3 is optimal or very close to optimal for both vars
# In other circumstances, we should use the best univariate E for each lib_column
E <- 5

output <- do.call(rbind, lapply(seq_len(NROW(params)), function(i) {
  ccm(dat, 
      E = E, 
      lib_sizes = NROW(dat), 
      random_libs = FALSE, 
      lib_column = params$lib_column[i], 
      target_column = params$target_column[i], 
      tp = params$tp[i], 
      silent = TRUE)
}))


# output
output$direction <- paste(output$lib_column, "xmap to", output$target_column)
ggplot(output, aes(x = tp, y = rho, color = direction)) + 
  geom_line() + theme_bw()

