devtools::document()
devtools::load_all()

library(dplyr)
library(pipevizr)



test_script <- "test_script.R"

# Visualize all pipelines in one diagram
pipe_vizr_file(test_script, combine = TRUE, direction = "TB")
pipe_vizr_file(test_script, combine = TRUE, direction = "LR")

# Visualize as separate diagrams
pipe_vizr_file(test_script, combine = FALSE)




# Test with fixed width (all nodes same width)
pipe_vizr(
  df <- mtcars %>%
    filter(cyl > 4, mpg > 20) %>%
    arrange(desc(mpg)) %>%
    select(mpg:hp) %>%
    mutate(
      efficiency = mpg / hp,
      power_ratio = hp / cyl
    ) %>%
    summarize(
      avg_eff = mean(efficiency),
      max_eff = max(efficiency)
    ),
  direction = "TB"
)

# Test with wrapping (wrap after 3 nodes)
pipe_vizr(
  mtcars %>%
    filter(cyl > 4) %>%
    mutate(x = mpg / hp) %>%
    filter(mpg > 20) %>%
    mutate(y = hp / cyl) %>%
    select(x, y) %>%
    summarize(sum_x = sum(x)),
  direction = "TB"
)

# Test vertical wrapping
pipe_vizr(
  mtcars %>%
    filter(cyl > 4) %>%
    mutate(x = 1) %>%
    filter(mpg > 20) %>%
    select(mpg, x),
  direction = "TB"
)