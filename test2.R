pipe_vizr_file("comprehensive_test.R", direction = "TB")

devtools::document()
devtools::load_all()

library(dplyr)
library(pipevizr)

test_branches <- "test_branches.R"
writeLines(c(
  "library(dplyr)",
  "",
  "# Three pipelines starting from mtcars",
  "result1 <- mtcars %>%",
  "  filter(cyl > 4) %>%",
  "  select(mpg, cyl)",
  "",
  "result2 <- mtcars %>%",
  "  filter(mpg > 20) %>%",
  "  mutate(efficiency = mpg / hp)",
  "",
  "result3 <- mtcars %>%",
  "  arrange(desc(hp)) %>%",
  "  select(hp, mpg)",
  "",
  "# One pipeline from iris (should be connected)",
  "iris_summary <- iris %>%",
  "  filter(Species == 'setosa') %>%",
  "  summarize(mean_length = mean(Sepal.Length))"
), test_branches)

viz <- pipe_vizr_file(test_branches, combine = TRUE)
print(viz)

unlink(test_branches)