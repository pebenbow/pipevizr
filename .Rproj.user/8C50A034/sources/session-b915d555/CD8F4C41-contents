df1 <- mtcars %>% select(mpg, cyl)
df2 <- mtcars %>% select(mpg, hp)

joined <- df1 %>%
  left_join(df2, by = 'mpg') %>%
  filter(!is.na(hp))