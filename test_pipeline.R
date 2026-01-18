library(nycflights13)
library(dplyr)

flights2 <- flights |> 
  select(origin, dest, tailnum, carrier)

airlines |>
  filter(name == 'American Airlines Inc.') |>
  left_join(flights2, by = join_by(carrier)) -> results
