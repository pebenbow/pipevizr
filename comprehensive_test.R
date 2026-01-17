library(nycflights13)

flights2 <- flights |> 
  select(origin, dest, tailnum, carrier)

airlines |>
  filter(name == "American Airlines Inc.") |>
  left_join(flights2, by = join_by(carrier)) -> results


# TODO: handle when airlines first introduced inside left_join
# TODO: handle when left_join has x&y arguments
# TODO: handle pivot_longer and pivot_wider
# TODO:JOIN nodes: put join_by criteria on subsequent lines
# TODO:SELECT nodes: list columns on one line with text wrapping