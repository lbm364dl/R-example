library("dplyr")

starwars |>
  select(sex) |>
  count(sex) |>
  print(n = 30)

starwars |>
  filter(is.na(sex)) |>
  print()

starwars |>
  filter(sex == "none") |>
  print()

starwars |>
  mutate()
