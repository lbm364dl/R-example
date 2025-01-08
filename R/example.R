dplyr::starwars |>
  dplyr::select(sex) |>
  dplyr::count(sex) |>
  print(n = 30)

dplyr::starwars |>
  dplyr::filter(is.na(sex)) |>
  print()

dplyr::starwars |>
  dplyr::filter(sex == "none") |>
  print()

dplyr::starwars |>
  dplyr::mutate()
