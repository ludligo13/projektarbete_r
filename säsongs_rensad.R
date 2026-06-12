# Skapar Z genom att ta bort månadsmedelvärdena
# Passar på att döpa om kolumnerna

tidsserie <- tidsserie %>%
  left_join(manad_medel, by = "manad_nummer") %>%
  rename(
    Månad = månad,
    Datum = datum,
    `Månad nummer` = manad_nummer,
    År = ar,
    Medelarbetslöshet = medel_arbetsloshet
  )

total_medel <- mean(tidsserie$Procent)

tidsserie$Z <- tidsserie$Procent - tidsserie$Medelarbetslöshet + total_medel
