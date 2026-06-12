# Beräknar medelvärden för månaderna
# Skapar en variabel för månader
tidsserie$manad_nummer <- month(tidsserie$datum)

# Beräknar medelvärden
manad_medel <- tidsserie %>%
  group_by(manad_nummer) %>%
  summarise(medel_arbetsloshet = mean(Procent), .groups = "drop")

# Skapar variabel med infon för tabell 
tabell <- manad_medel %>%
  rename(
    Månad = manad_nummer,
    `Medelarbetslöshet (%)` = medel_arbetsloshet)

# Skapar vairabel med infon för graf 
stapeldiagram <- ggplot(
  data = manad_medel,
  mapping = aes(x = factor(manad_nummer), y = medel_arbetsloshet)) +
  geom_col() +
  labs(x = "Månad", y = "Genomsnittlig arbetslöshet (%)")

