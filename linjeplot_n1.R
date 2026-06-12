# Gör en linjeplot 

# Skapar en datum-variabel som ggplot kan förstå
tidsserie$datum <- as.Date(
  paste0(
    substr(tidsserie$månad, 1, 4),
    "-",
    substr(tidsserie$månad, 6, 7),
    "-01"))

# Gör själva linjeplotten och baserar det på år som tidsskala
linjeplot_n1 <- ggplot(
  data = tidsserie,
  mapping = aes(x = datum, y = Procent)) +
  geom_line() +
  labs(x = "År", y = "Arbetslöshet (%)", title = "Figur 6")
