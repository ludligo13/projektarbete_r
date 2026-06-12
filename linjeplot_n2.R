linjeplot_n2 <- ggplot(tidsserie) +
  geom_line(
    mapping = aes(x = Datum, y = Procent, colour = "Y")) +
  geom_line(
    mapping = aes(x = Datum, y = Z, colour = "Z")) +
  labs(x = "År", y = "Arbetslöshet (%)", colour = "Serie", title = "Figur 8")
