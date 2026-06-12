# Gör grupperade boxplots baserat på år
tidsserie$ar <- year(tidsserie$datum)

boxplot_n2 <- ggplot(
  data = tidsserie,
  mapping = aes(x = factor(ar), y = Procent)) +
  geom_boxplot() +
  labs(x = "År", y = "Arbetslöshet (%)", title = "Figur 8")
