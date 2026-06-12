# Skapar en linjeplot 
linjeplot_n3 <- ggplot(
  data = tidsserie,
  mapping = aes(x = Datum, y = Procent)) +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "År", y = "Arbetslöshet (%)", title = "Figur 9")
