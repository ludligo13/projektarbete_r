# Scatterplot för Skattesats jämfört med Skatteintäkter per invånare
#  färgad efter den kategoriska variabeln Kommunstorlek
scatterplot_n3 <- ggplot(
  data = kommun_data,
  mapping = aes(
    x = `Skattesats (%)`,
    y = `Skatteintäkter per invånare (kr)`,
    color = kommunstorlek)) +
  geom_point() +
  stat_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  labs(title = "Figur 4")
