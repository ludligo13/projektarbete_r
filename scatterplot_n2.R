# Plottar en scatterplot 

scatterplot_n2 <- ggplot(
  data = kommun_data, 
  mapping = aes(
    x = `Andel invånare över 80 år`, 
    y = `Nettoutjämning per invånare (kr)`)) +
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(
    x     = "Andel invånare över 80 år",
    y     = "Nettoutjämning per invånare (kr)",
    title = "Figur 3"
  )
