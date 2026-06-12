# Plottar en scatterplot 

scatterplot_n1 <- ggplot(
  data = kommun_data, 
  mapping = aes(
    x = `Medianinkomst (tkr)`, 
    y = `Nettoutjämning per invånare (kr)`)) +
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Medianinkomst (tkr)",
    y = "Nettoutjämning per invånare (kr)",
    title = "Figur 2"
  )
