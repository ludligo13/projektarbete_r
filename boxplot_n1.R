# Skapar en boxplot som är grupperat på den kategoriska variabel Kommunstorlek

boxplot_n1 <- ggplot(
  data = kommun_data,
  mapping = aes(
    x = kommunstorlek,
    y = `Nettoutjämning per invånare (kr)`)) +
  geom_boxplot() + 
  labs(title = "Figur 5")
