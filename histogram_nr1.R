# Plottar ett histogram 

# Värden att markera ut i histogrammet
min <- kommun_data %>%
  slice_min(`Nettoutjämning per invånare (kr)`) %>%
  select(Kommun) %>%
  as.character(kommun_data)
q1 <- quantile(kommun_data$`Nettoutjämning per invånare (kr)`, 0.25)
median <- median(kommun_data$`Nettoutjämning per invånare (kr)`)
q3 <- quantile(kommun_data$`Nettoutjämning per invånare (kr)`, 0.75)

# Skapar själva histogrammet
histogram_n1 <- ggplot(
  data = kommun_data, 
  mapping = aes(x = `Nettoutjämning per invånare (kr)`)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = q1, linetype = "dashed") +
  geom_vline(xintercept = median, linetype = "solid") + 
  geom_vline(xintercept = q3, linetype = "dashed") +
  labs(
    x = "Nettoutjämning per invånare (kr)",
    y = "Antal kommuner",
    title = "Figur 1"
  )
