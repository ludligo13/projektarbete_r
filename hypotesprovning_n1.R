# Beräknar korrelation för variablerna i Scatterplot 1 och återger i en tabell

# Gör ett korrelationstest för variablerna 
cor_test <- cor.test(
  x = kommun_data$`Medianinkomst (tkr)`,
  y = kommun_data$`Nettoutjämning per invånare (kr)`
)

# Tilldelar värdena variabelnamn så de kan tillkallas i RMD-filen
p_varde <- round(cor_test$p.value[1], 3)
testvariabel <- round(as.numeric(cor_test$statistic), 3)
korrelation <- round(as.numeric(cor_test$estimate), 3)
ki_nedre <- round(cor_test$conf.int[1], 3)
ki_ovre <- round(cor_test$conf.int[2], 3)
alfa <- 1-attr(cor_test$conf.int,"conf.level")

# Gör en tabell med infon
tabell_namn <- c("Korrelation", "Testvariabel", "p-värde", "Nedre KI", "Övre KI")
tabell_varden <- c(korrelation, testvariabel, p_varde, ki_nedre, ki_ovre)
tabell <- data.frame("Beskrivning" = tabell_namn, "Värde" = tabell_varden)

# Rensar Glo Env på variabler som ej kommer användas mer
rm(tabell_namn, tabell_varden, cor_test)


