# Läser in två paket som används genom filen
library(pxweb)
library(dplyr)

#### Funktion för att ladda ner variabler ####
ladda_ner_kommun_data <- function(
    contentsCode, 
    kontopost = NULL, 
    civilstand = NULL,
    spridning = NULL,
    inkomstTyp = NULL,
    kon = NULL,
    alder = NULL,
    url) {
  
  # Skapar en lista med alla inputs alla variabler kommer behöva i sina querys
  pxweb_query_list <- list(
    "Region"       = c("0114","0115","0117","0120","0123","0125","0126","0127","0128","0136","0138","0139","0140","0160","0162","0163","0180","0181","0182","0183","0184","0186","0187","0188","0191","0192","0305","0319","0330","0331","0360","0380","0381","0382","0428","0461","0480","0481","0482","0483","0484","0486","0488","0509","0512","0513","0560","0561","0562","0563","0580","0581","0582","0583","0584","0586","0604","0617","0642","0643","0662","0665","0680","0682","0683","0684","0685","0686","0687","0760","0761","0763","0764","0765","0767","0780","0781","0821","0834","0840","0860","0861","0862","0880","0881","0882","0883","0884","0885","0980","1060","1080","1081","1082","1083","1214","1230","1231","1233","1256","1257","1260","1261","1262","1263","1264","1265","1266","1267","1270","1272","1273","1275","1276","1277","1278","1280","1281","1282","1283","1284","1285","1286","1287","1290","1291","1292","1293","1315","1380","1381","1382","1383","1384","1401","1402","1407","1415","1419","1421","1427","1430","1435","1438","1439","1440","1441","1442","1443","1444","1445","1446","1447","1452","1460","1461","1462","1463","1465","1466","1470","1471","1472","1473","1480","1481","1482","1484","1485","1486","1487","1488","1489","1490","1491","1492","1493","1494","1495","1496","1497","1498","1499","1715","1730","1737","1760","1761","1762","1763","1764","1765","1766","1780","1781","1782","1783","1784","1785","1814","1860","1861","1862","1863","1864","1880","1881","1882","1883","1884","1885","1904","1907","1960","1961","1962","1980","1981","1982","1983","1984","2021","2023","2026","2029","2031","2034","2039","2061","2062","2080","2081","2082","2083","2084","2085","2101","2104","2121","2132","2161","2180","2181","2182","2183","2184","2260","2262","2280","2281","2282","2283","2284","2303","2305","2309","2313","2321","2326","2361","2380","2401","2403","2404","2409","2417","2418","2421","2422","2425","2460","2462","2463","2480","2481","2482","2505","2506","2510","2513","2514","2518","2521","2523","2560","2580","2581","2582","2583","2584"),
    "Tid"          = "2024",
    "ContentsCode" = contentsCode,
    "Kontopost"    = kontopost,
    "Civilstand"   = civilstand,
    "Spridning"    = spridning,
    "InkomstTyp"   = inkomstTyp,
    "Kon"          = kon, 
    "Alder"        = alder)
     
  # Tar bort alla NULL-element
  pxweb_query_list <- pxweb_query_list[
    !sapply(X = pxweb_query_list, FUN = is.null)
    ]
       
  # Gör ett api-anrop genom url:en för query-infon och sparar som en data.frame
  px_data_frame <- as.data.frame(pxweb_get(
    url = url,
    query = pxweb_query_list))
}


# Variabel 1 & 2 --------------------------------------------------------------
befolkning <- ladda_ner_kommun_data(
  contentsCode = "BE0101N1", 
  civilstand = c("OG","G", "SK", "ÄNKL"),
  kon = c("1", "2"),
  alder = c("80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100+","tot"),
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy") 

befolkning <- befolkning %>%
  group_by(region, ålder) %>%
  summarise(Folkmängd = sum(Folkmängd), .groups = "drop")


# Variabel 1: Totalt antal invånare i kommunen --------------------------------
folkmangd <- befolkning %>%
  filter(ålder == "totalt ålder") %>%
  group_by(region) %>%
  summarise(folkmangd = sum(Folkmängd)) %>%
  arrange(region) %>%
  rename("Totalt antal invånare" = `folkmangd`) %>%
  as.data.frame()


# Variabel 2: Andel invånare i kommunen som är 80+ år -------------------------
andel_over_80 <- befolkning %>%
  filter(ålder %in% c("80 år","81 år","82 år","83 år","84 år","85 år","86 år","87 år","88 år","89 år","90 år","91 år","92 år","93 år","94 år","95 år","96 år","97 år","98 år","99 år","100+ år")) %>%
  group_by(region) %>%
  summarise(andel_over_80 = sum(Folkmängd)) %>%
  arrange(region) %>%
  rename("Andel invånare över 80 år" = `andel_over_80`) 

andel_over_80$`Andel invånare över 80 år` <- 100 * (round(
  andel_over_80$`Andel invånare över 80 år`/folkmangd$`Totalt antal invånare`,
  digits = 4))

andel_over_80 <- as.data.frame(andel_over_80)


# Variabel 3: Kommunalekonomisk utjämning i kr per invånare -------------------
nettoutjamning <- ladda_ner_kommun_data(
  contentsCode = "OE0115B7", 
  url          = "https://api.scb.se/OV0104/v1/doris/sv/ssd/OE/OE0115/OE0115A/KomEkUtj")

nettoutjamning <- nettoutjamning %>%
  select(region, `Kommunalekonomisk utjämning, utfall kr/invånare`) %>%
  arrange(region) %>%
  rename("Nettoutjämning per invånare (kr)" = `Kommunalekonomisk utjämning, utfall kr/invånare`)


# Variabel 4: Skatteintäker per invånare i kr --------------------------------------
skatteintakter <- ladda_ner_kommun_data(
  contentsCode = "000008H7", 
  kontopost    = "010",
  url          = "https://api.scb.se/OV0104/v1/doris/en/ssd/OE/OE0107/OE0107A/SkatteIntKnN")

skatteintakter <- skatteintakter %>%
  select(region, `SEK thousands, current prices`) %>%
  arrange(region) %>%
  rename("Skatteintäkter per invånare (kr)" = `SEK thousands, current prices`)

skatteintakter$`Skatteintäkter per invånare (kr)` <- 1000 * (round(
  skatteintakter$`Skatteintäkter per invånare (kr)`/folkmangd$`Totalt antal invånare`,
  digits = 3))


# Variabel 5: Skattesatser ----------------------------------------------------
skattesats <- ladda_ner_kommun_data(
  contentsCode = "OE0101D2",
  url          = "https://api.scb.se/OV0104/v1/doris/sv/ssd/OE/OE0101/Kommunalskatter2000")

skattesats <- skattesats %>%
  select(region, `Skattesats till kommun`) %>%
  arrange(region) %>%
  rename("Skattesats (%)" = `Skattesats till kommun`)


# Variabel 6: Medianinkomst ---------------------------------------------------
medianinkomst <- ladda_ner_kommun_data(
  inkomstTyp     = "NettoInk",
  spridning      = "P50",
  kon            = "1+2",
  alder          = "20-65",
  contentsCode   = "000006OL",
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110A/SamNetFrakt")

medianinkomst <- medianinkomst %>%
  select(region, `Gränsvärde, tkr`) %>%
  arrange(region) %>%
  rename("Medianinkomst (tkr)" = `Gränsvärde, tkr`)


# Sammanställer variablerna i gemensam data.frame -----------------------------
kommun_data <- medianinkomst %>%
  left_join(skatteintakter, by = "region") %>%
  left_join(skattesats, by = "region") %>% 
  left_join(nettoutjamning, by = "region") %>%
  left_join(andel_over_80, by = "region") %>%
  left_join(folkmangd, by = "region") %>%
  rename("Kommun" = region)

# Variabel för tidsserieanalysen ----------------------------------------------
pxweb_query_list <- list(
  "Arbetskraftstillh" = c("ALÖS"),
  "Kon"               = c("1+2"),
  "Alder"             = c("tot15-74"),
  "ContentsCode"      = c("AM04011K"),
  "Tid"               = c("2016M05","2016M06","2016M07","2016M08","2016M09","2016M10","2016M11","2016M12","2017M01","2017M02","2017M03","2017M04","2017M05","2017M06","2017M07","2017M08","2017M09","2017M10","2017M11","2017M12","2018M01","2018M02","2018M03","2018M04","2018M05","2018M06","2018M07","2018M08","2018M09","2018M10","2018M11","2018M12","2019M01","2019M02","2019M03","2019M04","2019M05","2019M06","2019M07","2019M08","2019M09","2019M10","2019M11","2019M12","2020M01","2020M02","2020M03","2020M04","2020M05","2020M06","2020M07","2020M08","2020M09","2020M10","2020M11","2020M12","2021M01","2021M02","2021M03","2021M04","2021M05","2021M06","2021M07","2021M08","2021M09","2021M10","2021M11","2021M12","2022M01","2022M02","2022M03","2022M04","2022M05","2022M06","2022M07","2022M08","2022M09","2022M10","2022M11","2022M12","2023M01","2023M02","2023M03","2023M04","2023M05","2023M06","2023M07","2023M08","2023M09","2023M10","2023M11","2023M12","2024M01","2024M02","2024M03","2024M04","2024M05","2024M06","2024M07","2024M08","2024M09","2024M10","2024M11","2024M12","2025M01","2025M02","2025M03","2025M04","2025M05","2025M06","2025M07","2025M08","2025M09","2025M10","2025M11","2025M12","2026M01","2026M02","2026M03","2026M04"))

# Gör ett api-anrop genom url:en för query-infon och sparar som en data.frame
tidsserie <- as.data.frame(pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0401/AM0401L/NAKUArblheltidstudM",
  query = pxweb_query_list))

tidsserie <- tidsserie %>%
  select(månad, Procent)

# Tar bort alla variabler och dfs som inte längre behövs ----------------------
rm(befolkning, folkmangd, andel_over_80, medianinkomst, nettoutjamning,
   skatteintakter, skattesats, pxweb_query_list)

