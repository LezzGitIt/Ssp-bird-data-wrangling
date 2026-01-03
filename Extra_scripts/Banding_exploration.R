### Investigate auxiliary data sets##

# Load libraries & data -------------------------------------------------------
library(naniar)

gaica <- read.csv("/Users/aaronskinner/Library/CloudStorage/OneDrive-UBC/Grad_School/PhD/TNC_Gaica_Unillanos_data/Aves_GAICA_2013-2017_CONSOLIDADA_11.25.22.csv")
names(gaica)

# Banding -----------------------------------------------------------------
# Separate banding data
banding <- dfPCs$`AVES_GAICA_MONITOREO-BD_2013-2017_V6.xlsx` %>% filter(Protocolo_muestreo == "Captura con redes de niebla")
banding <- banding %>%
  select(1:14, contains(c("ID_punto_conteo", "Protocolo_muestreo", "Mes", "Dia", "Hora", "Fecha", "Orden", "Especie", "Count", "Habitat", "Sistema", "Distancia_observacion")), "Ano", "Sexo", "Anillo", "CT", "CE", "LN", "AN", "AC", "LH", "T", "CA", "C", "Peso", "Observacion_campo_captura") %>%
  replace_with_na(list(CA = 0, C = 0, Peso = 0, T = 0))
# CA = Cuerda alar, C = Cola, T = Tarso
lapply(banding[, c("CA", "C", "Peso", "T")], function(x) {
  table(is.na(x))
})

banding$Habitat_AJUSTADO_2022

banding %>%
  distinct(ID_punto_conteo, Latitud, Longitud_decimal) %>%
  count(ID_punto_conteo, sort = T)
banding %>%
  count(Observacion_campo_captura, Sexo, Departamento, sort = T) %>%
  filter(str_detect(Observacion_campo_captura, regex("Juvenil|adult", ignore_case = TRUE)) & !str_detect(Sexo, "[Indeterminado]")) %>%
  summarize(sum = sum(n))


# Lets break things down by the numbers
banding %>% count(Department0)
banding %>%
  group_by(ID_punto_conteo) %>%
  count(Habitat_Ajustado) %>%
  ungroup() %>%
  count(Habitat_Ajustado)
banding %>%
  count(Habitat_AJUSTADO_2022, Especie, sort = T) %>%
  arrange(Habitat_AJUSTADO_2022)
banding %>% count(Sexo)

# In Quindio, top species are Bronzy Inca, Three-striped Warbler, speckled hummingbird, Streak necked flycatcher, slate-throated redstart, White-booted racket-tail, Mexican violetear, Long-tailed sylph, chestnut capped brushfinch, orange-billed nightingale thrush, Rufous-breasted flycatcher, Andean motmot
banding %>%
  filter(Department == "Quindío") %>%
  count(scientificName, sort = T)

banding %>%
  filter(Orden == "Passeriformes" & Peso > 70) %>%
  select(Especie, CA, C, Peso, T) %>%
  arrange(desc(Peso))

# Run mean and SDs of vars and identify outliers by species
banding %>%
  group_by(Especie) %>%
  mutate(
    CA_zscore = scale(CA),
    C_zscore = scale(C),
    Peso_zscore = scale(Peso),
    T_zscore = scale(T)
  ) %>%
  filter(Especie == "Vireo flavoviridis") %>%
  # ungroup() %>%
  filter(abs(CA_zscore) >= 3 |
    abs(C_zscore) >= 3 |
    abs(Peso_zscore) >= 3 |
    abs(T_zscore) >= 3) %>%
  select(CA, C, Peso, T, matches("zscore")) %>%
  View()

