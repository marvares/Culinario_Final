# ==============================================================================
# SCRIPT 00: PREPARACIÓN DE DATOS
# Tesis: Impacto de los factores de la experiencia gastronómica en la toma de
#         decisiones del comensal
# ==============================================================================
# Este script lee los datos del Excel original, los consolida en un solo
# data frame listo para análisis, crea índices agregados por dimensión,
# y exporta el resultado en formato .csv y .RData.
# ==============================================================================

# --- 1. Librerías necesarias -------------------------------------------------
# Instalar si no están disponibles:
# install.packages(c("readxl", "dplyr", "tidyr", "janitor", "skimr"))

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)

# --- 2. Configuración --------------------------------------------------------

# IMPORTANTE: Ajusta esta ruta al lugar donde tengas el archivo Excel
archivo_excel <- "Tab_Tes.xlsx"

# Mapeo de hojas a condiciones experimentales
hojas <- c("semana 1", "semana 2", "semana 3", "semana 4")

condiciones <- c(
  "semana 1" = "base",
  "semana 2" = "servicio_degradado",
  "semana 3" = "producto_degradado",
  "semana 4" = "entorno_degradado"
)

# Nombres estandarizados de las columnas de datos (sin la columna de número)
nombres_items <- c("P1", "P2", "P3",          # Producto (3 ítems)
                   "S1", "S2", "S3",           # Servicio (3 ítems)
                   "E1", "E2", "E3", "E4",     # Entorno (8 ítems)
                   "E5", "E6", "E7", "E8",
                   "I1", "I2", "I3")           # Resultados (3 ítems)

# --- 3. Lectura y consolidación de datos -------------------------------------

datos_lista <- list()

for (hoja in hojas) {

  df_temp <- read_excel(
    path      = archivo_excel,
    sheet     = hoja,
    range     = "B2:S46",
    # Lee desde celda B2 hasta S46:
    # B2 = encabezados (n°/Cant, P1..I3)
    # B3:S46 = 44 registros de datos
    col_names = TRUE
  )

  # Renombrar columnas de forma estandarizada
  # (la primera columna es n°/Cant, luego P1..I3 = 18 columnas total)
  df_temp <- df_temp %>%
    setNames(c("nro_comensal", nombres_items))

  # Agregar identificadores de semana y condición
  df_temp <- df_temp %>%
    mutate(
      semana    = which(hojas == hoja),
      condicion = condiciones[hoja],
      .before   = nro_comensal
    )

  datos_lista[[hoja]] <- df_temp
}

# Unir todo en un solo data frame
datos <- bind_rows(datos_lista)

cat("Dimensiones del data frame consolidado:", nrow(datos), "filas x", ncol(datos), "columnas\n")
# Esperado: 176 filas (44 x 4 semanas) x 20 columnas

# --- 4. Verificaciones básicas de integridad ---------------------------------

cat("\n--- Verificación de integridad ---\n")

# 4a. Conteo por semana
cat("\nRegistros por semana:\n")
print(table(datos$semana))

# 4b. Verificar que no hay valores fuera del rango Likert (1-5) en P, S, E, I1, I2
items_likert <- c("P1","P2","P3","S1","S2","S3",
                   "E1","E2","E3","E4","E5","E6","E7","E8",
                   "I1","I2")

cat("\nRango de valores en ítems Likert (esperado: 1 a 5):\n")
rangos <- datos %>%
  summarise(across(all_of(items_likert),
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE))))
# Mostrar de forma legible
for (item in items_likert) {
  mn <- rangos[[paste0(item, "_min")]]
  mx <- rangos[[paste0(item, "_max")]]
  flag <- ifelse(mn < 1 | mx > 5, " ** FUERA DE RANGO **", "")
  cat(sprintf("  %s: [%g, %g]%s\n", item, mn, mx, flag))
}

# 4c. Verificar que I3 es binaria (1 = Sí recomendaría, 2 = No recomendaría)
cat("\nDistribución de I3 (variable binaria: 1=Sí, 2=No):\n")
print(table(datos$semana, datos$I3, dnn = c("Semana", "I3")))

# 4d. Verificar valores faltantes (NA)
n_na <- sum(is.na(datos %>% select(all_of(nombres_items))))
cat(sprintf("\nValores faltantes en ítems: %d\n", n_na))

# --- 5. Recodificación de I3 -------------------------------------------------
# I3 originalmente: 1 = Sí recomendaría, 2 = No recomendaría
# Creamos una versión más intuitiva para análisis:
#   - I3_bin: 1 = Sí recomendaría, 0 = No recomendaría (para proporciones y
#     regresión logística)
#   - I3_factor: factor con etiquetas legibles

datos <- datos %>%
  mutate(
    I3_bin    = ifelse(I3 == 1, 1, 0),
    I3_factor = factor(I3_bin, levels = c(0, 1),
                       labels = c("No recomendaría", "Sí recomendaría"))
  )

cat("\nVerificación de recodificación de I3:\n")
print(table(datos$semana, datos$I3_factor, dnn = c("Semana", "Recomendaría")))

# --- 6. Crear índices agregados por dimensión --------------------------------
# Promedio de los ítems de cada dimensión para obtener un puntaje compuesto

datos <- datos %>%
  mutate(
    idx_producto = rowMeans(select(., P1, P2, P3)),
    idx_servicio = rowMeans(select(., S1, S2, S3)),
    idx_entorno  = rowMeans(select(., E1, E2, E3, E4, E5, E6, E7, E8)),
    idx_satisfaccion = I1,       # ítem único
    idx_revisitar    = I2        # ítem único
  )

# --- 7. Convertir variables categóricas a factores ---------------------------

datos <- datos %>%
  mutate(
    semana    = factor(semana, levels = 1:4,
                       labels = paste("Semana", 1:4)),
    condicion = factor(condicion,
                       levels = c("base",
                                  "servicio_degradado",
                                  "producto_degradado",
                                  "entorno_degradado"),
                       labels = c("Base (todo bien)",
                                  "Servicio degradado",
                                  "Producto degradado",
                                  "Entorno degradado"))
  )

# --- 8. Tabla descriptiva resumen --------------------------------------------

cat("\n--- Estadísticos descriptivos por condición ---\n\n")

resumen <- datos %>%
  group_by(condicion) %>%
  summarise(
    n = n(),
    Producto_M   = round(mean(idx_producto), 2),
    Producto_DE  = round(sd(idx_producto), 2),
    Servicio_M   = round(mean(idx_servicio), 2),
    Servicio_DE  = round(sd(idx_servicio), 2),
    Entorno_M    = round(mean(idx_entorno), 2),
    Entorno_DE   = round(sd(idx_entorno), 2),
    Satisf_M     = round(mean(idx_satisfaccion), 2),
    Satisf_DE    = round(sd(idx_satisfaccion), 2),
    Revisitar_M  = round(mean(idx_revisitar), 2),
    Revisitar_DE = round(sd(idx_revisitar), 2),
    Recomendar_prop = round(mean(I3_bin), 2),
    .groups = "drop"
  )

print(as.data.frame(resumen))

# --- 9. Verificación de manipulación experimental ----------------------------
# Comprobar que la dimensión degradada efectivamente bajó, y las demás se
# mantuvieron relativamente estables respecto a la línea base.

cat("\n--- Verificación de manipulación experimental ---\n")
cat("(Comparar cada fila con la Base para ver qué dimensión cambió)\n\n")

manip <- datos %>%
  group_by(condicion) %>%
  summarise(
    Producto = round(mean(idx_producto), 2),
    Servicio = round(mean(idx_servicio), 2),
    Entorno  = round(mean(idx_entorno), 2),
    .groups  = "drop"
  )

print(as.data.frame(manip))

cat("\nNota: En cada condición degradada, la dimensión manipulada debería\n")
cat("mostrar un valor notablemente inferior al de la línea base.\n")

# --- 10. Exportar datos preparados -------------------------------------------

# CSV (compatible con cualquier software)
write.csv(datos, "datos_tesis_preparados.csv", row.names = FALSE)

# RData (preserva factores y tipos de datos para R)
save(datos, file = "datos_tesis_preparados.RData")

cat("\n--- Archivos exportados ---\n")
cat("  - datos_tesis_preparados.csv\n")
cat("  - datos_tesis_preparados.RData\n")

# --- 11. Estructura final del data frame -------------------------------------

cat("\n--- Estructura final ---\n")
str(datos)

cat("\n--- Primeras filas ---\n")
print(head(datos, 6))

cat("\n=====================================================\n")
cat(" Preparación completada. El data frame 'datos' está\n")
cat(" listo para las fases de análisis subsiguientes.\n")
cat("=====================================================\n")

# ==============================================================================
# DICCIONARIO DE VARIABLES DEL DATA FRAME FINAL
# ==============================================================================
#
# semana          Factor  Semana del evento (Semana 1 a 4)
# condicion       Factor  Condición experimental:
#                           - Base (todo bien)
#                           - Servicio degradado
#                           - Producto degradado
#                           - Entorno degradado
# nro_comensal    Num     Número del comensal dentro de cada semana (1-44)
#                           NOTA: NO identifica a la misma persona entre semanas
#
# --- Ítems individuales (escala Likert 1-5) ---
# P1              Num     Sabor de la comida
# P2              Num     Presentación del plato
# P3              Num     Frescura de la comida
# S1              Num     Atención de los camareros
# S2              Num     Confianza que inspiran los camareros
# S3              Num     Apariencia profesional de los camareros
# E1              Num     Decoración acogedora
# E2              Num     Colores del ambiente
# E3              Num     Calidad del mobiliario
# E4              Num     Calidad del menaje
# E5              Num     Calidad de manteles y servilletas
# E6              Num     Espacio y distribución del mobiliario
# E7              Num     Disposición de asientos
# E8              Num     Música adecuada
#
# --- Variables dependientes ---
# I1              Num     Nivel de satisfacción general (Likert 1-5)
# I2              Num     Intención de revisitar (Likert 1-5)
# I3              Num     Recomendaría: original (1=Sí, 2=No)
# I3_bin          Num     Recomendaría: recodificado (1=Sí, 0=No)
# I3_factor       Factor  Recomendaría: con etiquetas
#
# --- Índices agregados ---
# idx_producto      Num   Promedio de P1, P2, P3
# idx_servicio      Num   Promedio de S1, S2, S3
# idx_entorno       Num   Promedio de E1..E8
# idx_satisfaccion  Num   = I1 (ítem único)
# idx_revisitar     Num   = I2 (ítem único)
# ==============================================================================
