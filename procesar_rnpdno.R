suppressMessages({
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(tidyr)
})

# ----------------------------------------
# Diagnóstico
# ----------------------------------------
cat("Working directory:\n")
print(getwd())

ruta <- "salida/circunstancias"

archivos <- list.files(
  path = ruta,
  pattern = "\\.json$",
  full.names = TRUE
)

cat("Archivos JSON encontrados:\n")
print(archivos)

if (length(archivos) == 0) {
  stop("❌ No se encontraron archivos JSON en salida/circunstancias")
}

# ----------------------------------------
# Lectura y aplanado
# ----------------------------------------

datos_raw <- map(archivos, ~ fromJSON(.x, simplifyVector = FALSE))

df <- map2_dfr(
  datos_raw,
  archivos,
  function(json_data, archivo) {

    entidad_id <- tools::file_path_sans_ext(basename(archivo))


    imap_dfr(
      json_data$por_edad,
      function(sexo_lista, sexo_nombre) {

        tibble(
          entidad = entidad_id,
          edad = suppressWarnings(as.integer(names(sexo_lista))),
          total_personas = as.integer(unlist(sexo_lista)),
          sexo = sexo_nombre
        )
      }
    )
  }
) %>%
  filter(!is.na(edad))

# ----------------------------------------
# Grupos de edad (Data Cívica)
# ----------------------------------------

df <- df %>%
  mutate(
    grupo_edad = case_when(
      edad >= 0  & edad <= 9  ~ "Infancias (0-9)",
      edad >= 10 & edad <= 19 ~ "Adolescentes (10-19)",
      edad >= 20 & edad <= 35 ~ "Jóvenes (20-35)",
      edad >= 36 & edad <= 59 ~ "Adultos (36-59)",
      edad >= 60              ~ "Adultos mayores (60+)",
      TRUE ~ NA_character_
    )
  )

# ----------------------------------------
# Agregación final
# ----------------------------------------

df_final <- df %>%
  filter(!is.na(grupo_edad)) %>%
  group_by(entidad, sexo, grupo_edad) %>%
  summarise(
    total_personas = sum(total_personas, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------------------
# Guardar CSV FINAL
# ----------------------------------------

write.csv(
  df_final,
  "desapariciones_2024_sexo_grupo_edad_entidad.csv",
  row.names = FALSE
)

cat("✅ CSV final generado correctamente\n")
 
