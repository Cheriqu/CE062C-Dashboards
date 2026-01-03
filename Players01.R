df <- read_csv("players.csv")

df_clean <- df %>%
  janitor::clean_names()

df_clean[df_clean == ""] <- NA

df_clean$int_value <- as.integer(df_clean$int_value)

df_clean <- df_clean[, -c(ncol(df_clean)-1, ncol(df_clean))]

df_clean <- df_clean %>%
  mutate(dt_date_of_birth = as.Date(dt_date_of_birth, format = "%d/%m/%Y"))

df_clean <- df_clean[, -c(which(names(df_clean) %in% c("str_work_rate", "str_body_type")))]

formatar_colunas <- function(names) {
  names <- str_replace_all(names, "^int_", "")
  names <- str_replace_all(names, "^str_", "")
  names <- str_replace_all(names, "^dt_", "")
  names <- str_to_title(names)
  return(names)
}

# Aplicar a função aos nomes das colunas
names(df_clean) <- formatar_colunas(names(df_clean))

write_csv(df_clean, 'players_dataset.csv', col_names = TRUE, na = "NA")
