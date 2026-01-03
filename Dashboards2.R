required_packages <- c("tidyverse", "ggplot2", "gridExtra", "forcats", "maps", "lubridate",
                       "corrplot", "shiny", "shinipsum", "tidyr", "purrr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE) 
  }
}

df <- read_csv("players.csv")
df_clean <- df %>% janitor::clean_names()
df_clean[df_clean == ""] <- NA
df_clean$int_value <- as.integer(df_clean$int_value)
df_clean <- df_clean[, -c(ncol(df_clean)-1, ncol(df_clean))]
df_clean <- df_clean %>%
  mutate(dt_date_of_birth = as.Date(dt_date_of_birth, format = "%d/%m/%Y"))
df <- df %>%
  mutate(Age = as.integer(interval(start = dt_date_of_birth, end = today()) / years(1)))
df_clean <- df_clean[, -c(which(names(df_clean) %in% c("str_work_rate", "str_body_type")))]
formatar_colunas <- function(names) {
  names <- str_replace_all(names, "^int_", "")
  names <- str_replace_all(names, "^str_", "")
  names <- str_replace_all(names, "^dt_", "")
  names <- str_to_title(names)
  return(names)
}
names(df_clean) <- formatar_colunas(names(df_clean))
write_csv(df_clean, 'players_dataset.csv', col_names = TRUE, na = "NA")
df <- subset(df, select = -dt_date_of_birth)
df <- subset(df, select = -str_trait)
df <- subset(df, select = -int_player_id)

# UI do Shiny
ui <- fluidPage(
  titlePanel("Dashboard Jogadores FIFA 21"),
  sidebarLayout(
    sidebarPanel(
      selectInput("position", "Posição:", choices = sort(unique(df$str_best_position)), selected = NULL),
      sliderInput("overall_rating", "Avaliação Geral:", min = min(df$int_overall_rating, na.rm = TRUE), max = max(df$int_overall_rating, na.rm = TRUE), value = c(min(df$int_overall_rating, na.rm = TRUE), max(df$int_overall_rating, na.rm = TRUE))),
      sliderInput("potential_rating", "Avaliação Potencial:", min = min(df$int_potential_rating, na.rm = TRUE), max = max(df$int_potential_rating, na.rm = TRUE), value = c(min(df$int_potential_rating, na.rm = TRUE), max(df$int_potential_rating, na.rm = TRUE))),
      sliderInput("best_overall_rating", "Melhor Avaliação Geral:", min = min(df$int_best_overall_rating, na.rm = TRUE), max = max(df$int_best_overall_rating, na.rm = TRUE), value = c(min(df$int_best_overall_rating, na.rm = TRUE), max(df$int_best_overall_rating, na.rm = TRUE))),
      sliderInput("value", "Valor de Mercado (em milhões):", min = min(df$int_value, na.rm = TRUE), max = max(df$int_value, na.rm = TRUE), value = c(min(df$int_value, na.rm = TRUE), max(df$int_value, na.rm = TRUE))),
      sliderInput("wage", "Salário (em milhares):", min = min(df$int_wage, na.rm = TRUE), max = max(df$int_wage, na.rm = TRUE), value = c(min(df$int_wage, na.rm = TRUE), max(df$int_wage, na.rm = TRUE))),
      sliderInput("age", "Idade:", min = min(df$Age, na.rm = TRUE), max = max(df$Age, na.rm = TRUE), value = c(min(df$Age, na.rm = TRUE), max(df$Age, na.rm = TRUE))),
      #selectInput("nationality", "Nacionalidade:", choices = unique(df$str_nationality), selected = NULL, multiple = FALSE),
      selectInput("preferred_foot", "Pé Preferido:", choices = c("Direito", "Esquerdo"), selected = NULL, multiple = FALSE),
      sliderInput("weak_foot", "Habilidade com Pé Fraco:", min = 1, max = 5, value = c(1, 5), step = 1),
      sliderInput("skill_moves", "Habilidades:", min = 1, max = 5, value = c(1, 5), step = 1)
    ),
    mainPanel(
      tableOutput("filtered_table")
    )
  )
)

# Server do Shiny
server <- function(input, output) {
  output$filtered_table <- renderTable({
    filtered_df <- df %>%
      filter(
        #str_nationality == input$nationality,
        str_best_position == input$position,
        int_overall_rating >= input$overall_rating[1] & int_overall_rating <= input$overall_rating[2],
        int_potential_rating >= input$potential_rating[1] & int_potential_rating <= input$potential_rating[2],
        int_best_overall_rating >= input$best_overall_rating[1] & int_best_overall_rating <= input$best_overall_rating[2],
        int_value >= input$value[1] & int_value <= input$value[2],
        int_wage >= input$wage[1] & int_wage <= input$wage[2],
        Age >= input$age[1] & Age <= input$age[2],
        if(is.null(input$preferred_foot)) TRUE else str_preferred_foot == ifelse(input$preferred_foot == "Direito", "Right", "Left"),
        int_weak_foot >= input$weak_foot[1] & int_weak_foot <= input$weak_foot[2],
        int_skill_moves >= input$skill_moves[1] & int_skill_moves <= input$skill_moves[2]
      )
    filtered_df
  })
}

# Executar o aplicativo
shinyApp(ui = ui, server = server)
